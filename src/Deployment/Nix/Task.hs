module Deployment.Nix.Task(
    Task(..)
  , executeTask
  , reverseTask
  , dryRunTask
  , liftShell
  -- * Helpers for task developing
  , ShellOptions(..)
  , setShellOptions
  , transShell
  , setMachineName
  -- * Color helpers
  , echonColor
  , echoColor
  , Color(..)
  ) where

import Control.Applicative
import Control.Exception.Base (SomeException)
import Control.Monad
import Control.Monad.Catch (MonadMask, bracket_)
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Deployment.Nix.Config
import Shelly
import System.Console.ANSI
import Transient.Base

import qualified Data.Text.IO as T

-- | Reversable task on remote machine
data Task a where
  -- | Atomic task
  AtomTask :: {
      -- | Optional task name
      taskName    :: Maybe Text
      -- | Check that task is need to be applied. 'True' means need to apply, second
      -- value is used as default result of task if no need to apply and as default
      -- value of task on reverse process. Left value is used for checks that are
      -- always passes, so we don't call IO action and Left value is used for reverse
      -- process.
    , taskCheck   :: Either a (TransIO (Bool, a))
      -- | Apply task
    , taskApply   :: TransIO a
      -- | Revert task side effects (if possible)
    , taskReverse :: TransIO ()
    } -> Task a
  -- | Applicative operation for tasks
  TaskApplicative :: Task (a -> b) -> Task a -> Task b
  -- | Monadic operation for tasks
  TaskMonadic :: Task a -> (a -> Task b) -> Task b

instance Functor Task where
  fmap f t = case t of
    AtomTask{..} -> AtomTask {
        taskName = taskName
      , taskCheck = bimap f (fmap . fmap $ f) taskCheck
      , taskApply = f <$> taskApply
      , taskReverse = taskReverse
      }
    TaskApplicative fa ta -> TaskApplicative (fmap f <$> fa) ta
    TaskMonadic ta fa -> TaskMonadic ta (fmap f <$> fa)

instance Applicative Task where
  pure a = AtomTask {
      taskName = Nothing
    , taskCheck = Left a
    , taskApply = pure a
    , taskReverse = pure ()
    }
  (<*>) = TaskApplicative

instance Monad Task where
  return = pure
  (>>=) = TaskMonadic

-- | Lift named shell action to task
liftShell :: Text -> a -> Sh a -> Task a
liftShell name a0 ma = AtomTask {
    taskName = Just name
  , taskCheck = Left a0
  , taskApply = transShell ma
  , taskReverse = pure ()
  }

-- | Special settings for shell execution
data ShellOptions = ShellOptions {
  shellVerbose :: Bool -- ^ Execute shell in verbose mode ?
}

-- | Set shell options to use when embedding shell in tasks
setShellOptions :: ShellOptions -> TransIO ()
setShellOptions = setData

-- | Execute shell inside task atom body with prefered settings (silence, verbos, trace, etc)
transShell :: Sh a -> TransIO a
transShell ma = do
  ShellOptions{..} <- getSData
  let vb = if shellVerbose then verbosely else silently
  shelly $ vb $ ma

-- | Apply effects only when encounter 'Just'
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

-- | Wrap command with SGR codes
withSGR :: (MonadIO m, MonadMask m) => [SGR] -> m a -> m a
withSGR sgr = bracket_ (liftIO $ setSGR sgr) (liftIO $ setSGR [SetConsoleIntensity NormalIntensity])

-- | Print in bold white color whitout new line
echonColor :: (MonadIO m, MonadMask m) => Color -> Text -> m ()
echonColor c = withSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c] . liftIO . T.putStr

-- | Print in bold white color
echoColor :: (MonadIO m, MonadMask m) => Color -> Text -> m ()
echoColor c = withSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c] . liftIO . T.putStrLn

-- | Type wrapper for storage of current machine name for logging purpose
data CurrentMachine = CurrentMachine Color Text

-- | Type wrapper to store queue of machine colors
newtype MachineColors = MachineColors [Color]

-- | Allowed colors for machine names
machineColors :: [Color]
machineColors = [
    Green
  , Yellow
  , Blue
  , Magenta
  , Cyan
  ]

-- | Return next color for machine name in cycle
getNextMachineColor :: TransIO Color
getNextMachineColor = do
  mres <- getData
  case mres of
    Nothing -> do
      setData $ MachineColors $ cycle machineColors
      getNextMachineColor
    Just (MachineColors colors) -> do
      setData $ MachineColors $ drop 1 colors
      pure $ head colors

-- | Set current machine name for logging
setMachineName :: MachineName -> Task ()
setMachineName nm = AtomTask {
    taskName = Just $ "Setting current machine to " <> nm
  , taskCheck = Left ()
  , taskApply = do
      c <- getNextMachineColor
      setData $ CurrentMachine c nm
  , taskReverse = pure ()
  }

-- | If current machine is set, print without newline prefix for the machine
printMachineName :: TransIO ()
printMachineName = do
  mres <- getData
  whenJust mres $ \(CurrentMachine mcol mname) -> liftIO $ echonColor mcol $ mname <> "> "

-- | Apply task if needed
executeTask :: forall a . Bool -> Task a -> TransIO a
executeTask needForce = go
  where
    go :: Task b -> TransIO b
    go t = case t of
      AtomTask{..} -> do
        let taskPrint col msg = whenJust taskName $ \nm -> do
              printMachineName
              liftIO $ echonColor col $ msg nm
            apply = do
              taskPrint White $ \nm -> "Applying task " <> nm <> "\n"
              onException $ \(e :: SomeException) -> do
                taskPrint White $ \nm -> "Reversing task " <> nm <> "\n"
                taskReverse
              taskApply
        case taskCheck of
          Left _ -> apply
          Right chk -> do
            taskPrint White $ \nm -> "Checking task " <> nm <> "... "
            (checkResult, a) <- chk
            if needForce || checkResult then do
                whenJust taskName $ const $ liftIO $ echoColor Red "need apply"
                apply
              else do
                whenJust taskName $ const $ liftIO $ echoColor Green "ok"
                pure a
      TaskApplicative fa ta -> do
        f <- go fa
        a <- go ta
        pure $ f a
      TaskMonadic ta fa -> do
        a <- go ta
        go $ fa a

-- | Apply reverse action of task
reverseTask :: forall a . Task a -> TransIO ()
reverseTask t = do
  (_, ms) <- go t
  forM_ ms $ \(mn, m) -> do
    whenJust mn $ \name -> liftIO $ echoColor White $ "Reversing " <> name
    m
  where
    go :: Task b -> TransIO (b, [(Maybe Text, TransIO ())])
    go t = case t of
      AtomTask{..} -> case taskCheck of
        Left a -> pure (a, [(taskName, taskReverse)])
        Right chk -> do
          (needApply, a) <- chk
          pure $ if needApply then (a, []) else (a, [(taskName, taskReverse)])
      TaskApplicative fa ta -> do
        (f, freverses) <- go fa
        (a, areverses) <- go ta
        pure (f a, freverses ++ areverses)
      TaskMonadic ta fa -> do
        (a, areverses) <- go ta
        (b, breverses) <- go $ fa a
        pure (b, areverses ++ breverses)

-- | Run check actions of task and return list of names, applied or not
dryRunTask :: forall a . Task a -> TransIO [(Maybe Text, Bool)]
dryRunTask = fmap snd . go
  where
    go :: Task b -> TransIO (b, [(Maybe Text, Bool)])
    go t = case t of
      AtomTask{..} -> case taskCheck of
        Left a -> pure (a, [(taskName, False)])
        Right chk ->do
          (needApply, a) <- chk
          pure (a, [(taskName, not needApply)])
      TaskApplicative fa ta -> do
        (f, freverses) <- go fa
        (a, areverses) <- go ta
        pure (f a, freverses ++ areverses)
      TaskMonadic ta fa -> do
        (a, areverses) <- go ta
        (b, breverses) <- go $ fa a
        pure (b, areverses ++ breverses)
