module Deployment.Nix.Task(
    Task(..)
  , executeTask
  , reverseTask
  , dryRunTask
  ) where

import Control.Exception.Base (SomeException)
import Control.Monad
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Transient.Base
import Shelly

-- | Reversable task on remote machine
data Task a where
  -- | Atomic task
  AtomTask :: {
      -- | Optional task name
      taskName    :: Maybe Text
      -- | Check that task is need to be applied. 'True' means need to apply, second
      -- value is used as default result of task if no need to apply and as default
      -- value of task on reverse process.
    , taskCheck   :: TransIO (Bool, a)
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
      , taskCheck = fmap f <$> taskCheck
      , taskApply = fmap f taskApply
      , taskReverse = taskReverse
      }
    TaskApplicative fa ta -> TaskApplicative (fmap f <$> fa) ta
    TaskMonadic ta fa -> TaskMonadic ta (fmap f <$> fa)

instance Applicative Task where
  pure a = AtomTask {
      taskName = Nothing
    , taskCheck = pure (False, a)
    , taskApply = pure a
    , taskReverse = pure ()
    }
  (<*>) = TaskApplicative

instance Monad Task where
  return = pure
  (>>=) = TaskMonadic

-- | Apply task if needed
executeTask :: forall a . Task a -> TransIO a
executeTask = go
  where
    go :: Task b -> TransIO b
    go t = case t of
      AtomTask{..} -> do
        shelly $ echo_n $ "Checking task " <> fromMaybe "unnamed" taskName <> "... "
        (needApply, a) <- taskCheck
        shelly $ echo $ pack (show needApply)
        if needApply then do
            shelly $ echo $ "Applying task " <> fromMaybe "unnamed" taskName
            onException $ \(e :: SomeException) -> do
              shelly $ echo $ "Reversing task " <> fromMaybe "unnamed" taskName
              taskReverse
            taskApply
          else pure a
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
    shelly . echo $ "Reversing " <> fromMaybe "unnamed" mn
    m
  where
    go :: Task b -> TransIO (b, [(Maybe Text, TransIO ())])
    go t = case t of
      AtomTask{..} -> do
        (needApply, a) <- taskCheck
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
      AtomTask{..} -> do
        (needApply, a) <- taskCheck
        pure (a, [(taskName, not needApply)])
      TaskApplicative fa ta -> do
        (f, freverses) <- go fa
        (a, areverses) <- go ta
        pure (f a, freverses ++ areverses)
      TaskMonadic ta fa -> do
        (a, areverses) <- go ta
        (b, breverses) <- go $ fa a
        pure (b, areverses ++ breverses)
