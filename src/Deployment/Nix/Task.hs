module Deployment.Nix.Task(
    Task(..)
  , executeTask
  , reverseTask
  ) where

import Shelly
import Data.Text (Text)

-- | Reversable task on remote machine
data Task a where
  -- | Atomic task
  AtomTask :: {
      -- | Optional task name
      taskName    :: Maybe Text
      -- | Check that task is need to be applied. 'True' means need to apply, second
      -- value is used as default result of task if no need to apply and as default
      -- value of task on reverse process.
    , taskCheck   :: Sh (Bool, a)
      -- | Apply task
    , taskApply   :: Sh a
      -- | Revert task side effects (if possible)
    , taskReverse :: Sh ()
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
executeTask :: forall a . Task a -> Sh a
executeTask = fmap fst . go []
  where
    go :: [Sh ()] -> Task b -> Sh (b, [Sh ()])
    go reverses t = case t of
      AtomTask{..} -> do
        (needApply, a) <- taskCheck
        if needApply then (, [taskReverse]) <$> taskApply
          else pure (a, [taskReverse])
      TaskApplicative fa ta -> do
        (f, freverses) <- go reverses fa
        (a, areverses) <- go reverses ta
        pure (f a, freverses ++ areverses)
      TaskMonadic ta fa -> do
        (a, areverses) <- go reverses ta
        (b, breverses) <- go reverses $ fa a
        pure (b, areverses ++ breverses)

-- | Apply reverse action of task
reverseTask :: forall a . Task a -> Sh ()
reverseTask t = do
  (_, ms) <- go t
  sequence_ ms
  where
    go :: Task b -> Sh (b, [Sh ()])
    go t = case t of
      AtomTask{..} -> do
        (needApply, a) <- taskCheck
        pure $ if needApply then (a, []) else (a, [taskReverse])
      TaskApplicative fa ta -> do
        (f, freverses) <- go fa
        (a, areverses) <- go ta
        pure (f a, freverses ++ areverses)
      TaskMonadic ta fa -> do
        (a, areverses) <- go ta
        (b, breverses) <- go $ fa a
        pure (b, areverses ++ breverses)
