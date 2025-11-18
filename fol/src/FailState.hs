module FailState(FailState, failFS, getFS, putFS, modifyFS, logFS,
                 runFS, evalFS, execFS) where

-- The FailState monad encodes two kinds of side-effects:
--   1) state
--   2) failure

import Debug.Trace(trace)

data FailState e state a = FailState (state -> Either e (a, state))

instance Functor (FailState e state) where
  fmap f (FailState ma) =
    FailState (\ s0 ->
      case ma s0 of
        Left  e       -> Left e
        Right (a, s1) -> Right (f a, s1))

instance Applicative (FailState e state) where
  pure a = FailState (\ s -> Right (a, s))
  FailState mf <*> FailState ma =
    FailState (\ s0 ->
      case mf s0 of
        Left e        -> Left e
        Right (f, s1) ->
          case ma s1 of
            Left e        -> Left e
            Right (a, s2) -> Right (f a, s2))

instance Monad (FailState e state) where
  return = pure
  FailState ma >>= f =
    FailState (\ s0 ->
      case ma s0 of
        Left e        -> Left e
        Right (a, s1) -> let FailState mb = f a in mb s1)

failFS :: e -> FailState e state a
failFS e = FailState (\ _ -> Left e)

getFS :: FailState e state state
getFS = FailState (\ s -> Right (s, s))

putFS :: state -> FailState e state ()
putFS s = FailState (\ _ -> Right ((), s))

modifyFS :: (state -> state) -> FailState e state ()
modifyFS f = FailState (\ s -> Right ((), f s))

logFS :: String -> FailState e state ()
logFS msg = trace msg (return ())

runFS :: FailState e state a -> state -> Either e (a, state)
runFS (FailState ma) s0 = ma s0

evalFS :: FailState e state a -> state -> Either e a
evalFS (FailState ma) s0 =
  case ma s0 of
    Left  e      -> Left e
    Right (a, _) -> Right a

execFS :: FailState e state a -> state -> Either e state
execFS (FailState ma) s0 =
  case ma s0 of
    Left  e      -> Left e
    Right (_, s) -> Right s

