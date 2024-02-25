module FailState(FailState, failFS, getFS, putFS, modifyFS, logFS,
                 runFS, evalFS, execFS) where

-- The FailState monad encodes two kinds of side-effects:
--   1) state
--   2) failure

import Debug.Trace(trace)
import Error(Error)

data FailState state a = FailState (state -> Either Error (a, state))

instance Functor (FailState state) where
  fmap f (FailState ma) =
    FailState (\ s0 ->
      case ma s0 of
        Left  e       -> Left e
        Right (a, s1) -> Right (f a, s1))

instance Applicative (FailState state) where
  pure a = FailState (\ s -> Right (a, s))
  FailState mf <*> FailState ma =
    FailState (\ s0 ->
      case mf s0 of
        Left e        -> Left e
        Right (f, s1) ->
          case ma s1 of
            Left e        -> Left e
            Right (a, s2) -> Right (f a, s2))

instance Monad (FailState state) where
  return = pure
  FailState ma >>= f =
    FailState (\ s0 ->
      case ma s0 of
        Left e        -> Left e
        Right (a, s1) -> let FailState mb = f a in mb s1)

failFS :: Error -> FailState state a
failFS e = FailState (\ _ -> Left e)

getFS :: FailState state state
getFS = FailState (\ s -> Right (s, s))

putFS :: state -> FailState state ()
putFS s = FailState (\ _ -> Right ((), s))

modifyFS :: (state -> state) -> FailState state ()
modifyFS f = FailState (\ s -> Right ((), f s))

logFS :: String -> FailState state ()
logFS msg = trace msg (return ())

runFS :: FailState state a -> state -> Either Error (a, state)
runFS (FailState ma) s0 = ma s0

evalFS :: FailState state a -> state -> Either Error a
evalFS (FailState ma) s0 =
  case ma s0 of
    Left  e      -> Left e
    Right (a, _) -> Right a

execFS :: FailState state a -> state -> Either Error state
execFS (FailState ma) s0 =
  case ma s0 of
    Left  e      -> Left e
    Right (_, s) -> Right s

