module FailState(
         ErrMsg, FailState,
         runFS, execFS, evalFS, getFS, putFS, modifyFS, failFS, logFS,
         tryOrBacktrackFS
       ) where

import Debug.Trace(trace)

type ErrMsg = String

data FailState s a = FS (s -> Either ErrMsg (s, a))

instance Functor (FailState s) where
  fmap f (FS sa) =
    FS (\ s0 ->
      case sa s0 of
        Left msg      -> Left msg
        Right (s1, a) -> Right (s1, f a))

instance Applicative (FailState s) where
  pure a = FS (\ s0 -> Right (s0, a))
  FS sf <*> FS sa =
    FS (\ s0 ->
      case sf s0 of
        Left msg -> Left msg
        Right (s1, f) ->
          case sa s1 of
            Left msg -> Left msg
            Right (s2, a) -> Right (s2, f a))

instance Monad (FailState s) where
  FS sa >>= f =
    FS (\ s0 ->
      case sa s0 of
        Left msg -> Left msg
        Right (s1, a) ->
          let FS fb = f a in
            fb s1)

runFS :: s -> FailState s a -> Either ErrMsg (s, a)
runFS s0 (FS sa) = sa s0

execFS :: s -> FailState s a -> Either ErrMsg s
execFS s0 x = fst <$> runFS s0 x

evalFS :: s -> FailState s a -> Either ErrMsg a
evalFS s0 x = snd <$> runFS s0 x

getFS :: FailState s s
getFS = FS (\ s0 -> Right (s0, s0))

putFS :: s -> FailState s ()
putFS s = FS (\ _ -> Right (s, ()))

modifyFS :: (s -> s) -> FailState s ()
modifyFS f = FS (\ s0 -> Right (f s0, ()))

failFS :: ErrMsg -> FailState s a
failFS msg = FS (\ s0 -> Left msg)

logFS :: String -> FailState s ()
logFS msg = FS (\ s -> trace msg (Right (s, ())))

tryOrBacktrackFS :: FailState s a -> FailState s a -> FailState s a
tryOrBacktrackFS alt1 alt2 = do
  state <- getFS
  case runFS state alt1 of
    Right (state', a) -> do putFS state'
                            return a
    Left msg -> alt2

