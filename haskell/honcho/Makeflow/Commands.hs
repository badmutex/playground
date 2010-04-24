module Honcho.Makeflow.Commands

import Honcho.Makeflow.monad



cat out cmds = let c = mconcat cmds
               in do mapM_ depend (get cmd_results c)
                     printf "cat %s" (intercalate " " (get cmd_results c)) >: out



catResults out wf = map (mkcmd out) $ wf
mkcmd out builder = do
  cmd <- builder
  return $ set cmd_results out $
           set cmd_depends (get cmd_results cmd) $
           mempty

chunk :: (Integral i) => i -> [a] -> [[a]]
chunk n xs = chunk' i xs
      where
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
        i = ceiling (fromIntegral (length xs) / fromIntegral n)

