module Makeflow.Commands where

import Makeflow.Monad

import Data.Monoid
import Data.Record.Label
import Data.List (intercalate)
import Text.Printf



-- | The result of a command
result :: FilePath -> CmdBuilder Cmd
result = modify_cmd return cmd_results

-- | Make the command depend on this file
depend :: FilePath -> CmdBuilder Cmd
depend = modify_cmd return cmd_depends

-- | Adds these files as dependencies
depends :: [FilePath] -> CmdBuilder [FilePath]
depends fs = mapM_ depend fs >> return fs

-- | The 'Program' to run
program :: Program -> CmdBuilder Cmd
program = modify_cmd id cmd_program

-- | Redirect the output of a program
output :: Program -> Redirection -> CmdBuilder Cmd
output prog redir = do program $ Output prog redir
                       result outputfile
    where outputfile = case redir of Join _ file -> file
                                     Redirect _ _ file -> file

shellCmd :: String -> CmdBuilder Cmd
shellCmd = program . shell


-- | Make a 'Program' directly from a string
-- > shell "cat /dev/random"
shell :: String -> Program
shell s = Executable bin args
    where (bin:args) = words s

redirBuilder f prog file = output (shell prog) (f file)

-- | Build commands redirected to outputfiles
(>:),(>::),(>>:),(>>::),(>*),(>>*) :: String -> OutputFile -> CmdBuilder Cmd

(>:)                     = redirBuilder writeStdOut -- ^ redirect StdOut to a file
(>::)                    = redirBuilder writeStdErr -- ^ redirect StdErr to a file
(>>:)                    = redirBuilder appendStdOut -- ^ append StdOut to a file
(>>::)                   = redirBuilder appendStdErr -- ^ append StdErr to a file
(>*)                     = redirBuilder writeOutErr -- ^ join StdOut and StdErr, writing to a file
(>>*)                    = redirBuilder appendOutErr -- ^ join StdOut and StdErr, appending to a file


-- | Cot the outputs of these commands into a file
cat :: OutputFile -> [Cmd] -> CmdBuilder Cmd
cat out cmds = let c = mconcat cmds
               in do depends (get cmd_results c)
                     printf "cat %s" (intercalate " " (get cmd_results c)) >: out


chunk :: (Integral i) => i -> [a] -> [[a]]
chunk n xs = chunk' i xs
      where
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
        i = ceiling (fromIntegral (length xs) / fromIntegral n)



t1 = do
  "cat foo.txt bar.txt" >>* "out1.txt"
  mapM_ depend [ "/tmp/foo.txt"
               , "/tmp/bar.txt" ]

t1' = do
  "cat baz.txt bang.txt" >>* "out2.txt"
  mapM_ depend [ "/tmp/foo.txt"
               , "/tmp/bar.txt" ]


t2 deps fout fin = do
  printf "cat %s" fin >* fout
  mapM_ depend deps

t3 = t2 ["/tmp/foo.txt","/tmp/bar.txt"]
t4 = zipWith t3 (map ((++".txt") . show) [1..2]) ["/tmp/hello.txt", "/tmp/world.txt"]


