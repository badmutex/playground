{-# LANGUAGE
  GADTs
  , GeneralizedNewtypeDeriving
  #-}

import Control.Monad.State
import Control.Applicative
-- import Control.Monad.Parallel
import Data.Graph.Inductive
import Text.Printf
import Data.List
import Data.Monoid


data ShellType = Bash | Csh | Zsh deriving Show

type Argument = String
type OutputFile = String


data Redirection where
    CaptureStdErr :: OutputFile -> Redirection
    CaptureStdOut :: OutputFile -> Redirection
    Separate :: Redirection -> Redirection -> Redirection
    Join :: OutputFile -> Redirection
    Ignore :: Redirection
    deriving Show

data Program where
    Executable :: FilePath  -> [Argument] -> Redirection -> Program
    Group      :: [Program] -> Program
    EmptyProgram :: Program

instance Monoid Program where
    mempty = EmptyProgram
    mappend e1@(Executable _ _ _) e2@(Executable _ _ _) = Group [e1, e2]
    mappend (Group g) e@(Executable _ _ _) = Group (e : g)
    mappend e@(Executable _ _ _) g@(Group _) = mappend g e
    mappend EmptyProgram EmptyProgram = EmptyProgram
    mappend EmptyProgram p@(_) = p
    mappend p@(_) EmptyProgram = p

instance Show Program where
    show (Executable path args redir) = printf "Executable %s %s %s" (show path) (show args) (show redir)
    show (Group ps) = printf "Group %s" (show ps)
    show EmptyProgram = "EmptyProgram"

data Cmd = Cmd {
      cmd_results :: [FilePath]
    , cmd_depends :: [FilePath]
    , cmd_program :: Program
    } deriving Show

instance Monoid Cmd where
    mempty = Cmd { cmd_results = [], cmd_depends = [], cmd_program = EmptyProgram }
    mappend c1 c2 = Cmd { cmd_results = cmd_results c1 `mappend` cmd_results c2
                        , cmd_depends = cmd_depends c1 `mappend` cmd_depends c2
                        , cmd_program = cmd_program c1 `mappend` cmd_program c2   }

newtype CmdBuilder a = CmdBuilder {
      runCmdBuilder :: State Cmd a
    } deriving (Monad, MonadState Cmd)

buildCmd :: CmdBuilder a -> Cmd
buildCmd = flip (execState . runCmdBuilder) mempty

add_to_cmd :: Cmd -> (Cmd -> Cmd) -> Cmd
add_to_cmd cmd mod = mappend cmd $ mod mempty

modify_cmd val mod = get >>= put . flip add_to_cmd mod

result :: FilePath -> CmdBuilder ()
result path = modify_cmd path (\cmd -> cmd {cmd_results = [path]})

depends :: FilePath -> CmdBuilder ()
depends path = modify_cmd path (\cmd -> cmd {cmd_depends = [path]})

program :: Program -> CmdBuilder ()
program prog = modify_cmd prog (\cmd -> cmd {cmd_program = prog})




type Makeflow = [Cmd]

newtype Weaver a = Weaver {
      runWeaver :: State Makeflow a
    } deriving (Monad, MonadState Makeflow)


add_cmd :: Cmd -> Weaver ()
add_cmd cmd = modify (cmd :)

clear :: Weaver ()
clear = modify (const [])

prog = Executable "/bin/ls" [] Ignore

test_cmdbuilder = buildCmd builder where
    builder = do result "hello"
                 result "world"
                 depends "/bin/echo"
                 program prog
                 program $ Executable "/bin/who" [] (Join "who.out-err")
