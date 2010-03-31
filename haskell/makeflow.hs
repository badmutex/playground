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


data ShellType = Bash | Csh | Zsh

type Argument = String
type OutputFile = String

class HasOutputFile a where
    output :: a -> String

data Output a where
    StdErr :: OutputFile -> Output ShellType
    StdOut :: OutputFile -> Output ShellType
    Separate :: Output ShellType -> Output ShellType ->  OutputFile  -> Output ShellType
    Join :: OutputFile -> Output ShellType

data Program where
    Executable :: FilePath  -> [Argument] -> Program
    Pipe       :: Program   -> Program    -> Program
    Redirect   :: Program    -> Output ShellType  -> Program
    Group      :: [Program] -> Program
    EmptyProgram :: Program

-- | TODO: finish
instance Show Program where
    show (Executable path args) = printf "%s %s" path (intercalate " " args)
    show (Group ps) = intercalate "\n" . map show $ ps
    show EmptyProgram = ""

data Cmd = Cmd {
      results :: [FilePath]
    , depends :: [FilePath]
    , program :: Program
    }

instance Monoid Cmd where
    mempty = Cmd { results = [], depends = [], program = EmptyProgram }
    mappend c1 c2 = Cmd { results = results c1 ++ results c2
                        , depends = depends c1 ++ depends c2
                        , program = Group [program c1, program c2]}

-- | TODO: finish
build_cmd :: Cmd -> Program -> Cmd
build_cmd cmd p@(Executable path _) = cmd {program = p}
build_cmd cmd (Pipe p1 p2) = undefined


instance Show Cmd where
    show cmd = let fmt f = intercalate " " (f cmd)
                   outs = fmt results
                   deps = fmt depends
                   prog = show $ program cmd
               in printf "%s : %s\n\t%s" outs deps prog

type Makeflow = [Cmd]

newtype Weaver a = Weaver {
      runWeaver :: State Makeflow a
    } deriving (Monad, MonadState Makeflow)


add_cmd :: Cmd -> Weaver ()
add_cmd cmd = modify (cmd :)

clear :: Weaver ()
clear = modify (const [])

prog = Executable "/bin/ls" []
-- cmd = Cmd ['test.out
