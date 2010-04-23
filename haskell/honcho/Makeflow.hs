{-# LANGUAGE
  FlexibleInstances,
  GADTs,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  PackageImports,
  TemplateHaskell,
  TypeOperators,
  TypeSynonymInstances
  #-}
  
import Data.Record.Label
import Language.Haskell.TH
import qualified "mtl" Control.Monad.State as S
import Control.Applicative
-- import Control.Monad.Parallel
import Text.Printf
import Data.List
import Data.Monoid
import System.FilePath
import Data.Maybe


data Rule = Rule {
      _command      :: String
    , _arguments    :: [String]
    , _outputs      :: [String]
    , _dependencies :: [String]
    }

$(mkLabels [''Rule])
command      :: Rule :-> FilePath
arguments    :: Rule :-> [String]
outputs      :: Rule :-> [String]
dependencies :: Rule :-> [String]




class Prepare a b where
    prepare :: a -> b


data ShellType = Bash | Csh | Zsh deriving Show

type Argument = String
type OutputFile = FilePath


data Redirection where
    StdErr :: Redirection
    StdOut :: Redirection
    Append :: Redirection
    Write :: Redirection
    Join :: Redirection -> OutputFile -> Redirection
    Redirect :: Redirection -> Redirection -> OutputFile -> Redirection
    deriving Show

writeStdOut = Redirect Write StdOut
writeStdErr = Redirect Write StdErr
appendStdOut = Redirect Append StdOut
appendStdErr = Redirect Append StdErr
writeOutErr = Join Write
appendOutErr = Join Append

instance Prepare Redirection String where
    prepare (Redirect Append StdErr file) = printf "1 >> %s" file
    prepare (Redirect StdErr Append file) = printf "1 >> %s" file
    prepare (Redirect Write StdErr file) = printf "1 > %s" file
    prepare (Redirect StdErr Write file) = printf "1 > %s" file
    prepare (Redirect Append StdOut file) = printf ">> %s" file
    prepare (Redirect StdOut Append file) = printf ">> %s" file
    prepare (Redirect Write StdOut file) = printf "> %s" file
    prepare (Redirect StdOut Write file) = printf "> %s" file
    prepare (Join Write file) = printf "> %s 2>&1" file
    prepare (Join Append file) = printf ">> %s 2>&1" file


data Program where
    Executable   :: FilePath  -> [Argument] -> Program
    Output       :: Program -> Redirection -> Program
    Group :: [Program] -> Program
    deriving Show

instance Monoid Program where
    mempty = Group []

    mappend (Group g1) (Group g2)                       = Group (g1 ++ g2)

    mappend g@(Group _) e@(Executable _ _)              = mappend e g
    mappend e@(Executable _ _) (Group g)                = Group (e:g)

    mappend g@(Group _) o@(Output _ _)                  = mappend o g
    mappend o@(Output _ _) (Group g)                    = Group (o:g)

    mappend p1@(Executable _ _) p2@(Output _ _)         = mappend p2 p1
    mappend (Output p1 redirection) p2@(Executable _ _) = Group [Output p1 redir, Output p2 redir]
                        where redir = case redirection of
                                        Redirect _ output file -> Redirect Append output file
                                        Join     _ file        -> Join Append file
    mappend p1 p2                                       = Group [p1,p2]


t1 = Executable "ls" []
t2 = Group []
t3 = Group [t4,t1,t1]
t4 = Output t1 (Join Append "output")


instance Prepare Program [String] where
    prepare (Executable path args) = [printf "%s %s" path (intercalate " " args)]
    prepare (Output exec redir) = let [prog] = prepare exec
                                      output = prepare redir
                                  in [prog ++ " " ++ output]
    prepare (Group g) = concatMap prepare . reverse $ g

data Cmd = Cmd {
      cmd_results :: [FilePath]
    , cmd_depends :: [FilePath]
    , cmd_program :: Program
    } deriving Show

class Makeflow a where
    makeflow :: a -> String

instance Makeflow Cmd where
    makeflow cmd = printf "%s : %s\n\t%s;\n" results depends commands
        where results  = intercalate " " (cmd_results cmd)
              depends  = intercalate " " (cmd_depends cmd)
              commands = intercalate "; " . prepare $ cmd_program cmd

instance Makeflow Workflow where
    makeflow = concatMap makeflow

instance Monoid Cmd where
    mempty = Cmd { cmd_results = [], cmd_depends = [], cmd_program = mempty }
    mappend c1 c2 = Cmd { cmd_results = cmd_results c1 `mappend` cmd_results c2
                        , cmd_depends = cmd_depends c1 `mappend` cmd_depends c2
                        , cmd_program = cmd_program c1 `mappend` cmd_program c2 }

newtype CmdBuilder a = CmdBuilder {
      runCmdBuilder :: S.State Cmd a
    } deriving (Monad, S.MonadState Cmd)

buildCmd :: CmdBuilder a -> Cmd
buildCmd = flip (S.execState . runCmdBuilder) mempty

add_to_cmd :: Cmd -> (Cmd -> Cmd) -> Cmd
add_to_cmd cmd mod = mappend cmd $ mod mempty

modify_cmd val mod = S.get >>= S.put . flip add_to_cmd mod

result :: FilePath -> CmdBuilder ()
result path = modify_cmd path (\cmd -> cmd {cmd_results = [path]})

depend :: FilePath -> CmdBuilder ()
depend path = modify_cmd path (\cmd -> cmd {cmd_depends = [path]})

runprogram :: Program -> CmdBuilder ()
runprogram prog = modify_cmd prog (\cmd -> cmd {cmd_program = prog})

output :: Program -> Redirection -> CmdBuilder ()
output prog redir = do runprogram $ Output prog redir
                       result outputfile
    where outputfile = case redir of Join _ file -> file
                                     Redirect _ _ file -> file

runshell :: String -> CmdBuilder ()
runshell command = runprogram $ Executable bin args
    where (bin:args) = words command



type Workflow = [Cmd]

-- newtype Weaver a = Weaver {
--       runWeaver :: S.State Workflow a
--     } deriving (Monad, S.MonadState Workflow)

-- buildWorkflow :: Weaver a -> Workflow
-- buildWorkflow = flip (S.execState . runWeaver) []

-- genWorkflow :: [CmdBuilder a] -> Workflow
-- genWorkflow builders = buildWorkflow $ mapM_ (add_cmd . buildCmd) builders

-- add_cmd :: Cmd -> Weaver ()
-- add_cmd cmd = S.modify (cmd :)

-- clear :: Weaver ()
-- clear = S.modify (const [])
