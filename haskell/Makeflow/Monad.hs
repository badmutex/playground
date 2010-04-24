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
import Text.Printf
import Data.List
import Data.Monoid
import System.FilePath
import Data.Maybe
import Data.Record.Label
import Language.Haskell.TH
import Control.Monad




class Prepare a b where
    prepare :: a -> b


data ShellType = Bash | Csh | Zsh deriving Show

type Argument = String
type OutputFile = FilePath


data Redirection where
    StdErr   :: Redirection
    StdOut   :: Redirection
    Append   :: Redirection
    Write    :: Redirection
    Join     :: Redirection -> OutputFile -> Redirection
    Redirect :: Redirection -> Redirection -> OutputFile -> Redirection
    deriving Show

writeStdOut  = Redirect Write StdOut
writeStdErr  = Redirect Write StdErr
appendStdOut = Redirect Append StdOut
appendStdErr = Redirect Append StdErr
writeOutErr  = Join Write
appendOutErr = Join Append



instance Prepare Redirection String where
    prepare (Redirect Append StdErr file) = printf "1 >> %s" file
    prepare (Redirect StdErr Append file) = printf "1 >> %s" file
    prepare (Redirect Write StdErr file)  = printf "1 > %s" file
    prepare (Redirect StdErr Write file)  = printf "1 > %s" file
    prepare (Redirect Append StdOut file) = printf ">> %s" file
    prepare (Redirect StdOut Append file) = printf ">> %s" file
    prepare (Redirect Write StdOut file)  = printf "> %s" file
    prepare (Redirect StdOut Write file)  = printf "> %s" file
    prepare (Join Write file)             = printf "> %s 2>&1" file
    prepare (Join Append file)            = printf ">> %s 2>&1" file


data Program where
    Executable :: FilePath  -> [Argument]  -> Program
    Output     :: Program   -> Redirection -> Program
    Group      :: [Program] -> Program
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


instance Prepare Program [String] where
    prepare (Executable path args) = [printf "%s %s" path (intercalate " " args)]
    prepare (Output exec redir) = let [prog] = prepare exec
                                      output = prepare redir
                                  in [prog ++ " " ++ output]
    prepare (Group g) = concatMap prepare . reverse $ g

data Cmd = Cmd {
      _cmd_results :: [FilePath]
    , _cmd_depends :: [FilePath]
    , _cmd_program :: Program
    } deriving Show

$(mkLabels [''Cmd])
cmd_results :: Cmd :-> [FilePath]
cmd_depends :: Cmd :-> [FilePath]
cmd_program :: Cmd :-> Program


class Makeflow a where
    makeflow :: a -> String

instance Makeflow Cmd where
    makeflow cmd = printf "%s : %s\n\t%s;\n" results depends commands
        where results  = intercalate " " (get cmd_results cmd)
              depends  = intercalate " " (get cmd_depends cmd)
              commands = intercalate "; " . prepare $ get cmd_program cmd



instance Monoid Cmd where
    mempty = Cmd { _cmd_results = mempty, _cmd_depends = mempty, _cmd_program = mempty }
    mappend c1 c2 = Cmd { _cmd_results = _cmd_results c1 `mappend` _cmd_results c2
                        , _cmd_depends = _cmd_depends c1 `mappend` _cmd_depends c2
                        , _cmd_program = _cmd_program c1 `mappend` _cmd_program c2 }

newtype CmdBuilder a = CmdBuilder {
      runCmdBuilder :: S.State Cmd a
    } deriving (Monad, S.MonadState Cmd)

buildCmd :: CmdBuilder a -> Cmd
buildCmd = flip (S.execState . runCmdBuilder) mempty


modify_cmd m f v = S.get >>= S.put . add f (m v) >> S.get
    where add f x a = set f (x `mappend` get f a) a

result :: FilePath -> CmdBuilder Cmd
result = modify_cmd return cmd_results

depend :: FilePath -> CmdBuilder Cmd
depend = modify_cmd return cmd_depends

runprogram :: Program -> CmdBuilder Cmd
runprogram = modify_cmd id cmd_program

output :: Program -> Redirection -> CmdBuilder Cmd
output prog redir = do runprogram $ Output prog redir
                       result outputfile
    where outputfile = case redir of Join _ file -> file
                                     Redirect _ _ file -> file

shellCmd :: String -> CmdBuilder Cmd
shellCmd = runprogram . shell

shell :: String -> Program
shell s = Executable bin args
    where (bin:args) = words s

redirBuilder f prog file = output (shell prog) (f file)
(>:)                     = redirBuilder writeStdOut
(>::)                    = redirBuilder writeStdErr
(>>:)                    = redirBuilder appendStdOut
(>>::)                   = redirBuilder appendStdErr
(>*)                     = redirBuilder writeOutErr
(>>*)                    = redirBuilder appendOutErr


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


