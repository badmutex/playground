{-# LANGUAGE
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  PackageImports,
  TemplateHaskell,
  TypeOperators,
  TypeSynonymInstances
  #-}

module Makeflow.Workflow where


import Makeflow.Monad
import Makeflow.Commands
import Makeflow.Util

import qualified "mtl" Control.Monad.State as S
import Data.Record.Label
import Control.Monad
import Data.List (intercalate)
import Data.Monoid
import Text.Printf
import Language.Haskell.TH
import Prelude hiding (mod)
import Data.Monoid

type Workflow = [Cmd]

instance Makeflow Workflow where
    makeflow = concatMap makeflow


data WFState = WFState {
      _wfoutputcount :: Integer
    , _workflow      :: Workflow
    }

$(mkLabels [''WFState])
wfoutputcount :: WFState :-> Integer
workflow      :: WFState :-> Workflow


instance Monoid WFState where
    mempty = WFState { _wfoutputcount = 0, _workflow = [] }
    mappend a b = mod wfoutputcount ((+)  (get wfoutputcount b)) $
                  mod workflow      ((++) (get workflow b))      $
                  a



newtype WorkflowM a = WorkflowM {
      runWorkflowMonad :: S.State WFState a
    } deriving (Functor, Monad, S.MonadState WFState)


cat :: Workflow -> WorkflowM OutputFile
cat cmds = do
  i <- get wfoutputcount `fmap` S.get
  let c    = mconcat cmds
      out  = printf "out_%09X.txt" i
      meow = buildCmd $ do depends (get cmd_results c)
                           printf "cat %s" (intercalate " " (get cmd_results c)) >: out

  S.modify (mod wfoutputcount (+ 1) .
            mod workflow ((++) (meow:cmds)))
  return out

wrapWorkflowM :: (S.State WFState a -> WFState -> b) -> WorkflowM a -> b
wrapWorkflowM = wrappedStateMonad runWorkflowMonad

runWorkflowM :: WorkflowM a -> (a, WFState)
runWorkflowM = wrapWorkflowM S.runState

execWorkflowM = snd . runWorkflowM

-- buildWorkflowM :: WFState -> WorkflowM a -> Workflow
-- buildWorkflowM wfstate = get workflow . flip (S.execState . runWorkflow) wfstate
