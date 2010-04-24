module Makeflow.Workflow where

import Makeflow.Monad



type Workflow = [Cmd]

instance Makeflow Workflow where
    makeflow = concatMap makeflow


data WFState = WFState {
      _wfoutput :: Integer
    , _workflow :: Workflow
    }

newtype WorkflowM a = WorkflowM {
      runWorkflow :: S.State WFState a
    } deriving (Monad, S.MonadState WFState)







-- buildWorkflowM :: WorkflowM a -> [Cmd]
buildWorkflowM = flip (S.execState . runWorkflow) 

-- workflow :: [CmdBuilder a] -> Workflow
-- workflow builders = buildWorkflowM $ mapM_ (S.modify . (:) . buildCmd) builders
