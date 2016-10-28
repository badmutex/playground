

import Makeflow.Commands
import Makeflow.Monad
import Makeflow.Run
import Makeflow.Workflow

import Data.Monoid

hello = "echo hello" >: "hello.out"

world = "echo world" >: "world.out"
hw = let [h,w] = map buildCmd [hello,world]
     in cat [h,w]

-- cs = map buildCmd [hello,world,helloworld]
-- cs' = joinCmds [hello,world]
