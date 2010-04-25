
module Makeflow.Util where

import Data.Monoid

wrappedStateMonad :: (Monoid c) => (a -> b) -> (b -> c -> d) -> a -> d
wrappedStateMonad c f = flip (f . c) mempty