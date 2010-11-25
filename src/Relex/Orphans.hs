module Relex.Orphans where

import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
