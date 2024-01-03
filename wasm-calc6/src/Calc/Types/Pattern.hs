{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
module Calc.Types.Pattern where

import           Calc.Types.Identifier
import qualified Data.List.NonEmpty    as NE

data Pattern ann
  = PVar ann Identifier
  | PWildcard ann
  | PTuple ann (Pattern ann) (NE.NonEmpty (Pattern ann))
  | PBox ann (Pattern ann)
  deriving stock (Eq,Ord,Show,Functor)
