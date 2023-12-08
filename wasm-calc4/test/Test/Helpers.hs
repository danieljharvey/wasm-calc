{-# LANGUAGE LambdaCase #-}

module Test.Helpers
  ( int,
    bool,
    var,
    tuple,
    tyInt,
    tyBool,
    tyTuple,
  )
where

import Calc
import qualified Data.List.NonEmpty as NE
import Data.String

int :: (Monoid ann) => Integer -> Expr ann
int = EPrim mempty . PInt

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

var :: (Monoid ann) => String -> Expr ann
var = EVar mempty . Identifier . fromString

tuple :: (Monoid ann) => [Expr ann] -> Expr ann
tuple = \case
  (a : b : rest) -> ETuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tuple"

tyInt :: (Monoid ann) => Type ann
tyInt = TPrim mempty TInt

tyBool :: (Monoid ann) => Type ann
tyBool = TPrim mempty TBool

tyTuple :: (Monoid ann) => [Type ann] -> Type ann
tyTuple = \case
  (a : b : rest) -> TTuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tyTuple"
