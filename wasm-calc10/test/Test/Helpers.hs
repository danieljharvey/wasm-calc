{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers
  ( joinLines,
    int,
    bool,
    float,
    box,
    var,
    tuple,
    tyVoid,
    tyInt32,
    tyInt64,
    tyFloat64,
    tyBool,
    tyContainer,
    tyConstructor,
    tyVar,
    patTuple,patInt,patVar
  )
where

import Calc
import qualified Data.List.NonEmpty as NE
import Data.String
import qualified Data.Text as T
import Data.Word

joinLines :: [T.Text] -> T.Text
joinLines = T.intercalate "\n"

int :: (Monoid ann) => Word64 -> Expr ann
int = EPrim mempty . PIntLit

float :: (Monoid ann) => Double -> Expr ann
float = EPrim mempty . PFloatLit

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

var :: (Monoid ann) => String -> Expr ann
var = EVar mempty . Identifier . fromString

box :: (Monoid ann) => Expr ann -> Expr ann
box = EBox mempty

tuple :: (Monoid ann) => [Expr ann] -> Expr ann
tuple = \case
  (a : b : rest) -> ETuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tuple"

tyVoid :: (Monoid ann) => Type ann
tyVoid = TPrim mempty TVoid

tyInt32 :: (Monoid ann) => Type ann
tyInt32 = TPrim mempty TInt32

tyInt64 :: (Monoid ann) => Type ann
tyInt64 = TPrim mempty TInt64

tyFloat64 :: (Monoid ann) => Type ann
tyFloat64 = TPrim mempty TFloat64

tyBool :: (Monoid ann) => Type ann
tyBool = TPrim mempty TBool

tyContainer :: (Monoid ann) => [Type ann] -> Type ann
tyContainer = \case
  (a : rest) -> TContainer mempty (a NE.:| rest)
  _ -> error "not enough items for tyContainer"

tyConstructor :: (Monoid ann) => Constructor -> [Type ann] -> Type ann
tyConstructor =
  TConstructor mempty . DataName

tyVar :: (Monoid ann) => String -> Type ann
tyVar = TVar mempty . TypeVar . fromString

patTuple :: (Monoid ann) => [Pattern ann] -> Pattern ann
patTuple = \case
  (a : rest) -> PTuple mempty a (NE.fromList rest)
  _ -> error "not enough items for patTuple"

patInt :: (Monoid ann) => Word64 -> Pattern ann
patInt = PLiteral mempty . PIntLit

patVar :: (Monoid ann) => String -> Pattern ann
patVar = PVar mempty . fromString



