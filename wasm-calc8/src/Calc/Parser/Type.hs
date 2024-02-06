{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Type (typeParser) where

import           Calc.Parser.Identifier
import           Calc.Parser.Shared     (addTypeLocation, myLexeme,
                                         stringLiteral)
import           Calc.Parser.Types
import           Calc.Types.Type
import           Data.Functor           (($>))
import qualified Data.List.NonEmpty     as NE
import           Text.Megaparsec        (MonadParsec (try), label, sepBy1,
                                         (<|>))

-- | top-level parser for type signatures
typeParser :: Parser ParserType
typeParser =
  tyPrimitiveParser <|> tyTupleParser <|> tyBoxParser <|> tyVarParser

tyPrimitiveParser :: Parser ParserType
tyPrimitiveParser = myLexeme $ addTypeLocation $ TPrim mempty <$> tyPrimParser
  where
    tyPrimParser =
      try (stringLiteral "Boolean" $> TBool)
        <|> try (stringLiteral "Int16" $> TInt16)
        <|> try (stringLiteral "Int32" $> TInt32)
        <|> try (stringLiteral "Int64" $> TInt64)
        <|> (stringLiteral "Float32" $> TFloat32)
        <|> (stringLiteral "Float64" $> TFloat64)
        <|> stringLiteral "Void"
        $> TVoid

tyBoxParser :: Parser ParserType
tyBoxParser = label "box" $
  addTypeLocation $ do
    _ <- stringLiteral "Box"
    _ <- stringLiteral "("
    tyInner <- typeParser
    _ <- stringLiteral ")"
    pure (TContainer mempty $ NE.singleton tyInner)

-- | tuples use container, but we parse them distinctly
tyTupleParser :: Parser ParserType
tyTupleParser = label "tuple" $
  addTypeLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 typeParser (stringLiteral ",")
    _neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _       -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (TContainer mempty neArgs)

tyVarParser :: Parser ParserType
tyVarParser = label "type variable" $
  addTypeLocation $ do
    TVar mempty <$> typeVarParser
