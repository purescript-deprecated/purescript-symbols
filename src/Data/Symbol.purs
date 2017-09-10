module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  ) where

import Prelude ((<>))
import Unsafe.Coerce (unsafeCoerce)

-- | A class for known symbols
class IsSymbol (sym :: Symbol) where
  reflectSymbol :: @sym -> String

instance isSymbolTypeConcat :: (IsSymbol left, IsSymbol right) => IsSymbol (TypeConcat left right) where
  reflectSymbol _ = reflectSymbol @left <> reflectSymbol @right

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => @sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } @"" where
  coerce
    :: (forall sym1. IsSymbol sym1        => @sym1 -> r)
    -> { reflectSymbol :: @"" -> String } -> @""   -> r
  coerce = unsafeCoerce
