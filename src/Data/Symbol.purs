module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  , SProxy(..)
  ) where

import Prelude ((<>))
import Unsafe.Coerce (unsafeCoerce)

-- | A value-level proxy for a type-level symbol.
data SProxy (sym :: Symbol) = SProxy

-- | A class for known symbols
class IsSymbol (sym :: Symbol) where
  reflectSymbol :: SProxy sym -> String

instance isSymbolTypeConcat :: (IsSymbol left, IsSymbol right) => IsSymbol (TypeConcat left right) where
  reflectSymbol _ = reflectSymbol (SProxy :: SProxy left) <> reflectSymbol (SProxy :: SProxy right)

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => SProxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } SProxy where
  coerce 
    :: (forall sym1. IsSymbol sym1              => SProxy sym1 -> r) 
    -> { reflectSymbol :: SProxy "" -> String } -> SProxy ""   -> r
  coerce = unsafeCoerce
