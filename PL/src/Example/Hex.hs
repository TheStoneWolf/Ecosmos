module Example.Hex where

import Clash.Prelude
import GHC.Generics (Generic)

data HexCoord a = HexCoord
  { x :: a,
    y :: a,
    z :: a
  }
  deriving (Show, Eq, Generic, NFDataX)

hexCoord :: (Num a, Eq a) => a -> a -> a -> Maybe (HexCoord a)
hexCoord x y z
  | x + y + z == 0 = Just (HexCoord x y z)
  | otherwise = Nothing

instance (Num a) => Semigroup (HexCoord a) where
  (HexCoord x0 y0 z0) <> (HexCoord x1 y1 z1) = HexCoord (x0 + x1) (y0 + y1) (z0 + z1)
