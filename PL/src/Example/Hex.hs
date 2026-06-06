module Example.Hex where

import Clash.Prelude
import Clash.Shockwaves

-- The type-level solver is not the brightest so all type-level computations must be included
type AddrConstraints a =
  (KnownNat a, 1 <= a, 1 <= (2 ^ a) - 1)

data HexCoord a = HexCoord
  { x :: a,
    y :: a
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, Waveform)

hexCoord :: (Num a, Eq a) => a -> a -> Maybe (HexCoord a)
hexCoord x y
  | x + y == 0 = Just (HexCoord x y)
  | otherwise = Nothing

instance (Num a) => Semigroup (HexCoord a) where
  (HexCoord x0 y0) <> (HexCoord x1 y1) = HexCoord (x0 + x1) (y0 + y1)
