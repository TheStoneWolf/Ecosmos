module Example.Hex where

import Clash.Prelude
import Clash.Shockwaves

-- The type-level solver is not the brightest so all type-level computations must be included
type AddrConstraints a =
  (KnownNat a, 1 <= a, 1 <= (2 ^ a) - 1)

data HexCoord a = HexCoord
  { x :: a,
    y :: a,
    z :: a
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, Waveform)

hexCoord :: (Num a, Eq a) => a -> a -> a -> Maybe (HexCoord a)
hexCoord x y z
  | x + y + z == 0 = Just (HexCoord x y z)
  | otherwise = Nothing

instance (Num a) => Semigroup (HexCoord a) where
  (HexCoord x0 y0 z0) <> (HexCoord x1 y1 z1) = HexCoord (x0 + x1) (y0 + y1) (z0 + z1)
