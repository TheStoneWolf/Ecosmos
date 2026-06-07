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

-- TODO Think about how implementing z-coordinate will work as it must be allowed to be negative
-- z :: forall a b .(Num a, Num b) => HexCoord a -> b
-- z coord = -fromIntegral (x coord) :: b - fromIntegral (y coord) :: b

instance (Num a) => Semigroup (HexCoord a) where
  (HexCoord x0 y0) <> (HexCoord x1 y1) = HexCoord (x0 + x1) (y0 + y1)

increment :: (Num a, Eq a, Bounded a) => HexCoord a -> HexCoord a
increment (HexCoord x0 y0)
  | x0 == maxBound = HexCoord minBound (y0 + 1)
  | otherwise = HexCoord (x0 + 1) y0 -- y0 will wrap when max is reached as intended
