module VegetationCalc (vegetationTick, vegetationGain) where

import Clash.Class.Num
import Clash.Prelude

vegetationGain :: (KnownNat dataWidth) => Unsigned dataWidth
vegetationGain = 4

-- | Calculate vegetation for next tick
--
-- >>> import Clash.Prelude
-- >>> vegetationTick @4 12
-- 0
-- >>> import Clash.Prelude
-- >>> (vegetationTick @4 5) - vegetationGain
-- 5
vegetationTick :: (KnownNat dataWidth) => Unsigned dataWidth -> Unsigned dataWidth
vegetationTick inData = satAdd SatWrap inData vegetationGain
