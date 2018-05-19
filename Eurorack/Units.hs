{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Eurorack.Units (HorizontalPitch(..), RackUnit(..)) where

import Data.Metrology.TH (declareDerivedUnit)
import Data.Units.US (Inch)

declareDerivedUnit "HorizontalPitch" [t| Inch |] 0.2  (Just "HP")
declareDerivedUnit "RackUnit"        [t| Inch |] 1.75 (Just "U")

