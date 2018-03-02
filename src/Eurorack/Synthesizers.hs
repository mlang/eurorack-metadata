{-# LANGUAGE DeriveFunctor, DeriveFoldable, FlexibleContexts, QuasiQuotes, OverloadedStrings, TemplateHaskell, TypeFamilies, TypeOperators #-}
module Eurorack.Synthesizers (
  Module(..), Row, Case, System, renderToDirectory
) where
import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (when, unless)
import Control.Monad.Extra (unlessM)
import Data.Foldable (for_, maximum, sum, toList, traverse_)
import Data.List (nub, (\\))
import Data.Maybe (fromJust)
import Data.Metrology ((%), (#), (|+|), (|*|), (:@), qSum)
import Data.Metrology.Poly (showIn)
import Data.Metrology.Show
import Data.Metrology.SI (Current, ElectricPotential, Power, Length)
import Data.Metrology.TH (declareDerivedUnit)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, pack, unpack)
import Data.Units.SI (Ampere(..), Hour(..), Meter(..), Volt(..), Watt(..))
import Data.Units.SI.Prefixes (centi, Milli, milli)
import Data.Units.US (Inch)
import Lucid hiding (for_)
import System.Directory (doesDirectoryExist, createDirectory)
import System.FilePath ((</>), (<.>))
import Text.RawString.QQ (r)

declareDerivedUnit "HorizontalPitch" [t| Inch |] 0.2  (Just "HP")
declareDerivedUnit "RackUnit"        [t| Inch |] 1.75 (Just "U")

mA :: Double -> Current
mA x = x % milli Ampere

v :: Double -> ElectricPotential
v = (% Volt)

data Module = Autobot | M303 | Robokop
            | VScale
            | A100_bl2 | A100_bl4 | A100_bl8 | A100_bl42
            | A101_2 | A103 | A106_6
            | A110_1 | A111_4 | A114 | A115 | A116 | A118 | A119
            | A120 | A124
            | A130 | A131 | A132_3 | A136 | A138a | A138b | A138m | A138s
            | A140 | A143_2 | A143_9 | A145 | A146 | A148
            | A151 | A152 | A156
            | A160 | A160_5 | A161 | A162 | A166
            | A170
            | A180_1 | A180_2 | A180_3 | A182_1 | A184_1 | A185_2
            | A190_4
            | DLD | QCD | QCDExp
            | SubMix
            | DPO | ErbeVerb | Maths | STO
            | Branches | Grids
            | BIA
            | Mixer | Outs
            | Evolution
            | CP909 | Hats808 | One | RS808 | SD808
            | ComplexOscillator | AmplitudeToneController
            | PerformanceMixer
            deriving (Bounded, Enum, Eq, Read, Show)

identifier Autobot = pack "Autobot"
identifier M303 = pack "M303"
identifier Robokop = pack "Robokop"
identifier VScale = pack "VScale"
identifier A100_bl2 = pack "A100-BL2"
identifier A100_bl4 = pack "A100-BL4"
identifier A100_bl8 = pack "A100-BL8"
identifier A100_bl42 = pack "A100-BL42"
identifier A101_2 = pack "A101-2"
identifier A103 = pack "A103"
identifier A106_6 = pack "A106-6"
identifier A110_1 = pack "A110-1"
identifier A111_4 = pack "A111-4"
identifier A114 = pack "A114"
identifier A115 = pack "A115"
identifier A116 = pack "A116"
identifier A118 = pack "A118"
identifier A119 = pack "A119"
identifier A120 = pack "A120"
identifier A124 = pack "A124"
identifier A130 = pack "A130"
identifier A131 = pack "A131"
identifier A132_3 = pack "A132-3"
identifier A136 = pack "A136"
identifier A138a = pack "A138a"
identifier A138b = pack "A138b"
identifier A138m = pack "A138m"
identifier A138s = pack "A138s"
identifier A140 = pack "A140"
identifier A143_2 = pack "A143-2"
identifier A143_9 = pack "A143-9"
identifier A145 = pack "A145"
identifier A146 = pack "A146"
identifier A148 = pack "A148"
identifier A151 = pack "A151"
identifier A152 = pack "A152"
identifier A156 = pack "A156"
identifier A160 = pack "A160"
identifier A160_5 = pack "A160-5"
identifier A161 = pack "A161"
identifier A162 = pack "A162"
identifier A166 = pack "A166"
identifier A170 = pack "A170"
identifier A180_1 = pack "A180-1"
identifier A180_2 = pack "A180-2"
identifier A180_3 = pack "A180-3"
identifier A182_1 = pack "A182-1"
identifier A184_1 = pack "A184-1"
identifier A185_2 = pack "A185-2"
identifier A190_4 = pack "A190-4"
identifier DLD = pack "DLD"
identifier QCD = pack "QCD"
identifier QCDExp = pack "QCDExp"
identifier SubMix = pack "SubMix"
identifier DPO = pack "DPO"
identifier ErbeVerb = pack "ErbeVerb"
identifier Maths = pack "Maths"
identifier STO = pack "STO"
identifier Branches = pack "Branches"
identifier Grids = pack "Grids"
identifier BIA = pack "BIA"
identifier Mixer = pack "Mixer"
identifier Outs = pack "Outs"
identifier Evolution = pack "Evolution"
identifier CP909 = pack "CP909"
identifier Hats808 = pack "Hats808"
identifier One = pack "One"
identifier RS808 = pack "RS808"
identifier SD808 = pack "SD808"
identifier ComplexOscillator = pack "CO"
identifier AmplitudeToneController = pack "AmpTone"
identifier PerformanceMixer = "PerformanceMixer"

name Autobot = pack "Autobot"
name M303 = pack "M303"
name Robokop = pack "Robokop"
name VScale = pack "V-Scale"
name A100_bl2 = pack "A100-bl2"
name A100_bl4 = pack "A100-bl4"
name A100_bl8 = pack "A100-bl8"
name A100_bl42 = pack "A100-bl42"
name A101_2 = pack "A101-2"
name A103 = pack "A103"
name A106_6 = pack "A106_6"
name A110_1 = pack "A110_1"
name A111_4 = pack "A111_4"
name A114 = pack "A114"
name A115 = pack "A115"
name A116 = pack "A116"
name A118 = pack "A118"
name A119 = pack "A119"
name A120 = pack "A120"
name A124 = pack "A124"
name A130 = pack "A130"
name A131 = pack "A131"
name A132_3 = pack "A132_3"
name A136 = pack "A136"
name A138a = pack "A138a"
name A138b = pack "A138b"
name A138m = pack "A138m"
name A138s = pack "A138s"
name A140 = pack "A140"
name A143_2 = pack "A143_2"
name A143_9 = pack "A143-9"
name A145 = pack "A145"
name A146 = pack "A146"
name A148 = pack "A148"
name A151 = pack "A151"
name A152 = pack "A152"
name A156 = pack "A156"
name A160 = pack "A160"
name A160_5 = pack "A160-5"
name A161 = pack "A161"
name A162 = pack "A162"
name A166 = pack "A166"
name A170 = pack "A170"
name A180_1 = pack "A180-1"
name A180_2 = pack "A180-2"
name A180_3 = pack "A180-3"
name A182_1 = pack "A182-1"
name A184_1 = pack "A184-1"
name A185_2 = pack "A185-2"
name A190_4 = pack "A190-4"
name DLD = pack "Dual Looping Delay"
name QCD = pack "Quad Clock Distributor"
name QCDExp = pack "QCD Expander"
name SubMix = pack "SubMix"
name DPO = pack "DPO"
name ErbeVerb = pack "ErbeVerb"
name Maths = pack "Maths"
name STO = pack "STO"
name Branches = pack "Branches"
name Grids = pack "Grids"
name BIA = pack "Basimilus Iteritas Alter"
name Mixer = pack "Mixer"
name Outs = pack "Outs"
name Evolution = pack "Evolution"
name CP909 = pack "CP909"
name Hats808 = pack "Hats808"
name One = pack "One"
name RS808 = pack "RS808"
name SD808 = pack "SD808"
name ComplexOscillator = pack "Complex Oscillator"
name AmplitudeToneController = pack "Amplitude & Tone Controller"
name PerformanceMixer = pack "Performance Mixer"

data Manufacturer = AcidLab
                  | AJHSynth
                  | Döpfer
                  | FourMS
                  | LowGainElectronics
                  | MakeNoise
                  | MutableInstruments
                  | NoiseEngineering
                  | PittsburghModular
                  | RossumEletroMusic
                  | TipTopAudio
                  | VerbosElectronics
                  | WMD
                  deriving (Enum, Eq, Read, Show)

manufacturer Autobot = AcidLab
manufacturer M303 = AcidLab
manufacturer Robokop = AcidLab
manufacturer VScale = AJHSynth
manufacturer A100_bl2 = Döpfer
manufacturer A100_bl4 = Döpfer
manufacturer A100_bl8 = Döpfer
manufacturer A100_bl42 = Döpfer
manufacturer A101_2 = Döpfer
manufacturer A103 = Döpfer
manufacturer A106_6 = Döpfer
manufacturer A110_1 = Döpfer
manufacturer A111_4 = Döpfer
manufacturer A114 = Döpfer
manufacturer A115 = Döpfer
manufacturer A116 = Döpfer
manufacturer A118 = Döpfer
manufacturer A119 = Döpfer
manufacturer A120 = Döpfer
manufacturer A124 = Döpfer
manufacturer A130 = Döpfer
manufacturer A131 = Döpfer
manufacturer A132_3 = Döpfer
manufacturer A136 = Döpfer
manufacturer A138a = Döpfer
manufacturer A138b = Döpfer
manufacturer A138m = Döpfer
manufacturer A138s = Döpfer
manufacturer A140 = Döpfer
manufacturer A143_2 = Döpfer
manufacturer A143_9 = Döpfer
manufacturer A145 = Döpfer
manufacturer A146 = Döpfer
manufacturer A148 = Döpfer
manufacturer A151 = Döpfer
manufacturer A152 = Döpfer
manufacturer A156 = Döpfer
manufacturer A160 = Döpfer
manufacturer A160_5 = Döpfer
manufacturer A161 = Döpfer
manufacturer A162 = Döpfer
manufacturer A166 = Döpfer
manufacturer A170 = Döpfer
manufacturer A180_1 = Döpfer
manufacturer A180_2 = Döpfer
manufacturer A180_3 = Döpfer
manufacturer A182_1 = Döpfer
manufacturer A184_1 = Döpfer
manufacturer A185_2 = Döpfer
manufacturer A190_4 = Döpfer
manufacturer DLD = FourMS
manufacturer QCD = FourMS
manufacturer QCDExp = FourMS
manufacturer SubMix = LowGainElectronics
manufacturer DPO = MakeNoise
manufacturer ErbeVerb = MakeNoise
manufacturer Maths = MakeNoise
manufacturer STO = MakeNoise
manufacturer Branches = MutableInstruments
manufacturer Grids = MutableInstruments
manufacturer BIA = NoiseEngineering
manufacturer Mixer = PittsburghModular
manufacturer Outs = PittsburghModular
manufacturer Evolution = RossumEletroMusic
manufacturer CP909 = TipTopAudio
manufacturer Hats808 = TipTopAudio
manufacturer One = TipTopAudio
manufacturer RS808 = TipTopAudio
manufacturer SD808 = TipTopAudio
manufacturer ComplexOscillator = VerbosElectronics
manufacturer AmplitudeToneController = VerbosElectronics
manufacturer PerformanceMixer = WMD

description :: Module -> Text
description A180_3 = "Dual Buffered Multiple"

isBlindPanel :: Module -> Bool
isBlindPanel A100_bl2 = True
isBlindPanel A100_bl4 = True
isBlindPanel A100_bl8 = True
isBlindPanel A100_bl42 = True
isBlindPanel _ = False

width :: Module -> Length
width Autobot = 50 % HorizontalPitch
width M303 = 16 % HorizontalPitch
width Robokop = 50 % HorizontalPitch
width VScale = 4 % HorizontalPitch
width A100_bl2 = 2 % HorizontalPitch
width A100_bl4 = 4 % HorizontalPitch
width A100_bl8 = 8 % HorizontalPitch
width A100_bl42 = 42 % HorizontalPitch
width A101_2 = 8 % HorizontalPitch
width A103 = 8 % HorizontalPitch
width A106_6 = 12 % HorizontalPitch
width A110_1 = 10 % HorizontalPitch
width A111_4 = 18 % HorizontalPitch
width A114 = 4 % HorizontalPitch
width A115 = 8 % HorizontalPitch
width A116 = 8 % HorizontalPitch
width A118 = 8 % HorizontalPitch
width A119 = 8 % HorizontalPitch
width A120 = 8 % HorizontalPitch
width A124 = 8 % HorizontalPitch
width A130 = 8 % HorizontalPitch
width A131 = 8 % HorizontalPitch
width A132_3 = 8 % HorizontalPitch
width A136 = 8 % HorizontalPitch
width A138a = 8 % HorizontalPitch
width A138b = 8 % HorizontalPitch
width A138m = 20 % HorizontalPitch
width A138s = 8 % HorizontalPitch
width A140 = 8 % HorizontalPitch
width A143_2 = 26 % HorizontalPitch
width A143_9 = 8 % HorizontalPitch
width A145 = 8 % HorizontalPitch
width A146 = 8 % HorizontalPitch
width A148 = 4 % HorizontalPitch
width A151 = 4 % HorizontalPitch
width A152 = 16 % HorizontalPitch
width A156 = 8 % HorizontalPitch
width A160 = 4 % HorizontalPitch
width A160_5 = 4 % HorizontalPitch
width A161 = 4 % HorizontalPitch
width A162 = 8 % HorizontalPitch
width A166 = 8 % HorizontalPitch
width A170 = 8 % HorizontalPitch
width A180_1 = 4 % HorizontalPitch
width A180_2 = 2 % HorizontalPitch
width A180_3 = 4 % HorizontalPitch
width A182_1 = 6 % HorizontalPitch
width A184_1 = 4 % HorizontalPitch
width A185_2 = 6 % HorizontalPitch
width A190_4 = 10 % HorizontalPitch
width DLD = 20 % HorizontalPitch
width QCD = 10 % HorizontalPitch
width QCDExp = 12 % HorizontalPitch
width SubMix = 28 % HorizontalPitch
width DPO = 28 % HorizontalPitch
width ErbeVerb = 20 % HorizontalPitch
width Maths = 20 % HorizontalPitch
width STO = 8 % HorizontalPitch
width Branches = 6 % HorizontalPitch
width Grids = 16 % HorizontalPitch
width BIA = 10 % HorizontalPitch
width Mixer = 8 % HorizontalPitch
width Outs = 6 % HorizontalPitch
width Evolution = 16 % HorizontalPitch
width CP909 = 4 % HorizontalPitch
width Hats808 = 8 % HorizontalPitch
width One = 4 % HorizontalPitch
width RS808 = 4 % HorizontalPitch
width SD808 = 4 % HorizontalPitch
width ComplexOscillator = 32 % HorizontalPitch
width AmplitudeToneController = 16 % HorizontalPitch
width PerformanceMixer = 40 % HorizontalPitch

height :: Module -> Length
height _ = 3 % RackUnit

data Currents = Currents Current Current Current deriving (Eq, Show)

instance Monoid Currents where
  mempty = Currents (mA 0) (mA 0) (mA 0)
  (Currents a1 a2 a3) `mappend` (Currents b1 b2 b3) =
    Currents (a1 |+| b1) (a2 |+| b2) (a3 |+| b3)

currents :: Module -> Currents
currents Autobot = Currents (mA 80) (mA 20) (mA 0)
currents M303 = Currents (mA 60) (mA 20) (mA 0)
currents Robokop = Currents (mA 80) (mA 20) (mA 0)
currents VScale = Currents (mA 15) (mA 15) (mA 0)
currents A100_bl2 = mempty
currents A100_bl4 = mempty
currents A100_bl8 = mempty
currents A100_bl42 = mempty
currents A101_2 = Currents (mA 20) (mA 20) (mA 0)
currents A103 = Currents (mA 30) (mA 10) (mA 0)
currents A106_6 = Currents (mA 50) (mA 50) (mA 0)
currents A110_1 = Currents (mA 90) (mA 20) (mA 0)
currents A111_4 = Currents (mA 120) (mA 100) (mA 0)
currents A114 = Currents (mA 40) (mA 30) (mA 0)
currents A115 = Currents (mA 20) (mA 10) (mA 0)
currents A116 = Currents (mA 20) (mA 20) (mA 0)
currents A118 = Currents (mA 20) (mA 10) (mA 0)
currents A119 = Currents (mA 30) (mA 20) (mA 0)
currents A120 = Currents (mA 30) (mA 20) (mA 0)
currents A124 = Currents (mA 30) (mA 10) (mA 0)
currents A130 = Currents (mA 20) (mA 10) (mA 0)
currents A131 = Currents (mA 20) (mA 10) (mA 0)
currents A132_3 = Currents (mA 30) (mA 30) (mA 0)
currents A136 = Currents (mA 30) (mA 10) (mA 0)
currents A138a = Currents (mA 10) (mA 10) (mA 0)
currents A138b = Currents (mA 10) (mA 10) (mA 0)
currents A138m = Currents (mA 30) (mA 30) (mA 0)
currents A138s = Currents (mA 10) (mA 10) (mA 0)
currents A140 = Currents (mA 20) (mA 20) (mA 0)
currents A143_2 = Currents (mA 70) (mA 50) (mA 0)
currents A143_9 = Currents (mA 30) (mA 30) (mA 0)
currents A145 = Currents (mA 30) (mA 20) (mA 0)
currents A146 = Currents (mA 20) (mA 20) (mA 0)
currents A148 = Currents (mA 20) (mA 20) (mA 0)
currents A151 = Currents (mA 20) (mA 10) (mA 0)
currents A152 = Currents (mA 40) (mA 20) (mA 0)
currents A156 = Currents (mA 50) (mA 10) (mA 0)
currents A160 = Currents (mA 40) (mA 0) (mA 0)
currents A160_5 = Currents (mA 50) (mA 0) (mA 0)
currents A161 = Currents (mA 20) (mA 0) (mA 0)
currents A162 = Currents (mA 40) (mA 0) (mA 0)
currents A166 = Currents (mA 40) (mA 20) (mA 0)
currents A170 = Currents (mA 20) (mA 20) (mA 0)
currents A180_1 = mempty
currents A180_2 = mempty
currents A180_3 = Currents (mA 20) (mA 20) (mA 0)
currents A182_1 = mempty
currents A184_1 = Currents (mA 40) (mA 30) (mA 0)
currents A185_2 = Currents (mA 10) (mA 10) (mA 0)
currents A190_4 = Currents (mA 200) (mA 40) (mA 0)
currents DLD = Currents (mA 188) (mA 48) (mA 0)
currents QCD = Currents (mA 48) (mA 40) (mA 41)
currents QCDExp = Currents (mA 44) (mA 30) (mA 0)
currents SubMix = Currents (mA 15) (mA 15) (mA 0)
currents DPO = Currents (mA 70) (mA 70) (mA 0)
currents ErbeVerb = Currents (mA 148) (mA 15) (mA 0)
currents Maths = Currents (mA 60) (mA 50) (mA 0)
currents STO = Currents (mA 40) (mA 30) (mA 0)
currents Branches = Currents (mA 10) (mA 1) (mA 0)
currents Grids = Currents (mA 25) (mA 1) (mA 0)
currents BIA = Currents (mA 80) (mA 5) (mA 90)
currents Mixer = Currents (mA 30) (mA 30) (mA 0)
currents Outs = Currents (mA 50) (mA 50) (mA 0)
currents Evolution = Currents (mA 85) (mA 75) (mA 0)
currents CP909 = Currents (mA 6) (mA 10) (mA 0)
currents Hats808 = Currents (mA 30) (mA 25) (mA 0)
currents One = Currents (mA 80) (mA 8) (mA 0)
currents RS808 = Currents (mA 14) (mA 13) (mA 0)
currents SD808 = Currents (mA 18) (mA 16) (mA 0)
currents ComplexOscillator = Currents (mA 70) (mA 50) (mA 0)
currents AmplitudeToneController = Currents (mA 30) (mA 20) (mA 0)
currents PerformanceMixer = Currents (mA 450) (mA 430) (mA 0)

powerOfCurrents :: Currents -> Power
powerOfCurrents (Currents p12 m12 p5) =
  v 12 |*| p12 |+| v 12 |*| m12 |+| v 5 |*| p5

data FrontPanel e = UnknownPanel
                  | Blank
                  | Tabular [[Maybe e]]
                  | ASCIILayoutDiagram Text [e]
                  deriving (Foldable, Functor, Eq)

data FrontPanelElement = Button
                       | LED
                       | Rotary
                       | SDSlot
                       | Socket Direction SocketType
                       | Switch [Text]
                       | RotarySwitch [Text]
                       deriving (Eq)

data Direction = In | Out | InOrOut deriving (Eq)
data SocketType = SocketType Length Contacts Channels deriving (Eq)
data Channels = Mono | Stereo deriving (Bounded, Enum, Eq)
data Contacts = TS | TRS deriving (Bounded, Enum, Eq)

type Label = Text

type Panel = FrontPanel (Label, FrontPanelElement)

frontPanel Autobot = ASCIILayoutDiagram [r|
                                                        CV Gate Accent
   Tempo    Prog     Mode        Slide

       Roll/Scale  Length/Shuffle     - -   - - -        Oct- Oct+ Clear
Start/Stop           Select          - - - - - - - -    Slide Acc Write/Next
|] [ ("CV", Socket Out mini)
   , ("Gate", Socket Out mini)
   , ("Accent", Socket Out mini)
   , ("Tempo", Rotary)
   , ("Prog", RotarySwitch $ map (pack . show) [1..12])
   , ("Mode", RotarySwitch [ "Track write", "Track play", "Pattern play",
                             "Tap write", "Step write", "Pattern play" ])
   , ("Slide", Rotary)
   , ("Roll/Scale", Button)
   , ("Length/Shuffle", Button)
   , ("C#", Button)
   , ("D#", Button)
   , ("F#", Button)
   , ("G#", Button)
   , ("A#", Button)
   , ("Oct-", Button)
   , ("Oct+", Button)
   , ("Clear", Button)
   , ("Start/Stop", Button)
   , ("Select", Button)
   , ("C", Button)
   , ("D", Button)
   , ("E", Button)
   , ("F", Button)
   , ("G", Button)
   , ("A", Button)
   , ("B", Button)
   , ("C", Button)
   , ("Slide", Button)
   , ("Acc", Button)
   , ("Write/Next", Button)
   ]
frontPanel M303 = ASCIILayoutDiagram [r|
CV-In   Slide-In   Tune    VCO-Out
              Saw/Sq
VCO-CV  VCO-Mod    VCF-Mod VCF-CV

VCF-In  Cutoff     Res     VCA-In

Gate-In Env-Mod    Decay   Env-Out
             Gate/VCA
Acc-In  Acc        Volume  Out
|] []
frontPanel Robokop = ASCIILayoutDiagram [r|
 b      c       d                                     e           f

 a    o   o   o   o   o   o   o   o   o   o   o   o   o   o   o   o

a=Start/Stop
b=Scale
c=Inst./Sel.
d=Leng./Shuf.
e=Clear
f=Write/Next
|] []
frontPanel VScale = UnknownPanel
frontPanel A100_bl2 = Blank
frontPanel A100_bl4 = Blank
frontPanel A100_bl8 = Blank
frontPanel A100_bl42 = Blank
frontPanel A101_2 = ASCIILayoutDiagram [r|
CV-In1    F/A  ,
CV-In2    CV2
AudioIn   Lev.
AudioOut  Res.
G1        LP/VCA
G2

------------------------
G1    H  L   L
G2    L  H   L
Func. LP VCA L+V
|] []
frontPanel A103 = UnknownPanel
frontPanel A106_6 = ASCIILayoutDiagram [r|
                        3A-1L
Audio-In       Lev.
                        2N-1L

FCV1           Frq.     4B

                        3H-1L
FCV2           FCV
                        2H-1L

QCV            QCV      2B

                        4L
Filtergrp      Q
                        2L
|] []
frontPanel A110_1 = ASCIILayoutDiagram [r|
SYNC     Range
CV1      Tune
CV2      CV2
PW-CV1   PW
PW-CV2   PW-CV2
Saw  Sqr  Tri  Sin
|] [
    ("Sync", Socket In mini)
  , ("Range", RotarySwitch $ map (pack . show) [0..9])
  , ("CV1", Socket In mini)
  , ("Tune", Rotary)
  , ("CV2", Socket In mini)
  , ("CV2", Rotary)
  , ("PWCV1", Socket In mini)
  , ("PW", Rotary)
  , ("PWCV2", Socket In mini)
  , ("PWCV2", Rotary)
  , ("Saw", Socket Out mini)
  , ("Sqr", Socket Out mini)
  , ("Tri", Socket Out mini)
  , ("Sin", Socket Out mini)
  ]
frontPanel A111_4 = ASCIILayoutDiagram [r|
1V  +/-                          Tri  Sq
        Tune         ModLevel
Mod           XM/LM|PM           Saw  Snc

1V  +/-                          Tri  Sq
        Tune         ModLevel
Mod           XM/LM|PM           Saw  Snc

1V  +/-                          Tri  Sq
        Tune         ModLevel
Mod           XM/LM|PM           Saw  Snc

1V  +/-                          Tri  Sq
        Tune         ModLevel
Mod           XM/LM|PM           Saw  Snc

1V  +/-                          Tri  Sq
        Tune         ModLevel
XM                               Saw  CVOut
|] []
frontPanel A114 = let g = [ [Just ("X-In", Socket In mini)]
                          , [Just ("Y-In", Socket In mini)]
                          , [Just ("X*Y-Out", Socket Out mini)]
                          ]
                  in Tabular $ g <> [[Nothing]] <> g
frontPanel A115 = Tabular [
    [Just ("In", Socket In mini), Just ("Orig.", Rotary)]
  , [Nothing, Just ("f/2", Rotary)]
  , [Nothing, Just ("f/4", Rotary)]
  , [Nothing, Just ("f/8", Rotary)]
  , [Just ("Out", Socket Out mini), Just ("f/16", Rotary)]
  ]
frontPanel A116 = ASCIILayoutDiagram [r|
Audio-In        Lev.
                Clipping-Level
Clipping-CV     CCV
Symm.-CV        SCV
Audio-Out       Sym
|] []
frontPanel A118 = ASCIILayoutDiagram [r|
White           Blue
Colored         Red
                Rate
                Lev.
Random-Output   , ,
|] []
frontPanel A119 = ASCIILayoutDiagram [r|
Asym.In       Symm.In
Audio-Out     Gain
Audio-Out     ,
Envelope-Out  ,
              ,
Gate-Out      Thresh.
|] []
frontPanel A120 = ASCIILayoutDiagram [r|
CV1             Frq.
CV2             CV2
CV3             CV3
AudioIn         Lev.
AudioOut        Res.
|] []
frontPanel A124 = ASCIILayoutDiagram [r|
  AudioIn  Lev.
  CV1      Frq.
  CV2      CV2
  BPOut    Res.
  LP/HPOut Mix
|] []
frontPanel A130 = ASCIILayoutDiagram [r|
CV1        Gain
CV2        CV2
AudioIn1   In1
AudioIn2   In2
AudioOut   Out
|] []
frontPanel A131 = frontPanel A130
frontPanel A132_3 = ASCIILayoutDiagram [r|
CV-In   CV
In      Gain
Out     lin./exp.
CV-In   CV
In      Gain
Out     lin./exp.
|] []
frontPanel A136 = ASCIILayoutDiagram [r|
Input           +A
Ext.Level       +L
                A
Ext.Level       -L
Output          -A
|] []
frontPanel A138a = ASCIILayoutDiagram [r|
Input1  In1
Input2  In2
Input3  In3
Input4  In4
Output  Out
|] []
frontPanel A138b = frontPanel A138a
frontPanel A138m = UnknownPanel
frontPanel A138s = UnknownPanel
frontPanel A140 = ASCIILayoutDiagram [r|
Gate            A
Retrig.         D
Output          S
Output          R
Inv.Output      TimeRange       ,
|] []
frontPanel A143_2 = ASCIILayoutDiagram [r|
RT   A   D
                Attack Decay Sutain Release   Out
G    R  m/l/h

RT   A   D
                Attack Decay Sutain Release   Out
G    R  m/l/h

RT   A   D
                Attack Decay Sutain Release   Out
G    R  m/l/h

RT   A   D
                Attack Decay Sutain Release   Out
G    R  m/l/h
|] []
frontPanel A143_9 = ASCIILayoutDiagram [r|
CV1    Frq.
CV2    CV2
       Range
0°     ,
       ,
90°
180°
270°
|] []
frontPanel A145 = UnknownPanel
frontPanel A146 = ASCIILayoutDiagram [r|
        Freq.
        Freq.Range
        WaveForm
SQ      ,
Sq(positiv!)
TRI     ,
|] []
frontPanel A148 = ASCIILayoutDiagram [r|
Trig.In
Smp.In
S&H-Out
, ,
Trig.In
Smp.In
S&H-Out
, ,
|] []
frontPanel A151 = Tabular [
    [Nothing, Just ("Trig.In", Socket In mini)]
  , [Nothing, Just ("Res.In", Socket In mini)]
  , [Nothing, Just ("O/I", Socket InOrOut mini)]
  , [Just ("", LED), Just ("I/O1", Socket InOrOut mini)]
  , [Just ("", LED), Just ("I/O2", Socket InOrOut mini)]
  , [Just ("", LED), Just ("I/O3", Socket InOrOut mini)]
  , [Just ("", LED), Just ("I/O4", Socket InOrOut mini)]
  , [Nothing, Just ("Steps", Switch [])]
  ]
frontPanel A152 = UnknownPanel
frontPanel A156 = ASCIILayoutDiagram [r|
CV.In    CV.Out
Trig.In  Trig.Out
CV.In    CV.Out
Trig.In  Trig.Out

         All/Maj/Minor
         Scale/Chord/Quint
         -/+7/+6

Transpose.CV.In1+2
|] []
frontPanel A160 = Tabular [
    [Nothing, Just ("Trig.In", Socket In mini)]
  , [Nothing, Just ("Res.In", Socket In mini)]
  , [Just ("", LED), Just ("/2", Socket Out mini)]
  , [Just ("", LED), Just ("/4", Socket Out mini)]
  , [Just ("", LED), Just ("/8", Socket Out mini)]
  , [Just ("", LED), Just ("/16", Socket Out mini)]
  , [Just ("", LED), Just ("/32", Socket Out mini)]
  , [Just ("", LED), Just ("/64", Socket Out mini)]
  ]
frontPanel A160_5 = ASCIILayoutDiagram [r|
     Divider-Set
     ,
     ,
     ,
     ,
     ,
     ,
     ,
     ,
     Manual
     CV-In
  ,  Clock-In
  ,  Clock-Out
  

1 2 3 4 5 6 7 8 / 1 2 4 8 16 16 16 16 / 1 2 3 4 6 8 12 16
|] []
frontPanel A161 = Tabular [
    [Just ("", LED), Just ("1", Socket Out mini)]
  , [Just ("", LED), Just ("2", Socket Out mini)]
  , [Just ("", LED), Just ("3", Socket Out mini)]
  , [Just ("", LED), Just ("4", Socket Out mini)]
  , [Just ("", LED), Just ("5", Socket Out mini)]
  , [Just ("", LED), Just ("6", Socket Out mini)]
  , [Just ("", LED), Just ("7", Socket Out mini)]
  , [Just ("", LED), Just ("8", Socket Out mini)]
  ]
frontPanel A162 = UnknownPanel
frontPanel A166 = Tabular [
    [Just ("1", Socket In mini), Just ("AND", Socket In mini)]
  , [Just ("2", Socket In mini), Just ("OR", Socket In mini)]
  , [Just ("3", Socket In mini), Just ("XOR", Socket In mini)]
  , [Just ("1", Socket In mini), Just ("AND", Socket In mini)]
  , [Just ("2", Socket In mini), Just ("OR", Socket In mini)]
  , [Just ("3", Socket In mini), Just ("XOR", Socket In mini)]
  , [Just ("In", Socket In mini), Just ("Inv", Socket In mini)]
  , [Just ("In", Socket In mini), Just ("Inv", Socket In mini)]
  ]
frontPanel A170 = ASCIILayoutDiagram [r|
In              Time
Out             , ,
In              CURSOR_UP
Time-Range      CURSOR_DOWN
Out             , ,
|] []
frontPanel A180_1 = UnknownPanel
frontPanel A180_2 = UnknownPanel
frontPanel A180_3 = UnknownPanel
frontPanel A182_1 = UnknownPanel
frontPanel A184_1 = UnknownPanel
frontPanel A185_2 = UnknownPanel
frontPanel A190_4 = ASCIILayoutDiagram [r|
     *FettesLCDDisplay*
</-     >/+     Back    Enter
Clock           USB
Reset           Midi-In
Gate
CV1     CV2     Midi-Thru
|] []
frontPanel DLD = ASCIILayoutDiagram [r|
Time     +16/=/(1/8)  Ping        Ping        +16/=/(1/8) Time
              LoopA        ClockOut      LoopB
Feedback Reverse      Hold        Hold        Reverse     Feedback
DelayFeed   Mix                               Mix         DelayFeed
InA      ReturnA      RevA        RevB        ReturnB     InB
OutA     SendA        HoldA       HoldB       SendB       OutB
TimeA    FeedbackA    DelayFeedA  DelayFeedB  FeedbackB   TimeB
|] []
frontPanel QCD = ASCIILayoutDiagram [r|
Tap       Tap-Out
                       CLK-In
          Div/Mult-CV  CLK-Out
Div/Mult  Reset
                       CLK-In
          DIV/Mult-CV  CLK-Out
Div/Mult  Reset
                       CLK-In
          DIV/Mult-CV  CLK-Out
Div/Mult  Reset
                       CLK-In
          DIV/Mult-CV  CLK-Out
Div/Mult  Reset
|] []
frontPanel QCDExp = ASCIILayoutDiagram [r|
CV                     Mode
    CV-Att  Gate-PW
Inv                    Div/Mult-Att

CV                     Mode
    CV-Att  Gate-PW
Inv                    Div/Mult-Att

CV                     Mode
    CV-Att  Gate-PW
Inv                    Div/Mult-Att

CV                     Mode
    CV-Att  Gate-PW
Inv                    Div/Mult-Att
|] []
frontPanel SubMix = let row c = map (\n -> Just (c <> pack (show n), Rotary)) [1..3]
                             <> [Nothing]
			     <> map (\n -> Just (c <> pack (show n), Socket In mini)) [1..3]
			     <> [Just (c, Socket Out mini)]
                        mix x = replicate 7 Nothing <> [Just (x, Socket Out mini)]
	            in Tabular [
    row "A"
  , mix "AB"
  , row "B"
  , mix "All"
  , row "C"
  , mix "CD"
  , row "D"
  ]
frontPanel DPO = UnknownPanel
frontPanel ErbeVerb = ASCIILayoutDiagram [r|
 In    In    Mix      Dry/Wet        LOut
                                     ROut

 Rev   Size    Speed      PreDelay   Absort

 Rev           Depth      Tilt       Decay
        -+
Tempo          -+         -+         -+

CVOut  Size Speed Depth Pre Tilt Absorb Decay
|] []
frontPanel Maths = ASCIILayoutDiagram [r|
In1 Trig1  In2  In3  Trig4 In4

Cycle  Rise
Rise   Fall
Both   Exp/Lin/Log
Fall
Cycle

       ,  ,  1 2 3 4   ,  ,
EOR Unity , Or Sum Inv , Unity EOC
|] []
frontPanel STO = UnknownPanel
frontPanel Branches = ASCIILayoutDiagram [r|
     P  T

  In      P
  OutA ,  OutB

     P  T

  In      P
  OutA ,  OutB
|] []
frontPanel Grids = ASCIILayoutDiagram [r|
Clock  Clock
Reset  Tap/Reset
Map-X  Map-X
Map-Y  Map-Y   Fill1 Fill2 Fill3
Chaos  Chaos   Trig1 Trig2 Trig3
               Acc1 Acc2 Acc3
|] []
frontPanel BIA = ASCIILayoutDiagram [r|
Pitch            Attack
         Morph

Spread           Decay
         Fold

Harmonic          Skin/Liquid/Metal

                  Bass/Alto/Treble
         ,

Pitch         Attack   S/L/M
Spread  Morph  Decay   B/A/T
Harm    Fold    Trig    Out
|] []
frontPanel Mixer = ASCIILayoutDiagram [r|
In1
      Level1
Out1
In2
      Level2
Out2
In3
      Level3
Out3
Int4
      Level4
MixOut
|] []
frontPanel Outs = UnknownPanel
frontPanel Evolution = ASCIILayoutDiagram [r|
Freq     Q       Genus    Species
FCV3   QLevComp
FCV3
FCV2    QCV2     GCV2     SCV2
FCV2    QCV2     GCV2     SCV2
1V      QCV1     GCV1     SCV1
In                        Out
|] []
frontPanel CP909 = UnknownPanel
frontPanel Hats808 = ASCIILayoutDiagram [r|
 Level     Level
 Q         VCQ
 Decay     BPOut
 Accent    Accent
 AccentIn  AccentIn
 GateIn    GateIn
 OHOut     CHOut
|] []
frontPanel One = ASCIILayoutDiagram [r|
Lev.
Play

File
Pitch
CV
Gate-In
Out
|] []
frontPanel RS808 = UnknownPanel
frontPanel SD808 = Tabular $ map (map Just) [
    [("Level", Rotary)]
  , [("Tone", Rotary)]
  , [("Snappy", Rotary)]
  , [("Accent", Rotary)]
  , [("Accent-In", Socket In mini)]
  , [("Gate-In", Socket In mini)]
  , [("SD-Out", Socket Out mini)]
  ]
frontPanel ComplexOscillator = ASCIILayoutDiagram [r|
, t sq s  sync   t sq s out
          am/fm           e/o  av
      f   t/sq/s       f       cv
  mod       i      car    low  av
                               cv
 fm cv      i   fm cv     tmbr av
 fm cv 1v   i   fm cv 1v       cv
|] []
frontPanel AmplitudeToneController = ASCIILayoutDiagram [r|
Gain
Cutoff       Amp
Resonance
    FreqCV  ExpCV  LinCV
In  FreqCV  ExpCV  LinCV  Out
|] []
frontPanel PerformanceMixer = UnknownPanel

type Row = [Module]
type Case = [Row]
type System = [Case]

rowCurrents = mconcat . map currents
caseCurrents = mconcat . map rowCurrents
systemCurrents = mconcat . map caseCurrents

rowHeight = maximum . map height
rowWidth = qSum . map width
caseHeight = qSum . map rowHeight
caseWidth = maximum . map rowWidth
systemArea = qSum . map (\c -> caseWidth c |*| caseHeight c)

showCaseSize x = let w = caseWidth x
                     h = caseHeight x
                 in show (round (w # HorizontalPitch)) ++ " " ++
                    show HorizontalPitch ++ " × " ++
                    show (round (h # RackUnit)) ++ " " ++
                    show RackUnit ++ " (" ++ (w `showIn` centi Meter) ++ ")"

fullName :: Module -> Html ()
fullName m = toHtml $ manufacturerName (manufacturer m) <> pack " " <> name m

showAsIntegralIn x u = show (round $ x # u) <> show u

instance ToHtml Currents where
  toHtml (Currents p12 m12 p5) = toHtml $
    p12 `showAsIntegralIn` milli Ampere <> " @ +12V, " <>
    m12 `showAsIntegralIn` milli Ampere <> " @ -12V, " <>
    p5 `showAsIntegralIn` milli Ampere <> " @ +5V"
  toHtmlRaw = toHtml

moduleHtml :: Module -> Html ()
moduleHtml m = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ $ fullName m
  body_ $ do
    h1_ $ fullName m
    dl_ $ do
      dt_ "Width"
      dd_ $ toHtml $ width m `showAsIntegralIn` HorizontalPitch
      dt_ "Currents"
      dd_ $ toHtml $ currents m
      dt_ "Manufacturer"
      dd_ $ manufacturerLink $ manufacturer m
    h2_ "Front panel"
    panelHtml $ frontPanel m
    when (hasSwitchPositionLabels m) $ do
      h3_ "Labels of switch positions and rotary detents"
      describeSwitches m
 
panelHtml Blank = p_ "Blank (blind) front panel."
panelHtml UnknownPanel = p_ "Panel layout unknown."
panelHtml (ASCIILayoutDiagram layout _) = pre_ $ toHtml layout
panelHtml (Tabular rows) = table_ $ for_ rows $ tr_ . traverse_ cell where
  cell :: Maybe (Label, FrontPanelElement) -> Html ()
  cell Nothing = td_ mempty
  cell (Just ("", LED)) = td_ "LED"
  cell (Just (label, _)) = td_ $ toHtml label

hasSwitchPositionLabels = not . null . concatMap labels . frontPanel where
  labels (_, Switch x) = x
  labels (_, RotarySwitch x) = x
  labels _ = []

describeSwitches = dl_ . mconcat . toList . fmap desc . frontPanel where
  desc :: (Label, FrontPanelElement) -> Html ()
  desc (n, RotarySwitch x) = do
    dt_ $ toHtml $ n <> pack " (rotary switch, clockwise)"
    dd_ $ toHtml $ intercalate ", " x
  desc _ = mempty

systemHtml :: System -> Html ()
systemHtml sys = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Foobarion"
  body_ $ do
    dl_ $ do
      dt_ "Power"
      dd_ $ do
        toHtml (systemCurrents sys)
        toHtml $ pack " ("
        toHtml (powerOfCurrents (systemCurrents sys) `showAsIntegralIn` Watt)
        toHtml $ pack ")"
    for_ sys $ \c -> do
      p_ $ toHtml $ showCaseSize c
      table_ [class_ "case"] $ traverse_ row c
    h2_ "Known but unused modules"
    ul_ $ for_ (unused $ modules sys) $ li_ . toHtml . fullName
 where
  row :: Row -> Html ()
  row = tr_ . traverse_ cell
  cell :: Module -> Html ()
  cell m = td_ [colspan_ $ pack $ show $ round $ width m # HorizontalPitch] $
    unless (isBlindPanel m) $
      a_ [href_ $ identifier m <> ".html"] (toHtml $ identifier m)

modules :: System -> [Module]
modules = nub . filter (not . isBlindPanel) . concatMap concat

renderToDirectory :: FilePath -> System -> IO ()
renderToDirectory dir sys = do
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  let pages = ("index", systemHtml sys) :
              map (\mod -> (unpack $ identifier mod, moduleHtml mod))
                  (modules sys)
  forConcurrently_ pages $ \(base, html) ->
    renderToFile (dir </> base <.> "html") html

mini = SocketType (3.5 % milli Meter) TS Mono

balanced (SocketType _ TRS Mono) = True
balanced _ = False

unbalanced = not . balanced

manufacturerUrl Döpfer = Just "http://www.doepfer.de/"
manufacturerUrl _ = Nothing

manufacturerName AcidLab = pack "AcidLab"
manufacturerName AJHSynth = pack "AJH Synth"
manufacturerName Döpfer = pack "Döpfer"
manufacturerName FourMS = pack "4ms"
manufacturerName LowGainElectronics = pack "Low-Gain Electronics"
manufacturerName MakeNoise = pack "Make Noise"
manufacturerName MutableInstruments = pack "Mutable Instruments"
manufacturerName NoiseEngineering = pack "Noise Engineering"
manufacturerName PittsburghModular = pack "Pittsburgh Modular"
manufacturerName RossumEletroMusic = pack "Rossum Eletro-Music"
manufacturerName TipTopAudio = pack "Tiptop Audio"
manufacturerName VerbosElectronics = pack "Verbos Electronics"
manufacturerName WMD = pack "WMD"

instance ToHtml Manufacturer where
  toHtml = toHtml . manufacturerName
  toHtmlRaw = toHtmlRaw . manufacturerName

manufacturerLink :: Manufacturer -> Html ()
manufacturerLink m =
  maybe name (\url -> a_ [href_ $ pack url] name) $ manufacturerUrl m where
    name = toHtml m

unused = filter (not . isBlindPanel) . ([minBound .. maxBound] \\)
