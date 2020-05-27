{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, QuasiQuotes, OverloadedStrings, TypeFamilies, TypeOperators #-}
module Eurorack.Modules (
  Module(..), HorizontalPitch(..), RackUnit(..), Currents(..), synopsis, width, currents, fullName, Row, Case(..), System, identifier, frontPanel, panelHtml, systemHtml, hasSwitchPositionLabels, describeSwitches, frontPanelHtml, name, height, description
) where
import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.Foldable (for_, maximum, sum, toList, traverse_)
import Data.List (nub, (\\))
import Data.Metrology ((%), (#), (|+|), (|*|), (:@), qSum)
import Data.Metrology.Poly (showIn)
import Data.Metrology.Show ()
import Data.Metrology.SI (Current, ElectricPotential, Power, Length)
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text, intercalate, pack, unpack)
import Data.Units.SI (Ampere(..), Hour(..), Meter(..), Volt(..), Watt(..))
import Data.Units.SI.Prefixes (centi, Milli, milli)
import Data.Yaml
import Eurorack.Units
import GHC.Generics
import Lucid hiding (for_)
import System.FilePath ((</>), (<.>))
import Text.RawString.QQ (r)


mA :: Double -> Current
mA x = x % milli Ampere

v :: Double -> ElectricPotential
v = (% Volt)

data Module = TM | Pluck
            | Autobot | M303 | Robokop
            | VScale
            | Salt | SaltPlus
            | DUSeq
            | A100_bl2 | A100_bl4 | A100_bl8 | A100_bl42
            | A101_2 | A103 | A106_6
            | A110_1 | A111_4 | A114 | A115 | A116 | A118 | A119
            | A120 | A124
            | A130 | A131 | A132_3 | A136 | A138a | A138b | A138m | A138s
            | A140 | A143_2 | A143_9 | A145 | A146 | A148
            | A151 | A152 | A156
            | A160 | A160_5 | A161 | A162 | A166
            | A170 | A177_2
            | A180_1 | A180_2 | A180_3 | A182_1 | A184_1 | A185_2
            | A190_4
            | DLD | QCD | QCDExp
            | UnityMixer
            | SubMix
            | DPO | ErbeVerb | Maths | STO
            | Branches | Grids
            | BIA
            | Mixer | Outs
            | Evolution
            | BD808 | BD909 | CP909 | Hats808 | One | RS808 | SD808
            | CO | ATC
            | Compressor | Fracture | PerformanceMixer
            deriving (Bounded, Enum, Eq, Read, Show)

identifier :: Module -> Text
identifier SaltPlus = "Salt+"
identifier A100_bl2 = "A100-BL2"
identifier A100_bl4 = "A100-BL4"
identifier A100_bl8 = "A100-BL8"
identifier A100_bl42 = "A100-BL42"
identifier A101_2 = "A101-2"
identifier A106_6 = "A106-6"
identifier A110_1 = "A110-1"
identifier A111_4 = "A111-4"
identifier A132_3 = "A132-3"
identifier A143_2 = "A143-2"
identifier A143_9 = "A143-9"
identifier A160_5 = "A160-5"
identifier A177_2 = "A177-2"
identifier A180_1 = "A180-1"
identifier A180_2 = "A180-2"
identifier A180_3 = "A180-3"
identifier A182_1 = "A182-1"
identifier A184_1 = "A184-1"
identifier A185_2 = "A185-2"
identifier A190_4 = "A190-4"
identifier mod = pack . show $ mod

instance FromJSON Module where
  parseJSON = withText "Module" $ \s ->
    let failed = fail $ "expected Module, encountered " <> unpack s
        check mod = [mod | identifier mod == s]
    in foldr ((<|>) . check) failed [minBound .. maxBound] where

name VScale = "V-Scale"
name DUSeq = "DU-Seq"
name DLD = "Dual Looping Delay"
name QCD = "Quad Clock Distributor"
name QCDExp = "QCD Expander"
name BIA = "Basimilus Iteritas Alter"
name CO = "Complex Oscillator"
name ATC = "Amplitude & Tone Controller"
name PerformanceMixer = "Performance Mixer"
name mod = identifier mod

data Manufacturer = TwoHP
                  | AcidLab
                  | AJHSynth
                  | Bela
                  | DetroitUnderground
                  | Döpfer
                  | FourMS
                  | IntellijelDesigns
                  | LowGainElectronics
                  | MakeNoise
                  | MutableInstruments
                  | NoiseEngineering
                  | PittsburghModular
                  | RebelTechnologies
                  | RossumEletroMusic
                  | TipTopAudio
                  | VerbosElectronics
                  | WMD
                  deriving (Enum, Eq, Read, Show)

manufacturer Pluck = TwoHP
manufacturer TM = TwoHP
manufacturer Autobot = AcidLab
manufacturer M303 = AcidLab
manufacturer Robokop = AcidLab
manufacturer VScale = AJHSynth
manufacturer Salt = RebelTechnologies
manufacturer SaltPlus = RebelTechnologies
manufacturer DUSeq = DetroitUnderground
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
manufacturer A177_2 = Döpfer
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
manufacturer UnityMixer = IntellijelDesigns
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
manufacturer BD808 = TipTopAudio
manufacturer BD909 = TipTopAudio
manufacturer CP909 = TipTopAudio
manufacturer Hats808 = TipTopAudio
manufacturer One = TipTopAudio
manufacturer RS808 = TipTopAudio
manufacturer SD808 = TipTopAudio
manufacturer CO = VerbosElectronics
manufacturer ATC = VerbosElectronics
manufacturer Compressor = WMD
manufacturer Fracture = WMD
manufacturer PerformanceMixer = WMD

description :: Module -> Maybe Text
description TM = Just "Probabilistic random sequence generator"
description Pluck = Just "Carplus-Strong plucked string voice"
description Autobot = Just "X0x-style sequencer"
description M303 = Just "TB-303 alike synthesizer voice"
description Robokop = Just "X0x-style trigger sequencer"
description VScale = Just "Buffered (adjustable) multiple"
description Salt = Just "Programmable module"
description SaltPlus = Just "Expander for Salt"
description DUSeq = Just "Sequencer"
description A100_bl2 = Nothing
description A100_bl4 = Nothing
description A100_bl8 = Nothing
description A100_bl42 = Nothing
description A101_2 = Just "Low Pass Gate"
description A103 = Just "Low pass VCF with 18dB/octave slope"
description A106_6 = Just "XP Multimode filter"
description A110_1 = Just "Standard VCO"
description A111_4 = Just "Quad Precision VCO"
description A114 = Just "Dual Ring modulator"
description A115 = Just "Audio Divider"
description A116 = Just "VC Waveform Processor"
description A118 = Just "Noise / Random"
description A119 = Just "External Input / Envelope Follower"
description A120 = Just "24dB Low Pass"
description A124 = Just "Wasp Filter"
description A130 = Just "Linear VCA"
description A131 = Just "Exponential VCA"
description A132_3 = Just "Dual linear/exponential VCA"
description A136 = Just "Distortion / Waveshaper"
description A138a = Just "Linear mixer"
description A138b = Just "Logarithmic mixer"
description A138m = Just "Matrix mixer"
description A138s = Just "Stereo mixer"
description A140 = Just "ADSR envelope generator"
description A143_2 = Just "Quad ADSR envelope generator"
description A143_9 = Just "VC Quadrature LFO/VCO"
description A145 = Just "LFO"
description A146 = Just "Variable waveform LFO"
description A148 = Just "Dual Sample & Hold"
description A151 = Just "Quad sequential switch"
description A152 = Just "Voltage addressed switch"
description A156 = Just "Dual quantizer"
description A160 = Just "Clock divider"
description A160_5 = Just "VC clock multiplier"
description A161 = Just "8-step clock sequencer"
description A162 = Just "Dual trigger delay"
description A166 = Just "Logic"
description A170 = Just "Dual slew limiter"
description A177_2 = Just "Foot controller"
description A180_1 = Just "Passive multiple"
description A180_2 = Just "Passive multiple"
description A180_3 = Just "Dual buffered multiple"
description A182_1 = Just "Switched multiples"
description A184_1 = Just "Ring Modulator / S&H / T&H / Slew Limiter"
description A185_2 = Just "Precision CV adder"
description A190_4 = Just "Midi-CV/Gate/Sync-Interface"
description DLD = Nothing
description QCD = Nothing
description QCDExp = Nothing
description UnityMixer = Nothing
description SubMix = Just "12 channel mixer"
description DPO = Just "Dual Prismatic Oscillator"
description ErbeVerb = Nothing
description Maths = Just "Complex function generator"
description STO = Nothing
description Branches = Just "Dual bernoulli gate"
description Grids = Just "Topographic drum sequencer"
description BIA = Just "Digital drum voice"
description Mixer = Just "4 channel mixer / attenuators"
description Outs = Just "Stereo Headphone Amp and Line Outs"
description Evolution = Just "Variable character ladder filter"
description BD808 = Just "808 baddrum"
description BD909 = Just "909 baddrum"
description CP909 = Just "909 Clap"
description Hats808 = Just "808 Hats"
description One = Just "Mono WAV sample player"
description RS808 = Just "808 Rimshot"
description SD808 = Just "808 Snaredrum"
description CO = Nothing
description ATC = Nothing
description Compressor = Nothing
description Fracture = Just "Multi-Particle Percussion Synthesizer"
description PerformanceMixer = Nothing

synopsis :: Module -> Maybe (Html ())
synopsis Pluck = Nothing
synopsis TM = Nothing
synopsis Autobot = Nothing
synopsis M303 = Nothing
synopsis Robokop = Nothing
synopsis VScale = Just $ p_ $ toHtml $ pack "A precision active multiple which features very high input impedance, very low output impedance and an offset voltage of <1mV."
synopsis Salt = Nothing
synopsis SaltPlus = Nothing
synopsis DUSeq = Nothing
synopsis A100_bl2 = Nothing
synopsis A100_bl4 = Nothing
synopsis A100_bl8 = Nothing
synopsis A100_bl42 = Nothing
synopsis A101_2 = Just $ do
  p_ "A 12 dB low pass filter that can be switched to VCA or a combination of Low Pass and VCA. The controlling elements for frequency (LP mode) resp. amplitude (VCA mode) are so-called vactrols.  Because of the vactrol circuit the audio signal is not fully attenuated in the VCA mode at the minimum setting of the F/A control."
  p_ "The frequency (in LP mode) resp. the amplitude (in VCA mode) is controlled manually (F/A) and by the 2 control inputs CV1 (without attenuator) and CV2 (with attenuator). The audio input is equipped with an attenuator to enable distortion too (above position 5 distortion is obtained with standard A-100 audio levels, e.g. VCO). The resonance function \"colors\" the sound and is adjustable all the way up to self-oscillation. Resonance and consequently self-oscillation may vary with the filter frequency because of vactrol tolerances. Due to the circuit the resonance has a little bit of influence on the audio level (increasing resonance = increasing audio level). To obtain the original Buchla sound the resonance control has to be set fully counterclockwise."
  p_ "The function of the module is controlled by a manual switch. The left and right positions of the switch correspond to LP resp. VCA mode. In the middle position one obtains the combination of Low Pass and VCA. In this position it is also possible to control the function of the module by the two Gate inputs G1 and G2. The table printed at the front panel shows the connection between the gate levels (L = low, H = high) and the module function."

synopsis A103 = Just $ p_ $ toHtml $ pack "The circuit is based on a modified transistor ladder (Moog ladder) and is a reproduction of the legendary TB303 filter."
synopsis A106_6 = Just $ p_ $ do
    toHtml $ pack "A multimode filter based on the filter circuit of the "
    a_ [href_ $ pack "https://en.wikipedia.org/wiki/Oberheim_Xpander"] $ toHtml (pack "Oberheim Xpander")
    toHtml (pack ". The module features 15 different filter types with 8 filters available simultaneously. The toggle switch Filter Group is used to switch between 2 filter groups.")
synopsis A110_1 = Just $ p_ $ toHtml $ pack "A voltage controlled oscillator with a frequency range of about eight octaves (ca. 15Hz ... 8kHz). It can produce four waveforms simultaneously: rectangle, sawtooth, triangle, and sine wave. The output levels are typically 8Vpp for saw and rectangle, and 10Vpp for triangle and sine."
synopsis A111_4 = Just $ p_ $ toHtml $ pack "Four precision VCOs with individual controls, inputs and outputs as well as a common control and output unit."
synopsis A114 = Just $ p_ $ toHtml $ pack "Two individual ring modulator units with X and Y input and X*Y output sockets per unit."
synopsis A115 = Nothing
synopsis A116 = Nothing
synopsis A118 = Nothing
synopsis A119 = Nothing
synopsis A120 = Nothing
synopsis A124 = Nothing
synopsis A130 = Nothing
synopsis A131 = Nothing
synopsis A132_3 = Just $ p_ $ toHtml $ pack "Two identical voltage controlled amplifiers (VCA). Each VCA has a manual gain control and a control voltage input with attenuator. The character of the control scale can be switched to linear or exponential. All inputs and outputs are DC coupled. Consequently the VCAs can be used to process both audio and control voltages. The input has no attenuator available but is capable to process up to 16Vss signals (i.e. -8V...+8V) without distortion."
synopsis A136 = Nothing
synopsis A138a = Nothing
synopsis A138b = Nothing
synopsis A138m = Nothing
synopsis A138s = Nothing
synopsis A140 = Just $ p_ $ toHtml $ pack "An (ADSR) envelope generator.  The shape of the envelope is governed by four parameters: Attack, Decay, Sustain and Release."
synopsis A143_2 = Nothing
synopsis A143_9 = Nothing
synopsis A145 = Nothing
synopsis A146 = Nothing
synopsis A148 = Nothing
synopsis A151 = Nothing
synopsis A152 = Nothing
synopsis A156 = Nothing
synopsis A160 = Nothing
synopsis A160_5 = Nothing
synopsis A161 = Nothing
synopsis A162 = Nothing
synopsis A166 = Nothing
synopsis A170 = Nothing
synopsis A177_2 = Nothing
synopsis A180_1 = Nothing
synopsis A180_2 = Nothing
synopsis A180_3 = Nothing
synopsis A182_1 = Just $ p_ $ toHtml $ pack "A passive multiple equipped with a 3-position switch per socket that allows to connect the corresponding socket to bus #1 (left position), bus #2 (right position) or to turn the socket off (center position)."
synopsis A184_1 = Just $ do
  p_ "The upper section is a ring modulator with the usual X/Y inputs and the X*Y output."
  p_ "The lower section is a Sample & Hold (S&H) / Track & Hold (T&H) unit followed by a slew limiter. A toggle switch is used to set the mode to S&H or T&H. In S&H mode the unit picks out a sample from the voltage at the SH input at the rising edge of the trigger signal input. In T&H mode the output follows the input voltage as long as the level of the trigger signal is high. As soon as the trigger signal turns low, the last voltage is stored. The trigger input is internally normalled to high, i.e. the unit works just as a slew limiter in T&H mode when no trigger signal is applied."
synopsis A185_2 = Just $ p_ $ toHtml $ pack "A precision control voltage adder with four inputs: one with attenuator and three without attenuator. Each input is normalled to +1 V."
synopsis A190_4 = Nothing
synopsis DLD = Nothing
synopsis QCD = Nothing
synopsis QCDExp = Nothing
synopsis UnityMixer = Nothing
synopsis SubMix = Nothing
synopsis DPO = Nothing
synopsis ErbeVerb = Nothing
synopsis Maths = Nothing
synopsis STO = Nothing
synopsis Branches = Just $ p_ "Takes a logic signal (trigger or gate) as an input, and routes it to either of its two outputs according to a random coin toss."
synopsis Grids = Nothing
synopsis BIA = Just $ p_ "A parameterized digital drum synthesizer. At its heart, it is a simple six-oscillator additive synthesizer with adjustable waveform, harmonic spread and decay. Adjustable attack including a noise oscillator is also included. These are summed and fed into an infinifolder for crunch and variety."
synopsis Mixer = Nothing
synopsis Outs = Nothing
synopsis Evolution = Nothing
synopsis BD808 = Nothing
synopsis BD909 = Nothing
synopsis CP909 = Nothing
synopsis Hats808 = Nothing
synopsis One = Nothing
synopsis RS808 = Nothing
synopsis SD808 = Nothing
synopsis CO = Just $ do
  p_ "A master oscillator with a voltage controlled waveshaper and a modulation oscillator that can be used for FM or AM."
  p_ "The waveshape of the modulation can be set to triangle, square or saw, each with a dedicated output.  Modulation amount can be voltage controlled with a reversing attenuator on the CV input."
  p_ "The master oscillator has dedicated outputs for triangle, square and sine waves. The master output can blend from sine to square to saw to folded sine all from voltage control with reversing attenuators on all of the CV inputs."
  p_ "The oscillators are analog triangle cores with discrete transistor exponential converters. Each has inputs for linear and exponential FM. Both have 1 volt/octave trimmed CV inputs as well."
synopsis ATC = Just $
  p_ $ toHtml $ pack "A totally discrete VCA with simultaneous exponential and linear CV input. It also contains an all discrete, Vactrol based VCF with diode limited resonance. It also has a discrete input gain stage. Careful balancing of the input gain and resonance control sets the mix of self oscillation and input signal, distorted on the VCA input if desired."
synopsis Compressor = Nothing
synopsis Fracture = Nothing
synopsis PerformanceMixer = Nothing

isBlindPanel :: Module -> Bool
isBlindPanel A100_bl2 = True
isBlindPanel A100_bl4 = True
isBlindPanel A100_bl8 = True
isBlindPanel A100_bl42 = True
isBlindPanel _ = False

width :: Module -> Length
width Pluck = 2 % HorizontalPitch
width TM = 2 % HorizontalPitch
width Autobot = 50 % HorizontalPitch
width M303 = 16 % HorizontalPitch
width Robokop = 50 % HorizontalPitch
width VScale = 4 % HorizontalPitch
width Salt = 12 % HorizontalPitch
width SaltPlus = 10 % HorizontalPitch
width DUSeq = 30 % HorizontalPitch
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
width A177_2 = 4 % HorizontalPitch
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
width UnityMixer = 2 % HorizontalPitch
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
width BD808 = 4 % HorizontalPitch
width BD909 = 8 % HorizontalPitch
width CP909 = 4 % HorizontalPitch
width Hats808 = 8 % HorizontalPitch
width One = 4 % HorizontalPitch
width RS808 = 4 % HorizontalPitch
width SD808 = 4 % HorizontalPitch
width CO = 32 % HorizontalPitch
width ATC = 16 % HorizontalPitch
width Compressor = 12 % HorizontalPitch
width Fracture = 8 % HorizontalPitch
width PerformanceMixer = 40 % HorizontalPitch

height :: Module -> Length
height _ = 3 % RackUnit

data Currents = Currents Current Current Current deriving (Eq, Show)

instance Semigroup Currents where
  (Currents a1 a2 a3) <> (Currents b1 b2 b3) =
    Currents (a1 |+| b1) (a2 |+| b2) (a3 |+| b3)

instance Monoid Currents where
  mempty = Currents (mA 0) (mA 0) (mA 0)
  mappend = (<>)

currents :: Module -> Currents
currents Pluck = Currents (mA 78) (mA 6) (mA 0)
currents TM = Currents (mA 23) (mA 7) (mA 0)
currents Autobot = Currents (mA 80) (mA 20) (mA 0)
currents M303 = Currents (mA 60) (mA 20) (mA 0)
currents Robokop = Currents (mA 80) (mA 20) (mA 0)
currents VScale = Currents (mA 15) (mA 15) (mA 0)
currents Salt = Currents (mA 250) (mA 50) (mA 0)
currents SaltPlus = mempty
currents DUSeq = Currents (mA 8) (mA 8) (mA 110)
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
currents A177_2 = Currents (mA 10) (mA 10) (mA 0)
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
currents UnityMixer = Currents (mA 11) (mA 14) (mA 0)
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
currents BD808 = Currents (mA 13) (mA 6) (mA 0)
currents BD909 = Currents (mA 34) (mA 21) (mA 0)
currents CP909 = Currents (mA 6) (mA 10) (mA 0)
currents Hats808 = Currents (mA 30) (mA 25) (mA 0)
currents One = Currents (mA 80) (mA 8) (mA 0)
currents RS808 = Currents (mA 14) (mA 13) (mA 0)
currents SD808 = Currents (mA 18) (mA 16) (mA 0)
currents CO = Currents (mA 70) (mA 50) (mA 0)
currents ATC = Currents (mA 30) (mA 20) (mA 0)
currents Compressor = Currents (mA 52) (mA 40) (mA 0)
currents Fracture = Currents (mA 63) (mA 17) (mA 0)
currents PerformanceMixer = Currents (mA 450) (mA 430) (mA 0)

powerOfCurrents :: Currents -> Power
powerOfCurrents (Currents p12 m12 p5) =
  v 12 |*| p12 |+| v 12 |*| m12 |+| v 5 |*| p5

data FrontPanel e = UnknownPanel
                  | Blank
                  | Tabular [[Maybe e]]
                  | ASCIILayoutDiagram Text [e]
                  deriving (Foldable, Functor, Eq)

data FPECount a = FPECount
                { buttons, leds, rotaries, sockets, switches :: a }
                deriving (Eq, Foldable, Functor, Show)
instance Num a => Semigroup (FPECount a) where
  FPECount a b c d e <> FPECount a' b' c' d' e' =
    FPECount (a + a') (b + b') (c + c') (d + d') (e + e')

instance Num a => Monoid (FPECount a) where
  mempty = FPECount 0 0 0 0 0
  mappend = (<>)

instance Show a => ToHtml (FPECount a) where
  toHtml (FPECount {..}) = do 
    toHtml $ show buttons
    toHtml $ pack " buttons, "
    toHtml $ show leds
    toHtml $ pack " LEDs, "
    toHtml $ show rotaries
    toHtml $ pack " rotaries, "
    toHtml $ show sockets
    toHtml $ pack " sockets, "
    toHtml $ show switches
    toHtml $ pack " switches."
  toHtmlRaw (FPECount {..}) = do
    toHtmlRaw $ show buttons
    toHtmlRaw $ pack " buttons, "
    toHtmlRaw $ show leds
    toHtmlRaw $ pack " LEDs, "
    toHtmlRaw $ show rotaries
    toHtmlRaw $ pack " rotaries, "
    toHtmlRaw $ show sockets
    toHtmlRaw $ pack " sockets, "
    toHtmlRaw $ show switches
    toHtmlRaw $ pack " switches."

fpeCount :: Module -> FPECount Int
fpeCount = foldMap (go . snd) . frontPanel where
  go Button = mempty { buttons = 1 }
  go LED = mempty { leds = 1 }
  go Rotary = mempty { rotaries = 1 }
  go (Socket _ _) = mempty { sockets = 1 }
  go (Switch _) = mempty { switches = 1 }
  go _ = mempty

data FrontPanelElement = Button
                       | LED
                       | Rotary
                       | SDSlot
                       | Socket Direction SocketType
                       | Switch [Text]
                       | RotarySwitch [Text]
                       deriving (Eq, Show)

data Direction = In | Out | InOrOut deriving (Eq, Show)
data SocketType = SocketType Length Contacts Channels deriving (Eq, Show)
data Channels = Mono | Stereo deriving (Bounded, Enum, Eq, Show)
data Contacts = TS | TRS deriving (Bounded, Enum, Eq, Show)

type Label = Text

type Panel = FrontPanel (Label, FrontPanelElement)

frontPanel Pluck = Tabular [
    [Just ("Trig.", Socket In mini)]
  , [Just ("Damp", Rotary)]
  , [Just ("Damp", Socket In mini)]
  , [Just ("Decay", Rotary)]
  , [Just ("Decay", Socket In mini)]
  , [Just ("V/Oct", Socket In mini)]
  , [Just ("Pitch", Rotary)]
  , [Just ("", LED)]
  , [Just ("Out", Socket Out mini)]
  ]
frontPanel TM = Tabular [
    [Just ("", LED)]
  , [Just ("Trig", Socket In mini)]
  , [Just ("Prob", Socket In mini)]
  , [Just ("Prob", Rotary)]
  , [Just ("Steps", Socket In mini)]
  , [Just ("Steps", Rotary)]
  , [Just ("Amp", Rotary)]
  , [Just ("Out", Socket Out mini)]
  ]
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
|] [("CV-In", Socket In mini)
  , ("Slide-In", Socket In mini)
  , ("Tune", Rotary)
  , ("VCO-Out", Socket Out mini)

  , ("Saw/Sq", Switch ["Saw", "Square"])

  , ("VCO-CV", Socket In mini)
  , ("VCO-Mod", Rotary)
  , ("VCF-Mod", Rotary)
  , ("VCF-CV", Socket In mini)

  , ("VCF-In", Socket In mini)
  , ("Cutoff", Rotary)
  , ("Res", Rotary)
  , ("VCA-In", Socket In mini)

  , ("Gate-In", Socket In mini)
  , ("Env-Mod", Rotary)
  , ("Decay", Rotary)
  , ("Env-Out", Socket Out mini)

  , ("Gate/VCA", Switch ["Gate", "VCA"])

  , ("Acc-In", Socket In mini)
  , ("Acc", Rotary)
  , ("Volume", Rotary)
  , ("Out", Socket Out mini)
  ]
frontPanel Robokop = ASCIILayoutDiagram [r|
 b      c       d                                     e           f

 a    o   o   o   o   o   o   o   o   o   o   o   o   o   o   o   o

a=Start/Stop
b=Scale
c=Inst./Sel.
d=Leng./Shuf.
e=Clear
f=Write/Next
|] [("1", Socket Out mini)
  , ("2", Socket Out mini)
  , ("3", Socket Out mini)
  , ("4", Socket Out mini)
  , ("5", Socket Out mini)
  , ("6", Socket Out mini)
  , ("7", Socket Out mini)
  , ("8", Socket Out mini)
  , ("9", Socket Out mini)
  , ("10", Socket Out mini)
  , ("11", Socket Out mini)
  , ("12", Socket Out mini)
  , ("Tempo", Rotary)
  , ("Prog", RotarySwitch $ map (pack . show) [1..12])
  , ("Mode", RotarySwitch [ "Track write", "Track play", "Pattern play",
                             "Tap write", "Step write", "Pattern play" ])
  , ("Slide", Rotary)
  , ("Scale", Button)
  , ("Inst./Sel.", Button)
  , ("Leng./Shuf.", Button)
  , ("Clear", Button)
  , ("Next/Write", Button)
  , ("Start/Stop", Button)
  , ("1", Button)
  , ("2", Button)
  , ("3", Button)
  , ("4", Button)
  , ("5", Button)
  , ("6", Button)
  , ("7", Button)
  , ("8", Button)
  , ("9", Button)
  , ("10", Button)
  , ("11", Button)
  , ("12", Button)
  , ("13", Button)
  , ("14", Button)
  , ("15", Button)
  , ("16", Button)
  ]
frontPanel VScale = ASCIILayoutDiagram [r|
  Out5
- Out4
- Out3
- Out2
- Out1

  In
|] []
frontPanel Salt = ASCIILayoutDiagram [r|
   Poti1    Poti2
   Poti3    Poti4

   SW1       SW2

T1I T2I CV1I CV2I AI

T1O T2O CV3I CV4I AI

        CV1O CV2O AO
  USB
        CV3O CV4O AO
|] []
frontPanel SaltPlus = ASCIILayoutDiagram [r|
   Poti1    Poti2
   Poti3    Poti4

   SW3       SW4

CV5I CV6I T3I T4I

CV7I CV8I T3O T4O

CV5O CV5O
            USB
CV7O CV8O
|] []
frontPanel DUSeq = UnknownPanel
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
|] [("CV-In1", Socket In mini)
  , ("F/A", Rotary)
  , ("CV-In2", Socket In mini)
  , ("CV2", Rotary)
  , ("AudioIn", Socket In mini)
  , ("Lev.", Rotary)
  , ("AudioOut", Socket Out mini)
  , ("Res.", Rotary)
  , ("LP/VCA", Switch ["LP", "LP+VCA", "VCA"])
  , ("G1", Socket In mini)
  , ("G2", Socket In mini)
  ]
frontPanel A103 = UnknownPanel
frontPanel A106_6 = ASCIILayoutDiagram [r|
                        3A/3A1L
Audio-In       Lev.
                        2N/2N1L

FCV1           Frq.   2H1L/4B

                        3H/3H1L
FCV2           FCV
                        2H/2H1L

QCV            QCV      1H/2B    

                        3L/4L
Filtergrp      Q
                        1L/2L
|] [("Audio-In", Socket In mini)
  , ("FCV1", Socket In mini)
  , ("FCV2", Socket In mini)
  , ("QCV", Socket In mini)
  , ("Filtergrp", Switch ["Group 1", "Group 2"])
  , ("Lev.", Rotary)
  , ("Frq.", Rotary)
  , ("FCV", Rotary)
  , ("QCV", Rotary)
  , ("Q", Rotary)
  , ("3A/3A1L", Socket Out mini)
  , ("2N/2N1L", Socket Out mini)
  , ("2H1L/4B", Socket Out mini)
  , ("3H/3H1L", Socket Out mini)
  , ("2H/2H1L", Socket Out mini)
  , ("1H/2B", Socket Out mini)
  , ("3L/4L", Socket Out mini)
  , ("1L/2L", Socket Out mini)
  ]
frontPanel A110_1 = ASCIILayoutDiagram [r|
SYNC     Range
CV1      Tune
CV2      CV2
PW-CV1   PW
PW-CV2   PW-CV2
Saw  Sqr  Tri  Sin
|] [("Sync", Socket In mini)
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
|] $ concat (replicate 4 [
    ("1V", Socket In mini)
  , ("+/-", Switch ["-1", "0", "+1"])
  , ("Tri", Socket Out mini)
  , ("Sq", Socket Out mini)
  , ("Tune", Rotary)
  , ("ModLevel", Rotary)
  , ("Mod", Socket In mini)
  , ("XM/LM|PM", Switch ["XM", "LM", "PM"])
  , ("Saw", Socket Out mini)
  , ("Sync", Socket In mini)
  ]) <> [
    ("1V", Socket In mini)
  , ("+/-", Switch ["-1", "0", "+1"])
  , ("Tri", Socket Out mini)
  , ("Sq", Socket Out mini)
  , ("Tune", Rotary)
  , ("ModLevel", Rotary)
  , ("XM", Socket In mini)
  , ("Saw", Socket Out mini)
  , ("CVOut", Socket Out mini)
  ]

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
frontPanel A116 = Tabular [
    [Just ("AudioIn", Socket In mini), Just ("Lev.", Rotary)]
  , [Nothing, Just ("Clipping-Level", Rotary)]
  , [Just ("Clipping-CV", Socket In mini), Just ("CCV", Rotary)]
  , [Just ("Symm.-CV", Socket In mini), Just ("SCV", Rotary)]
  , [Just ("AudioOut", Socket In mini), Just ("Sym", Rotary)]
  ]
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
frontPanel A120 = Tabular [
    [Just ("CV1", Socket In mini), Just ("Frq.", Rotary)]
  , [Just ("CV2", Socket In mini), Just ("CV2", Rotary)]
  , [Just ("CV3", Socket In mini), Just ("CV3", Rotary)]
  , [Just ("AudioIn", Socket In mini), Just ("Lev.", Rotary)]
  , [Just ("AudioOut", Socket Out mini), Just ("Res.", Rotary)]
  ]
frontPanel A124 = Tabular [
    [Just ("AudioIn", Socket In mini), Just ("Lev.", Rotary)]
  , [Just ("CV1", Socket In mini), Just ("Frq.", Rotary)]
  , [Just ("CV2", Socket In mini), Just ("CV2", Rotary)]
  , [Just ("BPOut", Socket Out mini), Just ("Res.", Rotary)]
  , [Just ("LP/HPOut", Socket Out mini), Just ("Mix", Rotary)]
  ]
frontPanel A130 = Tabular [
    [Just ("CV1", Socket In mini), Just ("Gain", Rotary)]
  , [Just ("CV2", Socket In mini), Just ("CV2", Rotary)]
  , [Just ("AudioIn1", Socket In mini), Just ("In1", Rotary)]
  , [Just ("AudioIn2", Socket In mini), Just ("In2", Rotary)]
  , [Just ("AudioOut", Socket In mini), Just ("Out", Rotary)]
  ]
frontPanel A131 = frontPanel A130
frontPanel A132_3 = Tabular $ concat $ replicate 2 [
    [Just ("CV-In", Socket In mini), Just ("CV", Rotary)]
  , [Just ("In", Socket In mini), Just ("Gain", Rotary)]
  , [Just ("Out", Socket Out mini), Just ("Lin./Exp.", Switch ["Lin.", "Exp."])]]
frontPanel A136 = Tabular [
    [Just ("Input", Socket In mini), Just ("+A", Rotary)]
  , [Just ("Ext.Level", Socket In mini), Just ("+L", Rotary)]
  , [Nothing, Just ("A", Rotary)]
  , [Just ("Ext.Level", Socket In mini), Just ("-L", Rotary)]
  , [Just ("Output", Socket Out mini), Just ("-A", Rotary)]
  ]
frontPanel A138a = Tabular [
    [Just ("Input1", Socket In mini), Just ("In1", Rotary)]
  , [Just ("Input2", Socket In mini), Just ("In2", Rotary)]
  , [Just ("Input3", Socket In mini), Just ("In3", Rotary)]
  , [Just ("Input4", Socket In mini), Just ("In4", Rotary)]
  , [Just ("Output", Socket In mini), Just ("Out", Rotary)]
  ]
frontPanel A138b = frontPanel A138a
frontPanel A138m = UnknownPanel
frontPanel A138s = Tabular [
    [Just ("Level", Rotary), Nothing, Just ("Pan", Rotary)]
  , [Just ("Level", Rotary), Nothing, Just ("Pan", Rotary)]
  , [Just ("Level", Rotary), Nothing, Just ("Pan", Rotary)]
  , [Just ("Level", Rotary), Nothing, Just ("Pan", Rotary)]
  , [Just ("In1", Socket In mini), Just ("In2", Socket In mini), Just ("OutL", Socket Out mini)]
  , [Just ("In3", Socket In mini), Just ("In4", Socket In mini), Just ("OutR", Socket Out mini)]
  ]
frontPanel A140 = Tabular [
    [Just ("Gate", Socket In mini), Just ("A", Rotary)]
  , [Just ("Retrig.", Socket In mini), Just ("D", Rotary)]
  , [Just ("Output", Socket In mini), Just ("S", Rotary)]
  , [Just ("Output", Socket In mini), Just ("R", Rotary)]
  , [Just ("Inv.Output", Socket In mini), Just ("Timerange", Switch ["?", "?", "?"])]
  ]
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
|] $ concat $ replicate 4 [
    ("RT", Socket In mini)
  , ("G", Socket In mini)
  , ("A", Socket Out mini)
  , ("D", Socket Out mini)
  , ("R", Socket Out mini)
  , ("m/l/h", Switch ["m", "l", "h"])
  , ("Attack", Rotary)
  , ("Decay", Rotary)
  , ("Sustain", Rotary)
  , ("Release", Rotary)
  , ("Out", Socket Out mini)
  ]
frontPanel A143_9 = Tabular [
    [Just ("CV1", Socket In mini), Just ("Frq.", Rotary)]
  , [Just ("CV2", Socket In mini), Just ("CV2", Rotary)]
  , [Nothing, Just ("Range", Switch ["L", "M", "h"])]
  , [Just ("0°", Socket Out mini), Just (":", LED)]
  , [Just ("90°", Socket Out mini), Nothing]
  , [Just ("180°", Socket Out mini), Nothing]
  , [Just ("270°", Socket Out mini), Nothing]
  ]
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
  , [Nothing, Just ("Steps", Switch ["2", "3", "4"])]
  ]
frontPanel A152 = ASCIILayoutDiagram [r|
          SWI/O  THOuts  Dig.Outs
          1      1       1
Addr
          2      2       2

          3      3       3
 CV
          4      4       4

 CVIn     5      5       5
 ClockIn
 ResetIn  6      6       6

          7      7       7

          8      8       8

          SWI/O  THOut
|] []
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
frontPanel A177_2 = UnknownPanel
frontPanel A180_1 = Tabular $ replicate 8 [Just ("I/O", Socket InOrOut mini)]
frontPanel A180_2 = frontPanel A180_1
frontPanel A180_3 = Tabular $ concat $ replicate 2 [
    [Just ("In", Socket In mini)]
  , [Just ("Out1", Socket Out mini)]
  , [Just ("Out2", Socket Out mini)]
  , [Just ("Out3", Socket Out mini)]
  ]
frontPanel A182_1 = UnknownPanel
frontPanel A184_1 = UnknownPanel
frontPanel A185_2 = ASCIILayoutDiagram [r|
  Lev.1
1     -/0/+
2     -/0/+
3     -/0/+
4     -/0/+
o1    o2
inv   o3
|] []
--0..10 (5)
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
|] $ [("Tap", Button)
    , ("Tap-Out", Socket Out mini)
    ] <> concat (replicate 4 [
      ("CLK-In", Socket In mini)
    , ("CLK-Out", Socket Out mini)
    , ("Div/Mult-CV", Socket In mini)
    , ("Reset", Socket In mini)
    , ("Div/Mult", RotarySwitch ["/32", "/16", "/8", "/7", "/6", "/5", "/4", "/3", "/2", "=", "*2", "*3", "*4", "*5", "*6", "*7", "*8", "*16", "*16"])
    ])
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
|] $ concat $ replicate 4 [
    ("CV", Socket In mini)
  , ("Inv", Socket Out mini)
  , ("CV-Att", Rotary)
  , ("Gate-PW", Rotary)
  , ("Mode", Switch ["?", "?", "?"])
  , ("Div/Mult-Att", Rotary)
  ]
frontPanel UnityMixer = UnknownPanel
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
|] [("Pitch", Rotary)
  , ("Attack", Rotary)
  , ("Spread", Rotary)
  , ("Morph", Rotary)
  , ("Decay", Rotary)
  , ("Harmonic", Rotary)
  , ("Fold", Rotary)
  , ("S/L/M", Switch ["Skin", "Liquid", "Metal"])
  , ("B/A/T", Switch ["Bass", "Alto", "Treble"])
  , ("Trig", Button)
  , ("Pitch", Socket In mini)
  , ("Attack", Socket In mini)
  , ("S/L/M", Socket In mini)
  , ("Spread", Socket In mini)
  , ("Morph", Socket In mini)
  , ("Decay", Socket In mini)
  , ("B/A/T", Socket In mini)
  , ("Harmonic", Socket In mini)
  , ("Fold", Socket In mini)
  , ("Trig", Socket In mini)
  , ("Out", Socket Out mini)
  ]
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
In4
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
frontPanel BD808 = Tabular [
    [Just ("Level", Rotary)]
  , [Just ("Tone", Rotary)]
  , [Just ("Decay", Rotary)]
  , [Just ("Accent", Rotary)]
  , [Just ("Accent-In", Socket In mini)]
  , [Just ("Gate-In", Socket In mini)]
  , [Just ("BDOut", Socket Out mini)]
  ]
frontPanel BD909 = Tabular [
    [Just ("Overload", Rotary), Just ("Level", Rotary)]
  , [Just ("Attack", Rotary), Just ("Decay", Rotary)]
  , [Just ("TuneAttack", Rotary), Just ("ToneDecay", Rotary)]
  , [Just ("OscTune", Rotary), Just ("Accent", Rotary)]
  , [Just ("CV Tune", Rotary), Just ("Accent-In", Socket In mini)]
  , [Just ("CV Tune", Socket In mini), Just ("Gate-In", Socket In mini)]
  , [Just ("CV Overload", Socket In mini), Just ("BDOut", Socket Out mini)]
  ]
frontPanel CP909 = Tabular [
    [Just ("Level", Rotary)]
  , [Just ("Trash", Rotary)]
  , [Nothing]
  , [Just ("Accent", Rotary)]
  , [Just ("Accent-In", Socket In mini)]
  , [Just ("Gate-In", Socket In mini)]
  , [Just ("ClapOut", Socket Out mini)]
  ]
frontPanel Hats808 = Tabular [
    [Just ("Level", Rotary), Just ("Level", Rotary)]
  , [Just ("Q", Rotary), Just ("QCV", Socket In mini)]
  , [Just ("Decay", Rotary), Just ("BPOut", Socket Out mini)]
  , [Just ("Accent", Rotary), Just ("Accent", Rotary)]
  , [Just ("AccentIn", Socket In mini), Just ("AccentIn", Socket In mini)]
  , [Just ("GateIn", Socket In mini), Just ("GateIn", Socket In mini)]
  , [Just ("OHOut", Socket Out mini), Just ("CHOut", Socket Out mini)]
  ]
frontPanel One = Tabular [
    [Just ("Lev.", Rotary), Nothing]
  , [Just ("Play", Button), Just ("SD", SDSlot)]
  , [Just ("File", Rotary), Nothing]
  , [Just ("Pitch", Rotary), Nothing]
  , [Just ("CV", Socket In mini), Nothing] 
  , [Just ("Gate-In", Socket In mini), Nothing]
  , [Just ("Out", Socket Out mini), Nothing]
  ]
frontPanel RS808 = Tabular [
    [Nothing, Just ("Level", Rotary), Nothing]
  , [Just ("S1", Switch []), Nothing, Just ("S2", Switch [])]
  , [Nothing, Just ("Snap", Rotary), Nothing]
  , [Nothing, Just ("Pitch", Rotary), Nothing]
  , [Nothing, Just ("Accent", Rotary), Nothing]
  , [Nothing, Just ("Accent-In", Socket In mini), Nothing]
  , [Nothing, Just ("Gate-In", Socket In mini), Nothing]
  , [Nothing, Just ("RS/ClOut", Socket Out mini), Nothing]
  ]
frontPanel SD808 = Tabular $ map (map Just) [
    [("Level", Rotary)]
  , [("Tone", Rotary)]
  , [("Snappy", Rotary)]
  , [("Accent", Rotary)]
  , [("AccentIn", Socket In mini)]
  , [("GateIn", Socket In mini)]
  , [("SDOut", Socket Out mini)]
  ]
frontPanel CO = ASCIILayoutDiagram [r|
, t sq s  sync   t sq s out
          am/fm           e/o  av
      f   t/sq/s       f       cv
  mod       i      car    low  av
                               cv
 fm cv      i   fm cv     tmbr av
 fm cv 1v   i   fm cv 1v       cv
|] []
frontPanel ATC = ASCIILayoutDiagram [r|
Gain
Cutoff       Amp
Resonance
    FreqCV  ExpCV  LinCV
In  FreqCV  ExpCV  LinCV  Out
|] [("Gain", Rotary)
  , ("Cutoff", Rotary)
  , ("Resonance", Rotary)
  , ("Amp", Rotary)
  , ("FreqCV", Rotary)
  , ("ExpCV", Rotary)
  , ("LinCV", Rotary)
  , ("In", Socket In mini)
  , ("FreqCV", Socket In mini)
  , ("ExpCV", Socket In mini)
  , ("LinCV", Socket In mini)
  , ("Out", Socket Out mini)
  ]
frontPanel Compressor = UnknownPanel
frontPanel Fracture = ASCIILayoutDiagram [r|
    Surface  Trig  Spread

    Decay          Freq
     Tail           Punch
    Density        Reverb

  Spread  Freq  Density  Reverb
  Surface Decay Pitch    OutR
  Trigger Tick  Inf      OutL
|] [ ("Surface", Rotary)
   , ("Trig", Button)
   , ("Spread", Rotary)
   , ("Decay", Rotary)
   , ("Freq", Rotary)
   , ("Tail", Switch ["", "", ""])
   , ("Punch", Switch ["", "", ""])
   , ("Density", Rotary)
   , ("Reverb", Rotary)
   , ("Spread", Socket In mini)
   , ("Freq", Socket In mini)
   , ("Density", Socket In mini)
   , ("Reverb", Socket In mini)
   , ("Surface", Socket In mini)
   , ("Decay", Socket In mini)
   , ("Pitch", Socket In mini)
   , ("Trigger", Socket In mini)
   , ("Tick", Socket In mini)
   , ("Inf", Socket In mini)
   , ("OutL", Socket Out mini)
   , ("OutR", Socket Out mini)
   ]
frontPanel PerformanceMixer = UnknownPanel

type Row = [Module]
data Case a = Case
  { caseType :: String
  , rows :: [[a]]
  } deriving (Show, Generic, Functor, Foldable)

instance FromJSON a => FromJSON (Case a)

type System = [Case Module]

rowCurrents = mconcat . map currents
caseCurrents = mconcat . map rowCurrents . rows
systemCurrents = mconcat . map caseCurrents

rowHeight = maximum . map height
rowWidth = qSum . map width
caseHeight = qSum . map rowHeight
caseWidth = maximum . map rowWidth
systemArea = qSum . map (\c -> caseWidth c |*| caseHeight c)

showCaseSize (Case _ x) = let w = caseWidth x
                              h = caseHeight x
                          in show (round (w # HorizontalPitch)) ++ " " ++
                             show HorizontalPitch ++ " × " ++
                             show (round (h # RackUnit)) ++ " " ++
                             show RackUnit

fullName :: Module -> Html ()
fullName m = toHtml $ manufacturerName (manufacturer m) <> pack " " <> name m <> maybe "" (\x -> " - " <> x) (description m)

showAsIntegralIn x u = show (round $ x # u) <> show u

instance ToHtml Currents where
  toHtml (Currents p12 m12 p5) = toHtml $
    p12 `showAsIntegralIn` milli Ampere <> " @ +12V, " <>
    m12 `showAsIntegralIn` milli Ampere <> " @ -12V, " <>
    p5 `showAsIntegralIn` milli Ampere <> " @ +5V"
  toHtmlRaw = toHtml

frontPanelHtml :: Module -> Html ()
frontPanelHtml m = do
  h2_ "Front panel"
  panelHtml $ frontPanel m
  when (hasSwitchPositionLabels m) $ do
    h3_ "Labels of switch positions and rotary detents"
    describeSwitches m

panelHtml :: Panel -> Html ()
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
  desc (n, Switch x) = do
    dt_ $ toHtml $ n <> pack " (switch)"
    dd_ $ toHtml $ intercalate ", " x
  desc _ = mempty

moduleLink :: Module -> Html () -> Html ()
moduleLink mod = a_ [href_ ("/Eurorack/Modules/" <> identifier mod <> ".html")]

systemHtml :: System -> Html ()
systemHtml sys = do
    dl_ $ do
      dt_ "Power"
      dd_ $ do
        toHtml (systemCurrents sys)
        toHtml $ pack " ("
        toHtml (powerOfCurrents (systemCurrents sys) `showAsIntegralIn` Watt)
        toHtml $ pack ")"
      dt_ "Controls"
      dd_ $ toHtml $ foldMap fpeCount $ modules sys
    for_ sys $ \c -> do
      p_ $ toHtml $ showCaseSize c
      table_ [class_ "case"] $ traverse_ row (rows c)
    h2_ "Known but unused modules"
    ul_ $ for_ (unused $ modules sys) $ \mod -> li_ (moduleLink mod (fullName mod))
 where
  row :: Row -> Html ()
  row = tr_ . traverse_ cell
  cell :: Module -> Html ()
  cell m = td_ [colspan_ $ pack $ show $ round $ width m # HorizontalPitch] $
    unless (isBlindPanel m) $
      a_ ([href_ $ "/Eurorack/Modules/" <> identifier m <> ".html"] <> maybe [] (\x -> [title_ x]) (description m)) (toHtml $ identifier m)

modules :: System -> [Module]
modules = nub . filter (not . isBlindPanel) . concatMap toList

mini = SocketType (3.5 % milli Meter) TS Mono

balanced (SocketType _ TRS Mono) = True
balanced _ = False

unbalanced = not . balanced

manufacturerUrl Döpfer = Just "http://www.doepfer.de/"
manufacturerUrl _ = Nothing

manufacturerName TwoHP = pack "2HP"
manufacturerName AcidLab = pack "AcidLab"
manufacturerName AJHSynth = pack "AJH Synth"
manufacturerName Bela = pack "Bela"
manufacturerName DetroitUnderground = pack "Detroit Underground"
manufacturerName Döpfer = pack "Döpfer"
manufacturerName FourMS = pack "4ms"
manufacturerName IntellijelDesigns = pack "Intellijel Designs"
manufacturerName LowGainElectronics = pack "Low-Gain Electronics"
manufacturerName MakeNoise = pack "Make Noise"
manufacturerName MutableInstruments = pack "Mutable Instruments"
manufacturerName NoiseEngineering = pack "Noise Engineering"
manufacturerName PittsburghModular = pack "Pittsburgh Modular"
manufacturerName RebelTechnologies = pack "Rebel Technologies"
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

data Device = A100LCB [Module] [Module]
            | A100LC9 [Module] [Module] [Module]
            | A100LMB [Module] [Module]
            | A100LMS9 [Module] [Module] [Module]
            | Megacity
            | X0xb0x
            deriving (Show)

impedance :: Html ()
impedance =
  a_ [href_ $ pack "https://en.wikipedia.org/wiki/Electrical_impedance"] $
  toHtml $ pack "impedance"
vca :: Html ()
vca =
  a_ [href_ $ pack "https://en.wikipedia.org/wiki/Variable-gain_amplifier"] $
  toHtml $ pack "VCA"
