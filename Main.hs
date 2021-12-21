module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

sampleRate :: Samples
sampleRate = 48000.0 -- samples per second

volume :: Float
volume = 0.2

bpm :: Beats
bpm = 240.0

pitchA :: Hz
pitchA = 440.0

sa :: Hz
sa  = semitone pitchA 0

re :: Hz
re  = semitone pitchA 2

ga :: Hz
ga  = semitone pitchA 4

ma :: Hz
ma  = semitone pitchA 5

pa :: Hz
pa  = semitone pitchA 7

dha :: Hz
dha = semitone pitchA 9

ni :: Hz
ni  = semitone pitchA 11

silence :: Hz
silence = 0.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

semitone :: Hz -> Semitones -> Hz
semitone pitch n = pitch * (2 ** (1.0 / 12.0)) ** n

note :: Hz -> Semitones -> Beats -> [Pulse]
note pitch n beats = freq (semitone pitch n) beats

freq' :: [Hz] -> Beats -> [Pulse]
freq' hzs beats = concatMap (`freq` beats) hzs

freq :: Hz -> Beats -> [Pulse]
freq hz beats =
  map (* volume) $
    zipWith4 (\w x y z -> w * x * y * z) output attack decaySustain release
  where
    step = hz * 2 * pi / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0, 0.0005 ..]

    decaySustain :: [Pulse]
    decaySustain = let
      attackLength = length $ takeWhile (/= 1) attack
      paddingForAttack = replicate attackLength 1.0
      in
      concat [paddingForAttack, [1.0, 0.99985 .. 0.5], repeat 0.5]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    duration :: Seconds
    duration = beats * beatDuration

    output :: [Pulse]
    output = map (sin . (* step)) [0.0 .. sampleRate * duration]

alankar :: [Hz]
alankar = [ sa, re, ga
          , silence
          , re, ga, ma
          , silence
          , ga, ma, pa
          , silence
          , ma, pa, dha
          , silence
          , pa, dha, ni
          , silence
          , dha, ni, sa * 2
          ]

wave :: [Pulse]
wave = freq' (alankar ++ [silence] ++ reverse alankar) beats
  where beats = 1

save :: IO ()
save = B.writeFile outputFilePath . B.toLazyByteString $ foldMap B.floatLE wave

play :: IO ()
play = do
  save
  runCommand $ printf "ffplay -showmode 2 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = putStrLn "Hello, world!"
