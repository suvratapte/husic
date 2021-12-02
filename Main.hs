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

pitchStandard :: Hz
pitchStandard = 440.0 -- pitch standard for the note `A`

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

semitone :: Semitones -> Hz
semitone n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

sa  = semitone 0
re  = semitone 2
ga  = semitone 4
ma  = semitone 5
pa  = semitone 7
dha = semitone 9
ni  = semitone 11
sa' = semitone 12

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (semitone n) beats

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
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

wave :: [Pulse]
wave = concat [ freq sa beats
              , freq re beats
              , freq ga beats
              , freq ma beats
              , freq pa beats
              , freq dha beats
              , freq ni beats
              , freq sa' beats
              ]
  where
    beats = 1

save :: IO ()
save = B.writeFile outputFilePath $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: IO ()
play = do
  save
  runCommand $ printf "ffplay -showmode 2 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = putStrLn "Hello, world!"
