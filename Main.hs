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

outputFilePath :: FilePath
outputFilePath = "output.bin"

sampleRate :: Samples
sampleRate = 48000.0 -- samples per second

volume :: Float
volume = 0.2

pitchStandard :: Hz
pitchStandard = 440.0 -- pitch standard for the note `A`

semitone :: Semitones -> Hz
semitone n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Seconds -> [Pulse]
note n duration = freq (semitone n) duration

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $
    zipWith4 (\w x y z -> w * x * y * z) output attack decaySustain release
  where
    step = hz * 2 * pi / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0, 0.0005 ..]

    decaySustain :: [Pulse]
    decaySustain = let
      attackLength = length $ takeWhile (/= 1) attack
      paddingForAttack = take attackLength $ repeat 1.0
      in
      concat [paddingForAttack, [1.0, 0.99985 .. 0.5], repeat 0.5]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

wave :: [Pulse]
wave = concat [ note 0 duration
              , note 0 duration
              , note 0 duration
              , note 0 duration
              , note 0 duration
              , note 0 duration
              , note 0 duration
              , note 0 duration
              ]
  where
    duration = 0.5

save :: IO ()
save = B.writeFile outputFilePath $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: IO ()
play = do
  save
  runCommand $ printf "ffplay -showmode 2 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = putStrLn "Hello, world!"
