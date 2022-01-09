module Sound.Tidal.Chords where

import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))

major :: Array Int
major = [0,4,7]
minor :: Array Int
minor = [0,3,7]
major7 :: Array Int
major7 = [0,4,7,11]
dom7 :: Array Int
dom7 = [0,4,7,10]
minor7 :: Array Int
minor7 = [0,3,7,10]
aug :: Array Int
aug = [0,4,8]
dim :: Array Int
dim = [0,3,6]
dim7 :: Array Int
dim7 = [0,3,6,9]
one :: Array Int
one = [0]
five :: Array Int
five = [0,7]
plus :: Array Int
plus = [0,4,8]
sharp5 :: Array Int
sharp5 = [0,4,8]
msharp5 :: Array Int
msharp5 = [0,3,8]
sus2 :: Array Int
sus2 = [0,2,7]
sus4 :: Array Int
sus4 = [0,5,7]
six :: Array Int
six = [0,4,7,9]
m6 :: Array Int
m6 = [0,3,7,9]
sevenSus2 :: Array Int
sevenSus2 = [0,2,7,10]
sevenSus4 :: Array Int
sevenSus4 = [0,5,7,10]
sevenFlat5 :: Array Int
sevenFlat5 = [0,4,6,10]
m7flat5 :: Array Int
m7flat5 = [0,3,6,10]
sevenSharp5 :: Array Int
sevenSharp5 = [0,4,8,10]
m7sharp5 :: Array Int
m7sharp5 = [0,3,8,10]
nine :: Array Int
nine = [0,4,7,10,14]
m9 :: Array Int
m9 = [0,3,7,10,14]
m7sharp9 :: Array Int
m7sharp9 = [0,3,7,10,14]
maj9 :: Array Int
maj9 = [0,4,7,11,14]
nineSus4 :: Array Int
nineSus4 = [0,5,7,10,14]
sixby9 :: Array Int
sixby9 = [0,4,7,9,14]
m6by9 :: Array Int
m6by9 = [0,3,9,7,14]
sevenFlat9 :: Array Int
sevenFlat9 = [0,4,7,10,13]
m7flat9 :: Array Int
m7flat9 = [0,3,7,10,13]
sevenFlat10 :: Array Int
sevenFlat10 = [0,4,7,10,15]
nineSharp5 :: Array Int
nineSharp5 = [0,1,13]
m9sharp5 :: Array Int
m9sharp5 = [0,1,14]
sevenSharp5flat9 :: Array Int
sevenSharp5flat9 = [0,4,8,10,13]
m7sharp5flat9 :: Array Int
m7sharp5flat9 = [0,3,8,10,13]
eleven :: Array Int
eleven = [0,4,7,10,14,17]
m11 :: Array Int
m11 = [0,3,7,10,14,17]
maj11 :: Array Int
maj11 = [0,4,7,11,14,17]
elevenSharp :: Array Int
elevenSharp = [0,4,7,10,14,18]
m11sharp :: Array Int
m11sharp = [0,3,7,10,14,18]
thirteen :: Array Int
thirteen = [0,4,7,10,14,17,21]
m13 :: Array Int
m13 = [0,3,7,10,14,17,21]

chordTable :: Map String (Array Int)
chordTable = Map.fromFoldable
  [ Tuple "major" major
  , Tuple "maj" major
  , Tuple "minor" minor
  , Tuple "min" minor
  , Tuple "aug" aug
  , Tuple "dim" dim
  , Tuple "major7" major7
  , Tuple "maj7" major7
  , Tuple "dom7" dom7
  , Tuple "minor7" minor7
  , Tuple "min7" minor7
  , Tuple "dim7" dim7
  , Tuple "one" one
  , Tuple "1" one
  , Tuple "five" five
  , Tuple "5" five
  , Tuple "plus" plus
  , Tuple "sharp5" sharp5
  , Tuple "msharp5" msharp5
  , Tuple "sus2" sus2
  , Tuple "sus4" sus4
  , Tuple "six" six
  , Tuple "6" six
  , Tuple "m6" m6
  , Tuple "sevenSus2" sevenSus2
  , Tuple "7sus2" sevenSus2
  , Tuple "sevenSus4" sevenSus4
  , Tuple "7sus4" sevenSus4
  , Tuple "sevenFlat5" sevenFlat5
  , Tuple "7f5" sevenFlat5
  , Tuple "m7flat5" m7flat5
  , Tuple "m7f5" m7flat5
  , Tuple "sevenSharp5" sevenSharp5
  , Tuple "7s5" sevenSharp5
  , Tuple "m7sharp5" m7sharp5
  , Tuple "m7s5" m7sharp5
  , Tuple "nine" nine
  , Tuple "m9" m9
  , Tuple "m7sharp9" m7sharp9
  , Tuple "m7s9" m7sharp9
  , Tuple "maj9" maj9
  , Tuple "nineSus4" nineSus4
  , Tuple "ninesus4" nineSus4
  , Tuple "9sus4" nineSus4
  , Tuple "sixby9" sixby9
  , Tuple "6by9" sixby9
  , Tuple "m6by9" m6by9
  , Tuple "sevenFlat9" sevenFlat9
  , Tuple "7f9" sevenFlat9
  , Tuple "m7flat9" m7flat9
  , Tuple "m7f9" m7flat9
  , Tuple "sevenFlat10" sevenFlat10
  , Tuple "7f10" sevenFlat10
  , Tuple "nineSharp5" nineSharp5
  , Tuple "9s5" nineSharp5
  , Tuple "m9sharp5" m9sharp5
  , Tuple "m9s5" m9sharp5
  , Tuple "sevenSharp5flat9" sevenSharp5flat9
  , Tuple "7s5f9" sevenSharp5flat9
  , Tuple "m7sharp5flat9" m7sharp5flat9
  , Tuple "eleven" eleven
  , Tuple "11" eleven
  , Tuple "m11" m11
  , Tuple "maj11" maj11
  , Tuple "elevenSharp" elevenSharp
  , Tuple "11s" elevenSharp
  , Tuple "m11sharp" m11sharp
  , Tuple "m11s" m11sharp
  , Tuple "thirteen" thirteen
  , Tuple "13" thirteen
  , Tuple "m13" m13
  ]
