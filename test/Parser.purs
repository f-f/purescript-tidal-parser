module Test.Parser where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Sound.Tidal.Parser (Expr(..))
import Sound.Tidal.Parser as Parser
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

type Spec = Spec.SpecT Aff Unit Identity Unit

seq :: Array Expr -> Expr
seq = Seq <<< List.fromFoldable

squareSeq :: Array Expr -> Expr
squareSeq = SquareSeq <<< List.fromFoldable

squareSuperpos :: Array Expr -> Expr
squareSuperpos = SquareSuperpos <<< List.fromFoldable

angleSeq :: Array Expr -> Expr
angleSeq = AngleSeq <<< List.fromFoldable

curlyPoly :: Maybe Int -> Array Expr -> Expr
curlyPoly factor = CurlyPoly factor <<< List.fromFoldable

bd :: Expr
bd = Sample "bd" Nothing

cp :: Expr
cp = Sample "cp" Nothing

_909 :: Expr
_909 = Sample "909" Nothing

test :: Spec
test = Spec.describe "Parser" do
  Spec.describe "success" goodParser
  -- TODO: bad parser examples
  -- Spec.describe "failure" badParser

goodParser :: Spec
goodParser = Spec.parallel do
  let parsePattern str pat = Spec.it str do
        (Parser.parse str) `Assert.shouldEqual` (Right pat)

  parsePattern "~"
    $ Rest
  parsePattern " ~"
    $ Rest
  parsePattern "~ "
    $ Rest
  parsePattern "~ ~"
    $ seq [Rest, Rest]
  parsePattern " ~ ~   "
    $ seq [Rest, Rest]
  parsePattern "bd"
    $ bd
  parsePattern "bd:1"
    $ Sample "bd" (Just 1)
  parsePattern "bd:"
    $ bd
  parsePattern "bd: bd:"
    $ seq [bd, bd]
  parsePattern "bd!4"
    $ BangRep 4 bd
  parsePattern "bd*4"
    $ StarRep 4 bd
  parsePattern "bd!"
    $ bd
  parsePattern "bd*"
    $ bd
  parsePattern "c4"
    $ Note (-12)
  parsePattern "c4butactuallyasample"
    $ Sample "c4butactuallyasample" Nothing
  parsePattern "h4:0"
    $ Sample "h4" (Just 0)
  parsePattern "[bd bd bd bd]"
    $ squareSeq [bd, bd, bd, bd]
  parsePattern " [ bd bd bd bd]"
    $ squareSeq [bd, bd, bd, bd]
  parsePattern " [ bd bd bd bd  ]"
    $ squareSeq [bd, bd, bd, bd]
  parsePattern "[~ cp ~ cp]"
    $ squareSeq [Rest, cp, Rest, cp]
  parsePattern "[~ <bd cp bd>]"
    $ squareSeq [Rest, angleSeq [bd, cp, bd]]
  parsePattern "[~ gretsch:4]!4"
    $ BangRep 4 (squareSeq [Rest, Sample "gretsch" (Just 4)])
  parsePattern "909!2 909*2 909*4"
    $ seq [BangRep 2 _909, StarRep 2 _909, StarRep 4 _909]
  parsePattern "<c4'min7 as3'maj7 ds4'maj7 f4'maj7>"
    $ angleSeq [ Chord (-12) [0,3,7,10]
               , Chord (-14) [0,4,7,11]
               , Chord (-9) [0,4,7,11]
               , Chord (-7) [0,4,7,11]]
  parsePattern "[e3]!8 [c4]!8 [d4]!8 [a3]!8"
    $ seq [ BangRep 8 $ squareSeq [Note (-20)]
          , BangRep 8 $ squareSeq [Note (-12)]
          , BangRep 8 $ squareSeq [Note (-10)]
          , BangRep 8 $ squareSeq [Note (-15)]]
  parsePattern "[ c5,c6]"
    $ squareSuperpos [ Note 0, Note 12]
  parsePattern "[c5 ,c6]"
    $ squareSuperpos [ Note 0, Note 12]
  parsePattern "[c5, c6]"
    $ squareSuperpos [ Note 0, Note 12]
  parsePattern "[c5,c6 ]"
    $ squareSuperpos [ Note 0, Note 12]
  parsePattern "[c5,c6'min]"
    $ squareSuperpos [Note 0, Chord 12 [0,3,7]]
  parsePattern "[~ [c3,c4]]!4"
    $ BangRep 4 (squareSeq [Rest, squareSuperpos [Note (-24), Note (-12)]])
  parsePattern "<[c5,c6'maj] [c5,c6'min]>"
    $ angleSeq [ squareSuperpos [Note 0, Chord 12 [0,4,7]]
               , squareSuperpos [Note 0, Chord 12 [0,3,7]]]
  parsePattern "<[c3,c4,ds4,g4] [ds3,as4,ds4,g4] [g2,g3,as3,d4] [f2,f3,a3,c4]>"
    $ angleSeq [ squareSuperpos [Note (-24), Note (-12), Note (-9), Note (-5)]
               , squareSuperpos [Note (-21), Note (-2), Note (-9), Note (-5)]
               , squareSuperpos [Note (-29), Note (-17), Note (-14), Note (-10)]
               , squareSuperpos [Note (-31), Note (-19), Note (-15), Note (-12)]]
  parsePattern "<[~ [ c3,c4]]!8 [~ [ds3 , as3]]!8 [~ [g2, g3]]!8 [~ [f2 ,f3]]!8>"
    $ angleSeq [ BangRep 8 $ squareSeq [Rest, squareSuperpos [Note (-24), Note (-12)]]
               , BangRep 8 $ squareSeq [Rest, squareSuperpos [Note (-21), Note (-14)]]
               , BangRep 8 $ squareSeq [Rest, squareSuperpos [Note (-29), Note (-17)]]
               , BangRep 8 $ squareSeq [Rest, squareSuperpos [Note (-31), Note (-19)]]]
  parsePattern "bd bd@0.5 bd@1"
    $ seq [ bd, AtDuration 0.5 bd, AtDuration 1.0 bd ]
  parsePattern "{~}"
    $ curlyPoly Nothing [ Rest ]
  parsePattern "{~,~}"
    $ curlyPoly Nothing [ Rest, Rest ]
  parsePattern "{~ ~}"
    $ curlyPoly Nothing [ seq [Rest, Rest] ]
  parsePattern "{~ , ~ ~}"
    $ curlyPoly Nothing [ Rest, seq [Rest, Rest] ]
  parsePattern "{~ ~ ~ <[~ d6] [[c6 ~] d6]> ~}%4"
    $ curlyPoly (Just 4) [ seq [ Rest
                               , Rest
                               , Rest
                               , angleSeq [ squareSeq [Rest, Note 14]
                                          , squareSeq [squareSeq [Note 12, Rest], Note 14]]
                               , Rest]]
  parsePattern "z180_3"
    $ Sample "z180_3" Nothing
  parsePattern "z180_3:0"
    $ Sample "z180_3" (Just 0)
  parsePattern "<z180_3:0 ~ z180_3:1 ~>"
    $ angleSeq [ Sample "z180_3" (Just 0), Rest, Sample "z180_3" (Just 1), Rest ]
  parsePattern "<a5 [[~ c6 c6 ~] c6] f5 [[~ g5 g5 ~] g5] d5 [[~ e5 e5 ~] e5] f5 [~ [a4,a5] ~ [a4]]>"
    $ angleSeq [ Note 9, squareSeq [squareSeq [Rest, Note 12, Note 12, Rest], Note 12]
               , Note 5, squareSeq [squareSeq [Rest, Note 7, Note 7, Rest], Note 7]
               , Note 2, squareSeq [squareSeq [Rest, Note 4, Note 4, Rest], Note 4]
               , Note 5, squareSeq [Rest, squareSuperpos [Note (-3), Note 9], Rest, squareSeq [Note (-3)]]]
  parsePattern "<c4(3 , 8) as3( 5,8 ,4) ds4(3,8 ) f4>"
    $ angleSeq [ ParensEuclid 3 8 Nothing (Note (-12))
               , ParensEuclid 5 8 (Just 4) (Note (-14))
               , ParensEuclid 3 8 Nothing (Note (-9))
               , Note (-7)]
  parsePattern "shake(3,8,3)"
    $ ParensEuclid 3 8 (Just 3) (Sample "shake" Nothing)
