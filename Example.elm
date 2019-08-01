module Example exposing (..)

import Main exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange,list, string,tuple)
import Test exposing (..)

{-
Rappels sur les operateurs |> et <|

String.repeat 3  ( String.toUpper  ( String.append "h" "i"))
String.repeat 3 <| String.toUpper <| String.append "h" "i"

String.append "h" "i" |> String.toUpper |> String.repeat 3

->"HIHIHI"

-}

{-
procedures utiles : 
    describe : String -> List Test -> Test
    test :                                String -> (() -> Expectation) -> Test
    fuzz :                    Fuzzer a -> String -> (a -> Expectation) -> Test
    fuzzWith : FuzzOptions -> Fuzzer a -> String -> (a -> Expectation) -> Test
-}

testAddition : Test
testAddition =  
    describe "Addition  : a + b"
        [fuzzWith { runs = 10 }  (tuple ( intRange -10 0, intRange -10 0)) "a :[-10,0],b:[-10,0]" 
            (\(a,b)  -> Expect.equal  (a + b) (addition (a,b) ))

        ,fuzzWith { runs = 10 }  (tuple ( intRange 0 10, intRange 0 10)) "a :[0,10],b:[0,10]" <|
            \(a,b)  ->addition (a,b)
                    |> Expect.equal (a + b)  

        ,test "0+0"(\_-> Expect.equal (addition(0,0))  (0 + 0))
        ]
            
testMultiplication  : Test
testMultiplication  = 
        describe "Multiplication : a * b"
            [fuzz2  (intRange -10 10) (intRange -10 10) "a :[-10,10],b:[-10,10]" <|
                \a b  -> mult a b
                |> Expect.equal (a * b)
            ,test "0*0" <|
                \_ ->  Expect.equal (mult 0 0)  (0 * 0)
            ]

testPuissance  : Test
testPuissance = 
    describe "puissance  "
            [fuzzWith { runs = 10 }  (tuple ( intRange -6 6, intRange 0 5)) "a^b avec a : [-6,6] , b : [0,5]" <|
                \(a,b)  -> puissance a b
                |> Expect.equal ( a^b)
             , test " 0 ^ 0" <|
                \_ ->  Expect.equal (puissance 0 0)  (0^0)
             , fuzzWith { runs = 10 } (intRange -6 6)  " a^0 , avec a : [-6,6]" <|
                \ a  -> puissance a 0 
                |> Expect.equal (a^0)
             , fuzzWith { runs = 10 } (intRange 1 5)  " 0^b , avec b : [1,5]" <|
                \ b  -> puissance  0 b 
                |> Expect.equal (0^b)
            ]
            
           