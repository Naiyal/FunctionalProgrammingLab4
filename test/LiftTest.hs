{-# LANGUAGE OverloadedStrings #-}

module LiftTest (tests) where

import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import FiniteStateMachine

assertEqualText :: String -> Text -> Text -> Assertion
assertEqualText msg expected actual = assertEqual msg (T.unpack expected) (T.unpack actual)

test_generateDot :: Test
test_generateDot = TestCase $ do
    let fsm = liftModel
    let expectedDot = T.unlines
            [ "digraph FSM {"
            , "  Red [shape=circle];"
            , "  RedPedestrian [shape=circle];"
            , "  Green [shape=circle];"
            , "  Yellow [shape=circle];"
            , "  start -> Red;"
            , "  Red [shape=doublecircle];"
            , "  Red -> RedPedestrian [label=\"ButtonPressed\"];"
            , "  RedPedestrian -> Green [label=\"Timeout\"];"
            , "  Green -> Yellow [label=\"Timeout\"];"
            , "  Yellow -> Red [label=\"Timeout\"];"
            , "}"]
    assertEqualText "generateDot should produce the correct DOT output" expectedDot (generateDot fsm)

tests :: Test
tests = TestList [test_generateDot]
