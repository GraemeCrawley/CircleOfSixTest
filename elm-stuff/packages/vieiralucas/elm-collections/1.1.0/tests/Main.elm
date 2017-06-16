port module Main exposing (..)

import StackTests
import QueueTests
import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


all =
    Test.concat
        [ StackTests.all
        , QueueTests.all
        ]


main : TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg
