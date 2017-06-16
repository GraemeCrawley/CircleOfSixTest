module QueueTests exposing (..)

import Test exposing (..)
import Expect
import Stack exposing (Stack)
import Queue exposing (Queue)


all : Test
all =
    describe "Queue"
        [ test "empty queue contains nothing" <|
            \() ->
                Queue.empty
                    |> Queue.first
                    |> Expect.equal Nothing
        , test "first is the first enq element" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.first
                    |> Expect.equal (Just 1)
        , test "deq empty Queue does nothing" <|
            \() ->
                Queue.empty
                    |> Queue.deq
                    |> Expect.equal ( Nothing, Queue.empty )
        , test "deq eliminates first element" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.deq
                    |> Expect.equal ( Just 1, (Queue.empty |> Queue.enq 2) )
        , test "toList of empty gives empty List" <|
            \() ->
                Queue.empty
                    |> Queue.toList
                    |> Expect.equal []
        , test "toList converts queue to List" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.toList
                    |> Expect.equal [ 1, 2 ]
        ]
