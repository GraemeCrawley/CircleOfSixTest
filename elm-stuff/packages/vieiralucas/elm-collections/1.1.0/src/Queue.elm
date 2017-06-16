module Queue exposing (Queue, empty, first, enq, deq, toList)

{-|

# Creating Queues
@docs Queue, empty
# Getting first
@docs first
# Enqueuing/Dequeuing
@docs enq, deq
# To List
@docs toList

-}

import List


{-| A Queue containing items of type `a`.
-}
type Queue a
    = Queue (List a)


{-| Creates an empty Queue.

    Queue.empty
-}
empty : Queue a
empty =
    Queue []


{-| Gets first item of Queue.

    Queue.empty
    |> Queue.enq 1
    |> Queue.first
      -- == Just 1
-}
first : Queue a -> Maybe a
first (Queue queue) =
    List.head queue


{-| Enqueues an item to the Queue.

    Queue.empty
    |> Queue.enq 1
    |> Queue.toList
      -- == [1]
-}
enq : a -> Queue a -> Queue a
enq a (Queue queue) =
    Queue (List.append queue [ a ])


{-| Dequeues an item from the Queue.

    Queue.empty
    |> Queue.enq 1
    |> Queue.deq
      -- == (Just 1, Queue.empty)
-}
deq : Queue a -> ( Maybe a, Queue a )
deq (Queue queue) =
    ( List.head queue, Queue (List.drop 1 queue) )


{-| Converts a Queue into a List.

    Queue.empty
    |> Queue.enq 1
    |> Queue.enq 2
    |> Queue.toList
      -- == [1, 2]
-}
toList : Queue a -> List a
toList (Queue queue) =
    queue
