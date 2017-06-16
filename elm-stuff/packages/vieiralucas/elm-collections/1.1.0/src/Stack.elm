module Stack exposing (Stack, empty, top, pop, push, append, toList, fromList)

{-|

# Creating Stacks
@docs Stack, empty
# Getting top
@docs top
# Pushing/Popping
@docs push, pop
# Append
@docs append
# To List
@docs toList
# From List
@docs fromList

-}

import List


{-| A Stack containing items of type `a`.
-}
type Stack a
    = Stack (List a)


{-| Creates an empty Stack.

    Stack.empty
-}
empty : Stack a
empty =
    Stack []


{-| Gets the top of the Stack.

    Stack.empty
    |> Stack.push 1
    |> Stack.top
      -- == Just 1
-}
top : Stack a -> Maybe a
top (Stack stack) =
    List.head stack


{-| Pushes an item into the Stack.

    Stack.empty
    |> Stack.push 1
    |> Stack.toList
      -- == [1]
-}
push : a -> Stack a -> Stack a
push a (Stack stack) =
    Stack (a :: stack)


{-| Pops an item from the Stack.

    Stack.empty
    |> Stack.push 1
    |> Stack.pop
      -- == (Just 1, Stack.empty)
-}
pop : Stack a -> ( Maybe a, Stack a )
pop (Stack stack) =
    ( List.head stack, Stack (List.drop 1 stack) )


{-| Converts a Stack into a List

    Stack.empty
    |> Stack.push 1
    |> Stack.push 2
    |> Stack.toList
      -- == [2, 1]
-}
toList : Stack a -> List a
toList (Stack stack) =
    stack

{-| Appends a List into the Stack.
    Stack.empty
    |> Stack.push 1
    |> Stack.append [2, 3]
    |> Stack.toList
      -- == [3, 2, 1]
-}
append : List a -> Stack a -> Stack a
append list (Stack stack) =
  Stack <| (List.reverse list) ++ stack


{-| Converts a List into a Stack

    Stack.fromLista [1, 2]
    |> Stack.push 3
    |> Stack.toList
      -- == [3, 2, 1]
-}
fromList : List a -> Stack a
fromList a =
    Stack (List.reverse a)
