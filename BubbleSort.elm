module BubbleSort exposing (..)

import Html exposing (text)


bubbleSort : List comparable -> List comparable
bubbleSort list =
    case list of
        [] ->
            list

        [ _ ] ->
            list

        x :: y :: rest ->
            let
                sortingList =
                    bubbleCompare list

                reversedSortingList =
                    List.reverse sortingList

                head_ =
                    List.take 1 reversedSortingList

                tail_ =
                    List.drop 1 reversedSortingList
                        |> List.reverse
            in
                List.append head_ (bubbleSort tail_)


bubbleCompare : List comparable -> List comparable
bubbleCompare list =
    case list of
        [] ->
            list

        [ _ ] ->
            list

        x :: y :: rest ->
            (max x y) :: (bubbleCompare ((min x y) :: rest))


main =
    text (toString (bubbleSort [ 5, 3, 8, 1, 2 ]))
