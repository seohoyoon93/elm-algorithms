module QuickSort exposing (..)

import Html exposing (text)


quickSort : List comparable -> List comparable
quickSort list =
    case list of
        [] ->
            list

        [ _ ] ->
            list

        pv :: xs ->
            let
                smaller =
                    List.filter (\x -> x < pv) xs

                larger =
                    List.filter (\x -> x >= pv) xs
            in
                quickSort smaller ++ [ pv ] ++ quickSort larger


main =
    text (toString (quickSort [ 5, 3, 8, 1, 2 ]))
