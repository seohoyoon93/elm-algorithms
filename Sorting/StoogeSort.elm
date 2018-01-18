module StoogeSort exposing (..)

import Html exposing (text)


stoogeSort : List comparable -> List comparable
stoogeSort list =
    case list of
        [] ->
            list

        [ _ ] ->
            list

        [ f, l ] ->
            [ min f l ] ++ [ max f l ]

        f :: xs ->
            let
                l =
                    List.reverse xs
                        |> List.head
                        |> Maybe.withDefault f

                first_ =
                    min f l

                last_ =
                    max f l

                listLength =
                    List.length list

                stoogeLength =
                    listLength // 3

                middleList =
                    List.reverse xs
                        |> List.drop 1
                        |> List.reverse

                sortingList1 =
                    [ first_ ] ++ middleList ++ [ last_ ]

                stooge1 =
                    List.take (listLength - stoogeLength) sortingList1
                        |> stoogeSort

                remainderStooge1 =
                    List.drop (listLength - stoogeLength) sortingList1

                sortingList2 =
                    stooge1 ++ remainderStooge1

                stooge2 =
                    List.drop stoogeLength sortingList2
                        |> stoogeSort

                remainderStooge2 =
                    List.take stoogeLength sortingList2

                sortingList3 =
                    remainderStooge2 ++ stooge2

                stooge3 =
                    List.take (listLength - stoogeLength) sortingList3
                        |> stoogeSort

                remainderStooge3 =
                    List.drop (listLength - stoogeLength) sortingList3
            in
                stooge3 ++ remainderStooge3


main =
    text ("[5,3,8,1,2]  " ++ toString (stoogeSort [ 5, 3, 8, 1, 2 ]))
