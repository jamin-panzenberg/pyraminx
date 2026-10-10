module Pyraminx exposing (Color(..), Face, Move, Pyraminx, bottomFace, faceColorsCcw, frontFace, leftFace, move, moveB, moveBI, moveGenerator, moveL, moveLI, moveR, moveRI, moveString, moveT, moveTI, problem, rightFace, solved)

import Problem exposing (Problem)
import Random


type Pyraminx
    = Pyraminx { front : Face, left : Face, right : Face, bottom : Face }


frontFace : Pyraminx -> Face
frontFace (Pyraminx p) =
    p.front


leftFace : Pyraminx -> Face
leftFace (Pyraminx p) =
    p.left


rightFace : Pyraminx -> Face
rightFace (Pyraminx p) =
    p.right


bottomFace : Pyraminx -> Face
bottomFace (Pyraminx p) =
    p.bottom


type Color
    = Green
    | Red
    | Yellow
    | Blue


type Move
    = Move { direction : MoveDirection, inverted : Bool }


moveGenerator : Random.Generator Move
moveGenerator =
    Random.map2
        (\direction inverted -> Move { direction = direction, inverted = inverted })
        (Random.uniform Right [ Left, Top, Back ])
        (Random.uniform True [ False ])


moveString : Move -> String
moveString (Move m) =
    moveDirectionString m.direction
        ++ (if m.inverted then
                "'"

            else
                ""
           )


moveDirectionString : MoveDirection -> String
moveDirectionString md =
    case md of
        Right ->
            "R"

        Left ->
            "L"

        Top ->
            "T"

        Back ->
            "B"


type MoveDirection
    = Right
    | Left
    | Top
    | Back


moveR : Move
moveR =
    Move { direction = Right, inverted = False }


moveRI : Move
moveRI =
    Move { direction = Right, inverted = True }


moveL : Move
moveL =
    Move { direction = Left, inverted = False }


moveLI : Move
moveLI =
    Move { direction = Left, inverted = True }


moveT : Move
moveT =
    Move { direction = Top, inverted = False }


moveTI : Move
moveTI =
    Move { direction = Top, inverted = True }


moveB : Move
moveB =
    Move { direction = Back, inverted = False }


moveBI : Move
moveBI =
    Move { direction = Back, inverted = True }


type Face
    = Face { topLeft : Color, topMiddle : Color, topRight : Color, bottomLeft : Color, bottomMiddle : Color, bottomRight : Color }


faceColorsCcw : Face -> List Color
faceColorsCcw (Face f) =
    [ f.topRight, f.topMiddle, f.topLeft, f.bottomLeft, f.bottomMiddle, f.bottomRight ]


solvedFace : Color -> Face
solvedFace c =
    Face { topLeft = c, topMiddle = c, topRight = c, bottomLeft = c, bottomMiddle = c, bottomRight = c }


solved : Pyraminx
solved =
    Pyraminx { front = solvedFace Blue, left = solvedFace Yellow, right = solvedFace Green, bottom = solvedFace Red }


moveDirection : Pyraminx -> MoveDirection -> Pyraminx
moveDirection (Pyraminx pyraminx) direction =
    let
        (Face front) =
            pyraminx.front

        (Face right) =
            pyraminx.right

        (Face left) =
            pyraminx.left

        (Face bottom) =
            pyraminx.bottom
    in
    Pyraminx
        (case direction of
            Right ->
                { left = Face left
                , front =
                    Face
                        { front
                            | topRight = bottom.topRight
                            , bottomMiddle = bottom.bottomMiddle
                            , bottomRight = bottom.bottomRight
                        }
                , right =
                    Face
                        { right
                            | topLeft = front.bottomMiddle
                            , bottomLeft = front.bottomRight
                            , bottomMiddle = front.topRight
                        }
                , bottom =
                    Face
                        { bottom
                            | topRight = right.bottomMiddle
                            , bottomMiddle = right.topLeft
                            , bottomRight = right.bottomLeft
                        }
                }

            Left ->
                { right = Face right
                , front =
                    Face
                        { front
                            | topLeft = left.bottomMiddle
                            , bottomLeft = left.bottomRight
                            , bottomMiddle = left.topRight
                        }
                , left =
                    Face
                        { left
                            | topRight = bottom.topLeft
                            , bottomMiddle = bottom.topRight
                            , bottomRight = bottom.topMiddle
                        }
                , bottom =
                    Face
                        { bottom
                            | topLeft = front.bottomMiddle
                            , topMiddle = front.bottomLeft
                            , topRight = front.topLeft
                        }
                }

            Top ->
                { bottom = Face bottom
                , front =
                    Face
                        { front
                            | topLeft = right.topLeft
                            , topMiddle = right.topMiddle
                            , topRight = right.topRight
                        }
                , left =
                    Face
                        { left
                            | topLeft = front.topLeft
                            , topMiddle = front.topMiddle
                            , topRight = front.topRight
                        }
                , right =
                    Face
                        { right
                            | topLeft = left.topLeft
                            , topMiddle = left.topMiddle
                            , topRight = left.topRight
                        }
                }

            Back ->
                { front = Face front
                , left =
                    Face
                        { left
                            | topLeft = right.bottomMiddle
                            , bottomLeft = right.bottomRight
                            , bottomMiddle = right.topRight
                        }
                , right =
                    Face
                        { right
                            | topRight = bottom.bottomMiddle
                            , bottomMiddle = bottom.topLeft
                            , bottomRight = bottom.bottomLeft
                        }
                , bottom =
                    Face
                        { bottom
                            | topLeft = left.topLeft
                            , bottomLeft = left.bottomLeft
                            , bottomMiddle = left.bottomMiddle
                        }
                }
        )


move : Pyraminx -> Move -> Pyraminx
move pyraminx (Move { direction, inverted }) =
    if inverted then
        moveDirection (moveDirection pyraminx direction) direction

    else
        moveDirection pyraminx direction


problem : Pyraminx -> Problem ( List Move, Pyraminx )
problem initialState =
    { initialState = ( [], initialState )
    , actions =
        \( ms, p ) ->
            [ moveR, moveRI, moveL, moveLI, moveT, moveTI, moveB, moveBI ]
                |> List.map (\m -> { result = ( ms ++ [ m ], move p m ), stepCost = 1 })
    , goalTest = \( _, p ) -> p == solved
    , heuristic = \_ -> 0
    , stateToString = Debug.toString
    }
