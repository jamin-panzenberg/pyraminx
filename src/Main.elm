module Main exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html
import Html.Events
import Json.Decode as Decode
import Length
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Pyraminx
import Quantity exposing (Quantity)
import Random
import Scene3d
import Scene3d.Material as Material
import SketchPlane3d
import Triangle2d
import Triangle3d


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    , windowSize : ( Int, Int )
    , pyraminx : Pyraminx.Pyraminx
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | WindowResized ( Int, Int )
    | Moved Pyraminx.Move
    | Scramble
    | Scrambled (List Pyraminx.Move)


init : () -> ( Model, Cmd Msg )
init () =
    ( { azimuth = Angle.degrees 0
      , elevation = Angle.degrees 0
      , orbiting = False
      , windowSize = ( 700, 700 )
      , pyraminx = Pyraminx.solved
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        WindowResized windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        Moved move ->
            ( { model | pyraminx = Pyraminx.move model.pyraminx move }, Cmd.none )

        Scramble ->
            ( model, Random.generate Scrambled (Random.list 100 Pyraminx.moveGenerator) )

        Scrambled moves ->
            ( { model | pyraminx = List.foldr (\move p -> Pyraminx.move p move) model.pyraminx moves }, Cmd.none )


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decode.Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        cameraSubscriptions =
            if model.orbiting then
                -- If we're currently orbiting, listen for mouse moves and mouse button
                -- up events (to stop orbiting); in a real app we'd probably also want
                -- to listen for page visibility changes to stop orbiting if the user
                -- switches to a different tab or something
                [ Browser.Events.onMouseMove decodeMouseMove
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

            else
                -- If we're not currently orbiting, just listen for mouse down events
                -- to start orbiting
                [ Browser.Events.onMouseDown (Decode.succeed MouseDown) ]
    in
    Sub.batch (Browser.Events.onResize (\x y -> WindowResized ( x, y )) :: cameraSubscriptions)


pyraminxColor : Pyraminx.Color -> Color.Color
pyraminxColor c =
    case c of
        Pyraminx.Blue ->
            Color.blue

        Pyraminx.Green ->
            Color.green

        Pyraminx.Red ->
            Color.red

        Pyraminx.Yellow ->
            Color.yellow


unitEquilateralTriangle : Triangle2d.Triangle2d Length.Meters coordinates
unitEquilateralTriangle =
    Triangle2d.from
        (Point2d.meters 0 0)
        (Point2d.meters 1 0)
        (Point2d.meters 0.5 (sqrt 3 / 2))


type alias VertexCoords coordinates =
    { top : Point3d.Point3d Length.Meters coordinates
    , back : Point3d.Point3d Length.Meters coordinates
    , right : Point3d.Point3d Length.Meters coordinates
    , left : Point3d.Point3d Length.Meters coordinates
    }


vertexCoords : VertexCoords coordinates
vertexCoords =
    { top = Point3d.meters 0 -1 (1 / sqrt 2)
    , back = Point3d.meters 0 1 (1 / sqrt 2)
    , right = Point3d.meters 1 0 (-1 / sqrt 2)
    , left = Point3d.meters -1 0 (-1 / sqrt 2)
    }



-- TODO face shouldn't take vertex exactly, it should be some directional offset


type alias VertexGetter coordinates =
    VertexCoords coordinates -> Point3d.Point3d Length.Meters coordinates


invertPoint : Point3d.Point3d units coordinates -> Point3d.Point3d units coordinates
invertPoint p =
    let
        ( y, x, z ) =
            ( Point3d.xCoordinate p, Point3d.yCoordinate p, Point3d.zCoordinate p )
    in
    Point3d.xyz (Quantity.negate x) (Quantity.negate y) (Quantity.negate z)


faceScene : Pyraminx.Face -> Pyraminx.Color -> VertexGetter coordinates -> VertexGetter coordinates -> VertexGetter coordinates -> Scene3d.Entity coordinates
faceScene face _ oppositeVertex upVertex rightVertex =
    let
        oppositeVertexCoord =
            oppositeVertex vertexCoords

        sketchPlane =
            SketchPlane3d.throughPoints (Point3d.scaleAbout Point3d.origin (1 / 3) (invertPoint oppositeVertexCoord)) (upVertex vertexCoords) (rightVertex vertexCoords)
                |> Maybe.withDefault SketchPlane3d.xy
    in
    Scene3d.group
        (Pyraminx.faceColorsCcw face
            |> List.indexedMap
                (\i pieceColor ->
                    Scene3d.triangle
                        (Material.color (pyraminxColor pieceColor))
                        (unitEquilateralTriangle
                            |> Triangle2d.rotateAround Point2d.origin (Angle.degrees (toFloat -i * 60 + 30))
                            |> Triangle2d.scaleAbout Point2d.origin (2 / 3)
                            |> Triangle3d.on sketchPlane
                        )
                )
        )


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a camera by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        camera =
            Camera3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 5
                , fov = Camera3d.angle (Angle.degrees 30)
                , projection = Camera3d.Perspective
                }
    in
    { title = "Pyraminx"
    , body =
        [ Html.div [] [ Html.button [ Html.Events.onClick Scramble ] [ Html.text "Scramble" ] ]
        , Html.div []
            ([ Pyraminx.moveL
             , Pyraminx.moveLI
             , Pyraminx.moveR
             , Pyraminx.moveRI
             , Pyraminx.moveT
             , Pyraminx.moveTI
             , Pyraminx.moveB
             , Pyraminx.moveBI
             ]
                |> List.map (\move -> Html.button [ Html.Events.onClick (Moved move) ] [ Html.text (Pyraminx.moveString move) ])
            )
        , Scene3d.cloudy
            { camera = camera
            , upDirection = Direction3d.negativeZ
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int (Tuple.first model.windowSize), Pixels.int (Tuple.second model.windowSize) )
            , background = Scene3d.transparentBackground
            , entities =
                [ faceScene (Pyraminx.bottomFace model.pyraminx) Pyraminx.Red .top .left .right
                , faceScene (Pyraminx.leftFace model.pyraminx) Pyraminx.Green .right .top .left
                , faceScene (Pyraminx.rightFace model.pyraminx) Pyraminx.Yellow .left .top .back
                , faceScene (Pyraminx.frontFace model.pyraminx) Pyraminx.Blue .back .top .right
                ]
            }
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
