-- Ferguson's Astronomical Rotula.
-- there are 5 plates in the rotula, which you can click
-- and drag to rotate. the centre can be dragged to
-- reposition, and the ring of the suns rays can be
-- dragged to resize.
-- This was used to predict eclipses; if you drag the
-- 'Sun's place' and 'Moon's place' handle round to
-- meet the 'ascending node' handle, the holes for
-- sun or moon in the top plate turn black.


module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html, text)
import Html.Attributes as HAttr
import Mouse
import Svg exposing (circle, defs, path, rect, svg, textPath, text_)
import Svg.Attributes as Attr


type alias Model =
    { angles : Array Float
    , clicked_r : Float
    , clicked_angle : Float
    , clicked_disc : Int
    , old_angles : Array Float
    , origin : ( Float, Float )
    , scale : Float
    , clicked_scale : Float
    }


init : Model
init =
    { angles = Array.fromList [ 0, 0, -1.478, -1.59, 2.625, -0.36 ]
    , clicked_angle = 0
    , clicked_disc = -1
    , clicked_r = 25
    , old_angles = Array.fromList []
    , origin = ( 250, 250 )
    , scale = 1
    , clicked_scale = 0
    }


type Msg
    = MouseDown Int Mouse.Event
    | MouseMove Mouse.Event
    | MouseUp Mouse.Event


recenter: Model -> (Float, Float) -> (Float, Float)
recenter model ( x, y ) =
    let
        ( ox, oy ) =
            model.origin
    in
    ( x - ox, y - oy )

update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown ring mouse ->
            click model ring <| recenter model mouse.clientPos

        MouseMove mouse ->
            drag model mouse.clientPos

        MouseUp mouse ->
            { model | clicked_disc = -1 }


click : Model -> Int  -> (Float, Float) -> Model
click model ring ( x, y ) =
    let
        ( r, clicked_angle ) =
            toPolar ( x, y )
    in
        { model
            | clicked_angle = clicked_angle
            , clicked_disc = ring
            , clicked_r = r
            , clicked_scale = model.scale
            , old_angles = model.angles
        }


dragScale : Model -> (Float, Float) -> Model
dragScale model ( x, y ) =
    let
        ( r, theta ) =
            toPolar ( x, y )
    in
    -- this doesn't work right, I kindof understand why.
    -- when we adjust the scale, it ends up somehow larger
    -- than it should be, with the result that the rescaled
    -- point we are now dragging is in the wrong place.
    { model | scale = clamp 1 3 (r / model.clicked_r) }


dragRotate : Model -> (Float, Float) -> Model
dragRotate model ( x, y ) =
    let
        ( r, theta ) =
            toPolar ( x, y )

        old_angle =
            Maybe.withDefault 0 <| Array.get model.clicked_disc model.old_angles
    in
    { model | angles = Array.set model.clicked_disc (theta - model.clicked_angle + old_angle) model.angles }


drag : Model -> (Float, Float) -> Model
drag model ( x, y ) =
    case model.clicked_disc of
        (-1) ->
            model

        0 ->
            { model | origin = ( x, y ) }

        1 ->
            dragScale model <| recenter model ( x, y )

        _ ->
            dragRotate model <| recenter model ( x, y )


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = root
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \m -> Sub.none
        }


root : Model -> Html Msg
root model =
    let
        angles =
            Array.toList model.angles

        ( ox, oy ) =
            model.origin

        disc n =
            Svg.g [ Attr.transform <| "rotate(" ++ (toString <| (*) (180 / pi) <| Maybe.withDefault 0 <| Array.get n model.angles) ++ ")" ]
    in
    Html.div []
    [ Html.h1 [][text "Ferguson's Astronomical Rotula"]
    , Html.p [] 
      [text "Ferguson was a self-taught astronomer in the 1700s; he designed this device to replace the use of some astronomical tables, for example, to predict eclipses. You can find the manual he published for it "
      , Html.a [ HAttr.href "https://books.google.co.uk/books?id=tSlbAAAAcAAJ&printsec=frontcover#v=onepage&q&f=false" ]
       [ text "here" ]
      , text ". I was curious how it worked and so sliced up a photo of one in svg so you can play with it. Drag the sun to move, drag the sun's rays to resize, and the other moving parts can be rotated. If the sun lies near the line of the ecliptic (the plane of the earth-moon-sun orbital system) at a new or full moon, the holes in the paper will show through a black mark indicating an eclipse. Eclipses of the moon can usefully be predicted this way, but eclipses of the sun only happen in specific places, so additional calculations would be needed" 
      ]

    , svg
        [ Attr.width "100%"
        , Attr.height "1000"
        , Mouse.onUp MouseUp
        , Mouse.onMove MouseMove
        , Attr.style "border: 1px solid black"
        ]
    <|
        [ Svg.defs []
            [ Svg.image
                [ Attr.id "image"
                , Attr.xlinkHref "rotula.jpeg"
                , Attr.width "660px"
                , Attr.height "756px"
                , Attr.x "-307px"
                , Attr.y "-330px"
                ]
                []
            , Svg.pattern
                [ Attr.id "plate1"
                , Attr.x "0"
                , Attr.y "0"
                , Attr.width "1"
                , Attr.height "1"
                ]
                [ Svg.g
                    [ Attr.transform "translate(235,235)" ]
                    [ Svg.use [ Attr.xlinkHref "#image" ] [] ]
                ]
            , Svg.pattern
                [ Attr.id "plate2"
                , Attr.x "0"
                , Attr.y "0"
                , Attr.width "1"
                , Attr.height "1"
                ]
                [ Svg.g
                    [ Attr.transform "translate(210,210),rotate(20.5)" ]
                    [ Svg.use [ Attr.xlinkHref "#image" ] [] ]
                ]
            , Svg.pattern
                [ Attr.id "plate3"
                , Attr.x "0"
                , Attr.y "0"
                , Attr.width "1"
                , Attr.height "1"
                ]
                [ Svg.g
                    [ Attr.transform "translate(142,142),rotate(209.5)" ]
                    [ Svg.use [ Attr.xlinkHref "#image" ] [] ]
                ]
            , Svg.pattern
                [ Attr.id "plate4"
                , Attr.x "0"
                , Attr.y "0"
                , Attr.width "1"
                , Attr.height "1"
                ]
                [ Svg.g
                    [ Attr.transform "translate(132,132.5),rotate(90.5)" ]
                    [ Svg.use [ Attr.xlinkHref "#image" ] [] ]
                ]
            , Svg.pattern
                [ Attr.id "plate5"
                , Attr.x "0"
                , Attr.y "0"
                , Attr.width "1"
                , Attr.height "1"
                ]
                [ Svg.g
                    [ Attr.transform "translate(122,122),rotate(84.6)" ]
                    [ Svg.use [ Attr.xlinkHref "#image" ] [] ]
                ]
            ]
        , Svg.g
            [ Attr.transform <| "translate(" ++ toString ox ++ "," ++ toString oy ++ "),scale(" ++ toString model.scale ++ ")" ]
            [ Svg.g [] <| renderPlate1 model
            , disc 5 <| renderPlate2 model
            , disc 4 <| renderPlate3 model
            , disc 3 <| renderPlate4 model
            , disc 2 <| renderPlate5 model
            , disc 2 <| renderScaleZone model
            , disc 2 <| renderDragZone model
            ]
        ]
  ]

renderPlate1 : a -> List (Svg.Svg Msg)
renderPlate1 model =
    [ Svg.path
        [ Attr.d <| circlePath 235
        , Attr.fill "url(#plate1)"
        , Attr.stroke "black"
        ]
        []
    ]

renderPlate2 : a -> List (Svg.Svg Msg)
renderPlate2 model =
    [ Svg.path
        [ Attr.d <| circlePath 210
        , Attr.fill "url(#plate2)"
        , Attr.stroke "black"
        , Attr.fillOpacity "1"
        , Mouse.onDown (MouseDown 5)
        ]
        []
    ]

renderPlate3 : a -> List (Svg.Svg Msg)
renderPlate3 model =
    [ Svg.path
        [ Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 142, 0 )
                , pathA ( 0, 0 ) Large Clockwise ( 142, -0.39 )
                , pathL ( 0, 0 ) ( 170, -0.39 )
                , pathL ( 0, 0 ) ( 200, -0.2 )
                , pathA ( 0, 0 ) Small Clockwise ( 200, 0 )
                , pathZ
                , pathM ( 0, 0 ) ( 169, -0.05 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 169, -0.31 )
                , pathL ( 0, 0 ) ( 144, -0.31 )
                , pathA ( 0, 0 ) Small Clockwise ( 144, -0.05 )
                , pathZ
                ]
        , Attr.stroke "black"
        , Attr.fill "url(#plate3)" -- "#c3a688"
        , Attr.fillOpacity "1"
        , Mouse.onDown (MouseDown 4)
        ]
        []

    -- blank fill under plate 3
    , Svg.circle
        [ Attr.r "132px"
        , Attr.fill "#c3a688"
        ]
        []

    -- these dark patches, indicating eclipses, 
    -- are +/- 12 days for lunar eclipses and +/- 18 days for solar.
    -- (or 11° 38' lunar=(11+38/60)/360*365=11.79,
    -- 17° 25' solar =(17+25/60)/360*365=17.65d)
    -- descending node
    , Svg.path
        [ Attr.fill "black"
        , Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 79, pi-0.304 )
                , pathA ( 0, 0 ) Small Clockwise ( 79, pi+0.304 )
                , pathL ( 0, 0 ) ( 93, pi+0.304 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 93, pi-0.304 )
                , pathZ
                ]
        ]
        []
    , Svg.path
        [ Attr.fill "black"
        , Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 97, pi-0.203 )
                , pathA ( 0, 0 ) Small Clockwise ( 97, pi+0.203 )
                , pathL ( 0, 0 ) ( 111, pi+0.203 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 111, pi-0.203 )
                , pathZ
                ]
        ]
        []
    -- ascending node
    , Svg.path
        [ Attr.fill "black"
        , Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 79, -0.304 )
                , pathA ( 0, 0 ) Small Clockwise ( 79, 0.304 )
                , pathL ( 0, 0 ) ( 93, 0.304 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 93, -0.304 )
                , pathZ
                ]
        ]
        []
    , Svg.path
        [ Attr.fill "black"
        , Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 97, -0.203 )
                , pathA ( 0, 0 ) Small Clockwise ( 97, 0.203 )
                , pathL ( 0, 0 ) ( 111, 0.203 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 111, -0.203 )
                , pathZ
                ]
        ]
        []
    ]

renderPlate4 : a -> List (Svg.Svg Msg)
renderPlate4 model =
    [ Svg.path
        [ Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 132, 0 )
                , pathA ( 0, 0 ) Large Anticlockwise ( 132, 0.12 )
                , pathL ( 0, 0 ) ( 160, 0.08 )
                , pathL ( 0, 0 ) ( 160, 0 )
                , pathZ
                , pathM ( 86, 0 ) ( 7, 0 )
                , pathA ( 86, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                , pathM ( 104, 0 ) ( 7, 0 )
                , pathA ( 104, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                ]
        , Attr.stroke "black"
        , Attr.fill "url(#plate4)" --"#c4aa8f"
        , Attr.fillOpacity "1"
        , Mouse.onDown (MouseDown 3)
        ]
        []

    -- blank fill under plate 5 except at holes
    , Svg.path
        [ Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 122, 0 )
                , pathA ( 0, 0 ) Large Anticlockwise ( 122, 0.1 )
                , pathA ( 0, 0 ) Small Anticlockwise ( 122, 0 )
                , pathZ
                , pathM ( 86, 0 ) ( 7, 0 )
                , pathA ( 86, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                , pathM ( 104, 0 ) ( 7, 0 )
                , pathA ( 104, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                ]
        , Attr.fill "#c4aa8f"
        ]
        []
    ]

renderPlate5 : a -> List (Svg.Svg Msg)
renderPlate5 model =
    [ Svg.path
        [ Attr.d <|
            String.join ""
                [ pathM ( 0, 0 ) ( 122, 0 )
                , pathA ( 0, 0 ) Large Anticlockwise ( 122, 0.16 )
                , pathL ( 0, 0 ) ( 159, 0.09 )
                , pathL ( 0, 0 ) ( 159, 0 )
                , pathZ
                , pathM ( 86, 0 ) ( 7, 0 )
                , pathA ( 86, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                , pathM ( -104, 0 ) ( 7, 0 )
                , pathA ( -104, 0 ) Large Clockwise ( 7, -0.01 )
                , pathZ
                ]
        , Attr.stroke "black"
        , Attr.fill "url(#plate5)" --"#c7a98d"
        , Attr.fillOpacity "1"
        , Mouse.onDown (MouseDown 2)
        ]
        []
    ]

renderScaleZone : a -> List (Svg.Svg Msg)
renderScaleZone model =
    [ Svg.path
        [ Attr.d <| circlePath 44
        , Attr.fill "transparent"
        , Attr.stroke "black"
        , Mouse.onDown (MouseDown 1)
        ]
        []
    ]


type ArcType
    = Large
    | Small


type ArcSweep
    = Clockwise
    | Anticlockwise


pathS : List a -> String
pathS xs =
    String.join " " <| List.map toString xs


pathM : (Float, Float)->(Float, Float)->String
pathM ( cx, cy ) ( r, theta ) =
    let
        ( x, y ) =
            fromPolar ( r, theta )
    in
    String.cons 'M' <| pathS [ x + cx, y + cy ]

pathA : ( Float, Float ) -> ArcType -> ArcSweep -> ( Float, Float ) -> String
pathA ( cx, cy ) large sweep ( r, theta ) =
    let
        lf =
            if large == Large then
                1
            else
                0

        sf =
            if sweep == Clockwise then
                1
            else
                0

        ( x, y ) =
            fromPolar ( r, theta )
    in
    String.cons 'A' <| pathS [ r, r, 0, lf, sf, x + cx, y + cy ]


pathL : (Float, Float)->(Float, Float)->String
pathL ( cx, cy ) ( r, theta ) =
    let
        ( x, y ) =
            fromPolar ( r, theta )
    in
    String.cons 'L' <| pathS [ x + cx, y + cy ]


pathZ : String
pathZ =
    "Z"

circlePath : Float -> String
circlePath r =
    String.join ""
        [ pathM ( 0, 0 ) ( r, 0 )
        , pathA ( 0, 0 ) Large Clockwise ( r, -1 )
        , pathA ( 0, 0 ) Small Clockwise ( r, 0 )
        ]



-- text prevents the event handlers from working,
-- so put the clickable path last if possible
-- (this will bite later when I add fills)
-- paths fills need to be transparent not none to get clicked.

renderDragZone : a -> List (Svg.Svg Msg)
renderDragZone model =
    [ Svg.path
        [ Attr.d <| circlePath 22
        , Attr.fill "transparent"
        , Attr.stroke "black"
        , Mouse.onDown (MouseDown 0)
        ]
        []
    ]
