module Main exposing (..)
import Browser
-- import Html exposing (Html, button, div, text)
-- import Html.Events exposing (onClick)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


main =
    gameApp Tick
        { model = init
        , view = view
        , update = update
        , title = "Understanding Fourier Transforms"
        }


type FTrnsfms
    = Type1
    | Type2
    | Type3


type Inputs
    = Step
    | Sine
    | Pulse

type Pulse
    = ON
    | OFF

init =
    { time = 0
    , ct = 0
    , trnsfm = Type1
    , tboxx = -260
    , tboxy = 50
    , inboxx = 60
    , inboxy = 50
    , addition1 = 0
    , addition2 = 1
    , coefficient1 = 1
    , input = Step

    -- Constant variables
    , maxAddition1 = 40
    , maxAddition2 = 40
    , maxCoefficient1 = 10

    -- Function lists
    , inputTime = []
    , outputTime = []
    , graphLength = 200
    }


type Msg
    = Tick Float GetKeyState
    | Add1Up
    | Add1Dn
    | Add2Up
    | Add2Dn
    | Coeff1Up
    | Coeff1Dn
    | Trnsfm1
    | Trnsfm2
    | Trnsfm3
    | InStep
    | InSine
    | InPulse

myShapes model = [
    -- Borders and lines
    rect 640 480
        |> outlined (solid 2) black,
    rect 640 1
        |> filled black,
    rect 1 240
        |> filled black
        |> move (0,120),

    -- Top left section
    text "1. Create your transfer function!"
        |> filled black
        |> scale 2
        |> move (-315,215),
    arrows model.addition1 Add1Up Add1Dn
        |> move (-260,150),
    text "+"
        |> filled black
        |> scale 2
        |> move (-240,150),
    -- arrows model.addition2 Add2Up Add2Dn
    --     |> move (-200,150),
    selectionBox1 model,
    button model "s/s+a" Trnsfm1
        |> move (-260,50),
    button model "a/s+a" Trnsfm2
        |> move (-160,50),
    button model "s/s^2+a" Trnsfm3
        |> move (-60,50),
    changeFuncDisplay model,
    
    -- Top Right section
    text "2. Select your input:"
        |> filled black
        |> scale 2
        |> move (70, 215),
    rect 100 100
        |> outlined (solid 2) black
        |> move (160,140),
    selectionBox2 model,
    button model "step" InStep
        |> move (60,50),
    button model "wave" InSine
        |> move (160,50),
    button model "pulse" InPulse
        |> move (260,50),
    changeInDisplay model,

    -- Bottom section
    rect 140 200
        |> outlined (solid 1) black
        |> move (0,-120),
    text "G(s):"
        |> filled black
        |> scale 2
        |> move (-25,-50),
    -- Graphs
    group
        [ group (inputGraphTime model) |> move ( -150 , -190 )
        , group (outputGraphTime model) |> move ( 220, -190 )
        ]
        |> move ( -140, 80 )
    ]


view model =
    collage 640 480 (myShapes model)


update msg model =
    case msg of
        Tick t _ ->
            let
                -- Elapsed time
                et = (t - model.ct)

                inputGraph =
                    case model.input of
                        Step ->
                            if et < 5 then
                                25
                            else
                                0
                        Sine ->
                            25 * sin (et)
                        Pulse ->
                            if  modBy 2 (floor (et)) == 0 then
                                25
                            else
                                0
                outputGraph =
                    case model.trnsfm of
                        Type1 ->
                            case model.input of
                                Step ->
                                    if et < 5 then
                                        25 * (e^(-et*toFloat(model.addition2)))
                                    else
                                        0
                                Sine ->
                                    25 * ((1/2)*cos(toFloat(model.addition2)*et) + (1/2)*sin(toFloat(model.addition2)*et) - (1/2)*(e^(-et*toFloat(model.addition2))))
                                Pulse ->
                                    if  modBy 2 (floor (et)) == 0 then
                                        25 * (e^(-et*toFloat(model.addition2)))
                                    else
                                        0
                        Type2 ->
                            case model.input of
                                Step ->
                                    if et < 5 then
                                        25 * (1 - e^(-et*toFloat(model.addition2)))
                                    else
                                        0
                                Sine ->
                                    25 * (-(1/2)*cos(toFloat(model.addition2)*et) + (1/2)*sin(toFloat(model.addition2)*et) + (1/2)*(e^(-et*toFloat(model.addition2))))
                                Pulse ->
                                    if  modBy 2 (floor (et)) == 0 then
                                        25 * (1 - e^(-et*toFloat(model.addition2)))
                                    else
                                        0
                        Type3 ->
                            case model.input of
                                Step ->
                                    if et < 5 then
                                        25 * sin(et*toFloat(model.addition2))
                                    else
                                        0
                                Sine ->
                                    0
                                Pulse ->
                                    if  modBy 2 (floor (et)) == 0 then
                                        25 * sin(et*toFloat(model.addition2))
                                    else
                                        0
                inputGraphPoint =
                    (0, inputGraph, rgb 0 0 0)

                outputGraphPoint =
                    (0, outputGraph, rgb 0 0 0)
            in
            { model
                | time = t
                , inputTime =
                    List.take 2470
                        ([ inputGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.graphLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                            model.inputTime
                        )
                , outputTime =
                    List.take 2470
                        ([ outputGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.graphLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                            model.outputTime
                        )
            }
        Add1Up ->
            { model
                | addition1 =
                    if model.addition1 < model.maxAddition1 then
                        model.addition1 + 1
                    else
                        model.addition1
            }

        Add1Dn ->
            { model
                | addition1 =
                    if model.addition1 > -model.maxAddition1 then
                        model.addition1 - 1
                    else
                        model.addition1
            }

        Add2Up ->
            { model
                | addition2 =
                    if model.addition2 < model.maxAddition2 then
                        model.addition2 + 1
                    else
                        model.addition2
            }

        Add2Dn ->
            { model
                | addition2 =
                    if model.addition2 > -model.maxAddition2 then
                        model.addition2 - 1
                    else
                        model.addition2
            }

        Coeff1Up ->
            { model
                | coefficient1 =
                    if model.coefficient1 < model.maxCoefficient1 then
                        model.coefficient1 + 1
                    else
                        model.coefficient1
            }

        Coeff1Dn ->
            { model
                | coefficient1 =
                    if model.coefficient1 > -model.maxCoefficient1 then
                        model.coefficient1 - 1
                    else
                        model.coefficient1
            }

        Trnsfm1 ->
            { model
                | trnsfm = Type1
                , tboxx = -260
                , tboxy = 50
                , ct = model.time
            }

        Trnsfm2 ->
            { model
                | trnsfm = Type2
                , tboxx = -160
                , tboxy = 50
                , ct = model.time
            }

        Trnsfm3 ->
            { model
                | trnsfm = Type3
                , tboxx = -60
                , tboxy = 50
                , ct = model.time
            }

        InStep ->
            { model
                | input = Step
                , inboxx = 60
                , tboxy = 50
                , ct = model.time
            }

        InSine ->
            { model
                | input = Sine
                , inboxx = 160
                , tboxy = 50
                , ct = model.time
            }

        InPulse ->
            { model
                | input = Pulse
                , inboxx = 260
                , tboxy = 50
                , ct = model.time
            }

--show which options are selected for function and input
selectionBox1 model =
    roundedRect 90 70 5
        |> filled (rgba 0 182 255 (0.5 + 0.5 * sin (5 * model.time)))
        |> move (model.tboxx, model.tboxy)

changeFuncDisplay model =
    case model.trnsfm of
        Type1 ->
            func1 model
                |> move (-160,160)
        Type2 ->
            func2 model
                |> move (-160,160)
        Type3 ->
            func3 model
                |> move (-160,160)

selectionBox2 model =
    roundedRect 90 70 5
        |> filled (rgba 0 182 255 (0.5 + 0.5 * sin (5 * model.time-0.5)))
        |> move (model.inboxx, model.inboxy)

changeInDisplay model =
    case model.input of
        Step ->
            stepShape
                |> move (160,140)
        Sine ->
            sineShape
                |> move (160,140)
        Pulse ->
            pulseShape
                |> move (160,140)


-- myShape groups
arrows display up down = group[
            polygon [(-10,0),(0,20),(10,0)]
                |> filled blue
                |> move (0,20)
                |> notifyTap up,
            polygon [(-10,0),(0,-20),(10,0)]
                |> filled blue
                |> move (0,0)
                |> notifyTap down,
            text ("" ++ String.fromInt display)
                |> filled black
                |> scale 1.3
                |> move (-5, 5)
            ]

numGraphPoints model =
    round 2505

inputGraphTime model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.inputTime (List.drop 1 model.inputTime)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)

outputGraphTime model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.outputTime (List.drop 1 model.outputTime)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)

button model display action = group[
            roundedRect 80 60 5
                |> filled gray
                |> notifyTap action,
            text display
                |> centered
                |> filled black
                |> notifyTap action
                |> scale 1.5
            ]


stepShape = group [
            rect 50 1
                |> filled black
                |> move (-25,0),
            rect 1 25
                |> filled black
                |> move (0,12.5),
            rect 50 1
                |> filled black
                |> move (25,25)
            ]

sineShape = curve (-50,0) [Pull (-25,50) (0,0), Pull (25,-50) (50,0) ]
                |> outlined (solid 1) black

pulseShape = group [
            rect 50 1
                |> filled black
                |> move (-25,0),
            rect 1 25
                |> filled black
                |> move (0,12.5),
            rect 25 1
                |> filled black
                |> move (12.5,25),
            rect 1 25
                |> filled black
                |> move (25,12.5),
            rect 25 1
                |> filled black
                |> move (37.5,0)
        ]


func1 model = group [
            text "s"
                |> centered
                |> filled black
                |> scale 2
                |> move (0,10),
            rect 100 1
                |> filled black,
            text "s +"
                |> centered
                |> filled black
                |> scale 2
                |> move (-10,-40),
            arrows model.addition2 Add2Up Add2Dn
                |> move (20,-42.5)
        ]

func2 model = group [
            arrows model.addition2 Add2Up Add2Dn
                |> move (0,15)
                |> scale 0.75,
            rect 100 1
                |> filled black
                |> move (0,-7.5),
            text "s +"
                |> centered
                |> filled black
                |> scale 2
                |> move (-10,-40),
            arrows model.addition2 Add2Up Add2Dn
                |> scale 0.75
                |> move (20,-40)
        ]

func3 model = group [
            text "s"
                |> centered
                |> filled black
                |> scale 2
                |> move (0,10),
            rect 100 1
                |> filled black,
            text "s  +"
                |> centered
                |> filled black
                |> scale 2
                |> move (-10,-40),
            text "2"
                |> centered
                |> filled black
                |> move (-17.5,-30),
            arrows model.addition2 Add2Up Add2Dn
                |> move (20,-42.5)
        ]
