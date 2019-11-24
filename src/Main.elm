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


init =
    { time = 0
    , trnsfm = Type1
    , addition1 = 1
    , addition2 = 1
    , coefficient1 = 1
    , input = Step

    -- Constant variables
    , maxAddition1 = 40
    , maxAddition2 = 40
    , maxCoefficient1 = 10
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
    text "Create your transfer function!"
        |> filled black
        |> scale 2
        |> move (-305, 215),
    arrows model.addition1 Add1Up Add1Dn
        |> move (-300,165),
    arrows model.addition2 Add2Up Add2Dn
        |> move (-270,165),
    
    -- Top Right section
    text "Select your input"
        |> filled black
        |> scale 2
        |> move (80, 215),
    
    -- Bottom section
    rect 140 200
        |> outlined (solid 1) black
        |> move (0,-120),
    text "G(s):"
        |> filled black
        |> scale 2
        |> move (-25,-50)
    ]


view model =
    collage 640 480 (myShapes model)


update msg model =
    case msg of
        Tick t _ ->
            { model
                | time = t
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
            }

        Trnsfm2 ->
            { model
                | trnsfm = Type2
            }

        Trnsfm3 ->
            { model
                | trnsfm = Type3
            }

        InStep ->
            { model
                | input = Step
            }

        InSine ->
            { model
                | input = Sine
            }

        InPulse ->
            { model
                | input = Pulse
            }


arrows display up down = group[
            polygon [(-10,0),(0,20),(10,0)]
                |> filled darkGray
                |> move (0,20)
                |> notifyTap up,
            polygon [(-10,0),(0,-20),(10,0)]
                |> filled darkGray
                |> move (0,0)
                |> notifyTap down,
            text ("" ++ String.fromInt display)
                |> filled black
                |> move (-5, 5)
            ]
