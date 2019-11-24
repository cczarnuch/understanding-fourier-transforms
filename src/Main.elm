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
    { trnsfm = Type1
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
    | Coeff1Up
    | Coeff1Dn
    | Trnsfm1
    | Trnsfm2
    | Trnsfm3
    | InStep
    | InSine
    | InPulse


myShapes model = [
    roundedRect 40 25 5
        |> filled darkGray
        |> move (0,60)
        |> notifyTap Add1Up,
    roundedRect 40 25 5
        |> filled darkGray
        |> move (0,20)
        |> notifyTap Add1Dn,
    text ("" ++ String.fromInt model.addition1)
        |> filled black
        |> move (0, 35)
    ]


view model =
    collage 512 380 (myShapes model)


update msg model =
    case msg of
        Tick f g ->
            model
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
