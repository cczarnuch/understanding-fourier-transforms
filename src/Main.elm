import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- module App exposing (..)
-- import GraphicSVG exposing (..)
-- import GraphicSVG.EllieApp exposing (..)


-- main =
--     gameApp Tick
--         { model = init
--         , view = view
--         , update = update
--         , title = "Understanding Fourier Transforms"
--         }
main = Browser.sandbox {init = init, update = update, view = view}

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
    = Add1Up
    | Add1Dn
    | Coeff1Up
    | Coeff1Dn
    | Trnsfm1
    | Trnsfm2
    | Trnsfm3
    | InStep
    | InSine
    | InPulse


-- view model =
--     collage 192 128 (myShapes model)
--
--myShapes model = []
view model =
    div []
    [ button [onClick Add1Up] [text "+"], button [onClick Coeff1Up] [text "+"]
    , div [] [text (String.fromInt model.addition1), text "\t", text (String.fromInt model.coefficient1)]
    , button [onClick Add1Dn] [text "-"], button [onClick Coeff1Dn] [text "-"]
    ]


update msg model =
    case msg of
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

