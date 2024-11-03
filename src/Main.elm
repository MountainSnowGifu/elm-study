module Main exposing (..)

import Browser
import Html
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


initModel : Model
initModel =
    { message = "Welcome elm2 "
    , firstname = Nothing
    , age = Nothing
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , update = update
        , view = view
        }


type Msg
    = MsgSurprise
    | MsgReset
    | MsgNewName String
    | MsgNewAgeAsString String


type alias Model =
    { message : String
    , firstname : Maybe String
    , age : Maybe Int
    }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewMessage model.message
        , viewFirstnameInput model.firstname
        , viewAgeInput model.age
        , viewSurpriseButton
        , viewResetButton
        , viewLength model.firstname
        ]


viewMessage : String -> Html.Html Msg
viewMessage message =
    Html.text message


viewFirstnameInput : Maybe String -> Html.Html Msg
viewFirstnameInput firstname =
    Html.input [ onInput MsgNewName, value (Maybe.withDefault "" firstname) ] []


viewAgeInput : Maybe Int -> Html.Html Msg
viewAgeInput age =
    Html.input [ onInput MsgNewAgeAsString, value (String.fromInt (Maybe.withDefault 0 age)) ] []


viewSurpriseButton : Html.Html Msg
viewSurpriseButton =
    Html.button [ onClick MsgSurprise ] [ Html.text "Suprise" ]


viewResetButton : Html.Html Msg
viewResetButton =
    Html.button [ onClick MsgReset ] [ Html.text "Reset" ]


viewLength : Maybe String -> Html.Html Msg
viewLength firstname =
    Html.text (String.fromInt (String.length (Maybe.withDefault "" firstname)))


update : Msg -> Model -> Model
update msg model =
    case msg of
        MsgSurprise ->
            case model.age of
                Just anAge ->
                    case model.firstname of
                        Just aName ->
                            { model
                                | message = "Happy Birthday " ++ aName ++ " with " ++ String.fromInt anAge ++ " years old !!"
                            }

                        Nothing ->
                            { model
                                | message = "The firstname is required"
                            }

                Nothing ->
                    { model
                        | message = "The age is required"
                    }

        MsgReset ->
            initModel

        MsgNewName newName ->
            if String.trim newName == "" then
                { model
                    | firstname = Nothing
                }

            else
                { model
                    | firstname = Just newName
                }

        MsgNewAgeAsString newValue ->
            case String.toInt newValue of
                Just anInt ->
                    { model
                        | age = Just anInt
                    }

                Nothing ->
                    { model
                        | message = "The age is wrong"
                        , age = Nothing
                    }
