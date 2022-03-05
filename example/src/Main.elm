module Main exposing (..)

import Browser
import Html exposing (Html, a, div, fieldset, form, h2, input, label, node, p, pre, small, text)
import Html.Attributes exposing (checked, class, href, name, rel, style, type_, value)
import Html.Events exposing (onInput)
import QRCode
import TOTP
import TOTP.Algorithm
import TOTP.Key
import Task
import Time


type alias Flags =
    {}


type alias Model =
    { now : Time.Posix
    , form : Form
    }


type alias Form =
    { issuer : String
    , user : String
    , rawSecret : String
    , outputLength : String
    , periodSeconds : String
    , algorithm : String
    }


parseForm : Form -> Result String TOTP.Key.Key
parseForm form =
    TOTP.Key.init
        { issuer = form.issuer
        , user = form.user
        , rawSecret = form.rawSecret
        , outputLength = String.toInt form.outputLength
        , periodSeconds = String.toInt form.periodSeconds
        , algorithm =
            TOTP.Algorithm.fromString form.algorithm
                |> Maybe.withDefault TOTP.Algorithm.SHA1
        }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { now = Time.millisToPosix 0
      , form =
            { issuer = "issuer"
            , user = "user"
            , rawSecret = "secret"
            , outputLength = "6"
            , periodSeconds = "30"
            , algorithm = TOTP.Algorithm.toString TOTP.Algorithm.SHA1
            }
      }
    , Task.perform OnTick Time.now
    )


type Msg
    = OnTick Time.Posix
    | OnIssuer String
    | OnUser String
    | OnRawSecret String
    | OnOutputLength String
    | OnPeriodSeconds String
    | OnAlgorithm String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick t ->
            ( { model | now = t }
            , Cmd.none
            )

        OnIssuer s ->
            ( { model | form = (\form -> { form | issuer = s }) model.form }
            , Cmd.none
            )

        OnUser s ->
            ( { model | form = (\form -> { form | user = s }) model.form }
            , Cmd.none
            )

        OnRawSecret s ->
            ( { model | form = (\form -> { form | rawSecret = s }) model.form }
            , Cmd.none
            )

        OnOutputLength s ->
            ( { model | form = (\form -> { form | outputLength = s }) model.form }
            , Cmd.none
            )

        OnPeriodSeconds s ->
            ( { model | form = (\form -> { form | periodSeconds = s }) model.form }
            , Cmd.none
            )

        OnAlgorithm s ->
            ( { model | form = (\form -> { form | algorithm = s }) model.form }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        qrcodeOutput =
            case parseForm model.form of
                Err err ->
                    text ("Error: " ++ err)

                Ok key ->
                    let
                        expires =
                            TOTP.Key.expiresIn key model.now

                        color =
                            if expires <= 5 then
                                "red"

                            else
                                "black"
                    in
                    case QRCode.fromString (TOTP.Key.toString key) of
                        Ok qrcode ->
                            div
                                [ style "text-align" "center"
                                ]
                                [ QRCode.toSvg [] qrcode
                                , small []
                                    [ pre [ style "white-space" "pre-wrap" ] [ text (TOTP.Key.toString key) ]
                                    ]
                                , h2 [ style "color" color ]
                                    [ case TOTP.Key.code model.now key of
                                        Ok str ->
                                            text str

                                        Err err ->
                                            text err
                                    ]
                                , p [ style "color" color ]
                                    [ text ("Expires in: " ++ String.fromInt expires) ]
                                ]

                        Err err ->
                            text ("Error: " ++ Debug.toString err)
    in
    div []
        [ div
            [ style "display" "flex"
            ]
            [ node "link"
                [ rel "stylesheet"
                , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
                ]
                []
            , node "style"
                []
                [ text "small { display: block; margin-top: -0.5em; font-size: 0.7em; }"
                , text "label.checkbox, label.radio { display: block; }"
                ]
            , div [ style "flex" "50%", style "margin" "1em" ] [ formOutput model.form ]
            , div [ style "flex" "50%", style "margin" "1em" ] [ qrcodeOutput ]
            ]
        , p [ style "margin" "1em" ]
            [ a [ href "https://github.com/choonkeat/elm-totp" ] [ text "https://github.com/choonkeat/elm-totp" ] ]
        ]


formOutput : Form -> Html Msg
formOutput form =
    Html.form []
        [ fieldset []
            [ p []
                [ label []
                    [ text "Issuer"
                    , p [] [ input [ onInput OnIssuer, value form.issuer ] [] ]
                    ]
                ]
            , p []
                [ label []
                    [ text "User"
                    , p [] [ input [ onInput OnUser, value form.user ] [] ]
                    ]
                ]
            , p []
                [ label []
                    [ text "Secret"
                    , p [] [ input [ onInput OnRawSecret, value form.rawSecret ] [] ]
                    ]
                ]
            , p []
                [ label []
                    [ text "Number of digits"
                    , p []
                        [ input [ onInput OnOutputLength, value form.outputLength, type_ "number" ] []
                        , small [] [ text "Standard is 6 digits" ]
                        ]
                    ]
                ]
            , p []
                [ label []
                    [ text "Duration in seconds"
                    , p []
                        [ input [ onInput OnPeriodSeconds, value form.periodSeconds, type_ "number" ] []
                        , small [] [ text "Standard is 30 seconds" ]
                        ]
                    ]
                ]
            , p []
                [ label []
                    [ text "Algorithm"
                    , p []
                        ([ TOTP.Algorithm.SHA1
                         , TOTP.Algorithm.SHA256
                         , TOTP.Algorithm.SHA512
                         ]
                            |> List.map
                                (\alg ->
                                    let
                                        str =
                                            TOTP.Algorithm.toString alg
                                    in
                                    label [ class "radio" ]
                                        [ input
                                            [ onInput OnAlgorithm
                                            , value str
                                            , type_ "radio"
                                            , checked (str == form.algorithm)
                                            , name "Algorithm"
                                            ]
                                            []
                                        , text (" " ++ str ++ " ")
                                        ]
                                )
                        )
                    , small [] [ text "Standard is SHA1" ]
                    ]
                ]
            , p []
                [ small [] [ a [ href "https://github.com/google/google-authenticator/wiki/Key-Uri-Format#algorithm" ] [ text "NOTE: Google Authenticator only works with standard values" ] ] ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 OnTick
