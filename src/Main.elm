module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import String
import Url.Builder



---- MODEL ----


type Session
    = FailureSession
    | LoadingSession
    | SuccessSession String


type alias Model =
    { resultUrl : String
    , session : Session
    }


init : ( Model, Cmd Msg )
init =
    ( { resultUrl = "", session = LoadingSession }, getFlightSearchSession )



---- UPDATE ----


type Msg
    = MorePlease
    | GotSkyscannerSession (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session } as model) =
    case msg of
        MorePlease ->
            ( { model | session = LoadingSession }
            , getFlightSearchSession
            )

        GotSkyscannerSession result ->
            case result of
                Ok url ->
                    ( { model | session = SuccessSession url, resultUrl = url }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | session = FailureSession }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "旅行アプリを作りたい" ]
        , viewSession model.session
        ]


viewSession : Session -> Html Msg
viewSession session =
    case session of
        FailureSession ->
            div []
                [ text "セッションの取得に失敗しました。"
                , button [ class "catButton", onClick MorePlease ] [ text "Try Again!" ]
                ]

        LoadingSession ->
            div [ class "loading" ]
                [ text "Loading..." ]

        SuccessSession url ->
            div [ class "catWrapper" ]
                [ button [ onClick MorePlease, class "catButton" ] [ text "まずセッションより始めよ" ]
                , div [] [ text url ]
                ]



-- HTTP


getFlightSearchSession : Cmd Msg
getFlightSearchSession =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "x-rapidapi-host" "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
            , Http.header "x-rapidapi-key" "SECRET KEY"
            ]
        , url = "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/pricing/v1.0"
        , body =
            Http.stringBody "application/x-www-form-urlencoded"
                (String.dropLeft 1
                    (Url.Builder.toQuery
                        [ Url.Builder.string "inboundDate" "2020-01-05"
                        , Url.Builder.string "children" "0"
                        , Url.Builder.string "infants" "0"
                        , Url.Builder.string "country" "JP"
                        , Url.Builder.string "currency" "JPY"
                        , Url.Builder.string "locale" "ja-JP"
                        , Url.Builder.string "originPlace" "TYOA-sky"
                        , Url.Builder.string "destinationPlace" "NYCA-sky"
                        , Url.Builder.string "outboundDate" "2019-12-28"
                        , Url.Builder.string "adults" "1"
                        ]
                    )
                )
        , expect = expectJson GotSkyscannerSession sessionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectJson : (Result Http.Error String -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    metadata.headers
                        |> Dict.get "location"
                        |> Result.fromMaybe (Http.BadStatus 403)


sessionDecoder : Decoder String
sessionDecoder =
    field "location" string



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
