module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, at, field)
import List
import Maybe
import String
import Url.Builder



---- MODEL ----


type Session
    = FailureSession
    | LoadingSession
    | SuccessSession String


type Fetch
    = FailureFetch
    | LoadingFetch
    | WaitingFetch
    | SuccessFetch


type alias Itinerary =
    { price : Float
    , deeplinkUrl : String
    }


type alias Model =
    { fetchUrl : String
    , session : Session
    , fetch : Fetch
    , itineraries : List Itinerary
    }


init : ( Model, Cmd Msg )
init =
    ( { fetchUrl = ""
      , session = LoadingSession
      , fetch = WaitingFetch
      , itineraries = []
      }
    , getFlightSearchSession
    )



---- UPDATE ----


type Msg
    = MorePlease
    | GotSkyscannerSession (Result Http.Error String)
    | GotSkyscannerFetch (Result Http.Error (List Itinerary))


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
                    ( { model | session = SuccessSession url, fetchUrl = url }
                    , getFlightSearchFetch url
                    )

                Err _ ->
                    ( { model | session = FailureSession }
                    , Cmd.none
                    )

        GotSkyscannerFetch result ->
            case result of
                Ok itineraries ->
                    ( { model | fetch = SuccessFetch, itineraries = itineraries }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | fetch = FailureFetch }
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
        , viewFetch model.fetch
        , ul [ class "itineraryWrapper" ] (List.map viewItinerary model.itineraries)
        ]


viewSession : Session -> Html Msg
viewSession session =
    case session of
        FailureSession ->
            div []
                [ text "セッションの取得に失敗しました。" ]

        LoadingSession ->
            div [ class "label" ]
                [ text "セッションを取得中..." ]

        SuccessSession url ->
            div [] []


viewFetch session =
    case session of
        FailureFetch ->
            div [ class "label" ]
                [ text "結果の取得に失敗しました。" ]

        LoadingFetch ->
            div [ class "label" ]
                [ text "結果を取得中..." ]

        WaitingFetch ->
            div [ class "label" ]
                [ text "セッション取得後に結果取得が開始されます。" ]

        SuccessFetch ->
            div [ class "label" ]
                [ text "冬休みの東京 ⇔ ニューヨーク往復航空券のお値段" ]


viewItinerary : Itinerary -> Html Msg
viewItinerary itinerary =
    li [ class "itineraryLink" ]
        [ a
            [ href itinerary.deeplinkUrl
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text <| String.fromFloat itinerary.price ++ " JPY" ]
        ]



-- HTTP


getFlightSearchSession : Cmd Msg
getFlightSearchSession =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "x-rapidapi-host" "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
            , Http.header "x-rapidapi-key" "MY SUPER SECRET KEY"
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
        , expect = expectResponseHeader GotSkyscannerSession sessionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectResponseHeader : (Result Http.Error String -> msg) -> Decoder a -> Http.Expect msg
expectResponseHeader toMsg decoder =
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
    field "location" Json.Decode.string


getFlightSearchFetch : String -> Cmd Msg
getFlightSearchFetch url =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "x-rapidapi-host" "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
            , Http.header "x-rapidapi-key" "MY SUPER SECRET KEY"
            ]
        , url =
            Url.Builder.crossOrigin
                "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/pricing/uk2/v1.0"
                [ Maybe.withDefault "" (String.split "/" url |> List.reverse |> List.head) ]
                [ Url.Builder.int "pageIndex" 0, Url.Builder.int "pageSize" 20 ]
        , body = Http.emptyBody
        , expect = Http.expectJson GotSkyscannerFetch itinerariesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


itinerariesDecoder : Decoder (List Itinerary)
itinerariesDecoder =
    field "Itineraries" (Json.Decode.list itineraryDecoder)


itineraryDecoder : Decoder Itinerary
itineraryDecoder =
    Json.Decode.map2 Itinerary
        (at [ "PricingOptions", "0", "Price" ] Json.Decode.float)
        (at [ "PricingOptions", "0", "DeeplinkUrl" ] Json.Decode.string)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
