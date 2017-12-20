port module Main exposing (main)

import Html
import Http
import View exposing (view)
import Types exposing (Model, Msg(..), TemperatureScale(..), Status(..))
import Decoder exposing (weatherDecoder)
import Json.Encode as Encode


-- http://crossingtheruby.com/2015/11/11/minimum-viable-elm-view.html


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { weather = Nothing
      , message = "app started"
      , location = "london"
      , apiKey = ""
      , temperatureScale = Centigrade
      , status = Start
      }
    , sendRequest "getApiKey"
    )


subscriptions model =
    restoreApiKey RestoreApiKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetWeather ->
            ( model, getNewWeatherByCity model.location model.apiKey )

        NewWeather (Ok newData) ->
            ( { model
                | weather = Just newData
                , message = "Successful request"
                , status = Authenticated
              }
            , Cmd.none
            )

        NewWeather (Err error) ->
            ( { model | message = "Error", status = Error }, Cmd.none )

        SetLocation location ->
            ( { model | location = location }, Cmd.none )

        SetApiKey apiKey ->
            ( { model | apiKey = apiKey }, saveApiKey apiKey )

        SetToCentigrade ->
            ( { model | temperatureScale = Centigrade }, Cmd.none )

        SetToFahrenheit ->
            ( { model | temperatureScale = Fahrenheit }, Cmd.none )

        RestoreApiKey apiKey ->
            let
                _ =
                    Debug.log "APIKEY = " apiKey

                status =
                    if apiKey /= "" then
                        Starting
                    else
                        Start

                message =
                    if apiKey == "" then
                        "apiKey: NOT FOUND"
                    else
                        "apiKey found"
            in
                ( { model | apiKey = apiKey, status = status, message = message }, Cmd.none )


getNewWeatherByCity : String -> String -> Cmd Msg
getNewWeatherByCity city apiKey =
    let
        url =
            "https://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&APPID=" ++ apiKey

        request =
            Http.get url weatherDecoder
    in
        Http.send NewWeather request


saveApiKey apiKey =
    sendApiKey apiKey



-- sendApiKey (Encode.object [ ( "apiKey", apiKey ) ])


port sendApiKey : String -> Cmd msg


port sendRequest : String -> Cmd msg


port restoreApiKey : (String -> msg) -> Sub msg
