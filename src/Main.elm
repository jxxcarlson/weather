port module Main exposing (main)

{- This app retrieves and displays weather data from openweathermap.org. -}

import Decoder exposing (weatherDecoder)
import Html
import Http
import Types exposing (Model, Msg(..), TemperatureScale(..), Status(..))
import View exposing (view)


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
            doRestoreApiKey apiKey model


{-| Data request for openweathermap.org
-}
getNewWeatherByCity : String -> String -> Cmd Msg
getNewWeatherByCity city apiKey =
    let
        url =
            "https://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&APPID=" ++ apiKey

        request =
            Http.get url weatherDecoder
    in
        Http.send NewWeather request



{- Helpers for managing a copy of the apiKey in local storage. -}


saveApiKey apiKey =
    sendApiKey apiKey


doRestoreApiKey apiKey model =
    let
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



{- Outbound ports -}


port sendApiKey : String -> Cmd msg


port sendRequest : String -> Cmd msg



{- Inbound ports -}


port restoreApiKey : (String -> msg) -> Sub msg
