module Main exposing (main)

import Html
import Http
import View exposing (view)
import Types exposing (Model, Msg(..), TemperatureScale(..))
import Decoder exposing (weatherDecoder)


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
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetWeather ->
            ( model, getNewWeatherByCity model.location model.apiKey )

        NewWeather (Ok newData) ->
            ( { model | weather = Just newData, message = "Successful request" }, Cmd.none )

        NewWeather (Err error) ->
            ( { model | message = "Error" }, Cmd.none )

        SetLocation location ->
            ( { model | location = location }, Cmd.none )

        SetApiKey apiKey ->
            ( { model | apiKey = apiKey }, Cmd.none )

        SetToCentigrade ->
            ( { model | temperatureScale = Centigrade }, Cmd.none )

        SetToFahrenheit ->
            ( { model | temperatureScale = Fahrenheit }, Cmd.none )


getNewWeatherByCity : String -> String -> Cmd Msg
getNewWeatherByCity city apiKey =
    let
        url =
            "https://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&APPID=" ++ apiKey

        request =
            Http.get url weatherDecoder
    in
        Http.send NewWeather request
