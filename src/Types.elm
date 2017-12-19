module Types exposing (..)

import Http exposing (get, send)
import Http


type alias Model =
    { weather : Maybe Weather
    , message : String
    , location : String
    , apiKey : String
    , temperatureScale : TemperatureScale
    }


type Msg
    = NoOp
    | GetWeather
    | NewWeather (Result Http.Error Weather)
    | SetLocation String
    | SetApiKey String
    | SetToCentigrade
    | SetToFahrenheit


type alias Weather =
    { id : Int
    , name : String
    , main : Main
    }


type TemperatureScale
    = Centigrade
    | Fahrenheit


type alias Main =
    { temp : Float
    , humidity : Float
    , pressure : Float
    , temp_min : Float
    , temp_max : Float
    }
