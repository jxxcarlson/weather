module Main exposing (main)

import Html exposing (Html, button, div, input, table, td, text, tr, a)
import Html.Attributes exposing (placeholder, style, type_, href, target)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, float, int, map2, map3, map4, map5, string)


-- http://crossingtheruby.com/2015/11/11/minimum-viable-elm-view.html


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { weather : Maybe Weather
    , message : String
    , location : String
    , apiKey : String
    , temperatureScale : TemperatureScale
    }


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


type Msg
    = NoOp
    | GetWeather
    | NewWeather (Result Http.Error Weather)
    | SetLocation String
    | SetApiKey String
    | SetToCentigrade
    | SetToFahrenheit


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


view : Model -> Html Msg
view model =
    div [ mainStyle ]
        [ div [ innerStyle ]
            [ weatherTable model.weather model.temperatureScale
            , setLocationInput model
            , getWeatherButton model
            , setTemperatureControl model
            , messageLine model
            , setApiKeyInput model
            , getApiKeyLink
            ]
        ]


mainStyle =
    style
        [ ( "margin", "15px" )
        , ( "margin-top", "20px" )
        , ( "background-color", "#eee" )
        , ( "width", "200px" )
        ]


innerStyle =
    style [ ( "padding", "15px" ) ]


setLocationInput model =
    div [ style [ ( "margin-bottom", "10px" ) ] ] [ input [ type_ "text", placeholder "Location", onInput SetLocation ] [] ]


setApiKeyInput model =
    div [ style [ ( "margin-top", "20px" ) ] ] [ input [ type_ "password", placeholder "Api key", onInput SetApiKey ] [] ]


getWeatherButton model =
    div [ style [ ( "margin-bottom", "0px" ) ] ] [ button [ onClick GetWeather ] [ text "Get weather" ] ]


setTemperatureControl model =
    div []
        [ setToCentigrade model
        , setToFahrenheit model
        ]


centigradeStyle model =
    case model.temperatureScale of
        Centigrade ->
            style [ ( "background-color", "black" ), ( "color", "white" ) ]

        Fahrenheit ->
            style [ ( "background-color", "#888" ), ( "color", "white" ) ]


fahrenheitStyle model =
    case model.temperatureScale of
        Centigrade ->
            style [ ( "background-color", "#888" ), ( "color", "white" ) ]

        Fahrenheit ->
            style [ ( "background-color", "black" ), ( "color", "white" ) ]


setToCentigrade model =
    button [ onClick SetToCentigrade, centigradeStyle model ] [ text "C" ]


setToFahrenheit model =
    button [ onClick SetToFahrenheit, fahrenheitStyle model ] [ text "F" ]


getApiKeyLink =
    div
        [ style [ ( "margin-top", "8px" ) ] ]
        [ a
            [ href "https://openweathermap.org/price", target "_blank" ]
            [ text "get Api key" ]
        ]


messageLine model =
    div [ style [ ( "margin-bottom", "10px" ) ] ] [ text model.message ]



-- text <| toString <| (decodeString weatherDecoder data)
{- WEATHER DISPLAY -}


weatherTable : Maybe Weather -> TemperatureScale -> Html msg
weatherTable maybeWeather temperatureScale =
    case maybeWeather of
        Just weather ->
            realWeatherTable weather temperatureScale

        Nothing ->
            noWeatherTable


noWeatherTable : Html msg
noWeatherTable =
    div [ style [ ( "margin-bottom", "20px" ) ] ]
        [ text "No weather data available" ]


realWeatherTable : Weather -> TemperatureScale -> Html msg
realWeatherTable weather temperatureScale =
    table [ style [ ( "margin-bottom", "20px" ) ] ]
        [ locationRow weather
        , temperatureRow weather temperatureScale
        , humidityRow weather
        , pressureRow weather
        ]


locationRow : Weather -> Html msg
locationRow weather =
    tr []
        [ td [] [ text "Location" ]
        , td [ style [ ( "padding-left", "20px" ) ] ]
            [ text <| weather.name ]
        ]


temperatureRow : Weather -> TemperatureScale -> Html msg
temperatureRow weather temperatureScale =
    tr []
        [ td [] [ text "Temp" ]
        , td [ style [ ( "padding-left", "20px" ) ] ]
            [ text <| temperatureString weather temperatureScale ]
        ]


temperatureString weather temperatureScale =
    case temperatureScale of
        Centigrade ->
            (toString <| toCentigrade <| weather.main.temp) ++ " C"

        Fahrenheit ->
            (toString <| toFahrenheit <| toCentigrade <| weather.main.temp) ++ " F"


humidityRow : Weather -> Html msg
humidityRow weather =
    tr []
        [ td [] [ text "Humidity" ]
        , td [ style [ ( "padding-left", "20px" ) ] ]
            [ text <| toString <| weather.main.humidity ]
        ]


pressureRow : Weather -> Html msg
pressureRow weather =
    tr []
        [ td [] [ text "Pressure" ]
        , td [ style [ ( "padding-left", "20px" ) ] ]
            [ text <| toString <| weather.main.pressure ]
        ]


getNewWeatherByCity : String -> String -> Cmd Msg
getNewWeatherByCity city apiKey =
    let
        url =
            "https://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&APPID=" ++ apiKey

        request =
            Http.get url weatherDecoder
    in
        Http.send NewWeather request


toCentigrade : Float -> Float
toCentigrade kelvin =
    kelvin - 273.15 |> round |> toFloat


toFahrenheit : Float -> Float
toFahrenheit centigrade =
    1.8 * centigrade + 32



{- DECODERS -}


weatherDecoder : Decoder Weather
weatherDecoder =
    map3 Weather
        (field "id" int)
        (field "name" string)
        (field "main" mainDecoder)


mainDecoder : Decoder Main
mainDecoder =
    map5 Main
        (field "temp" float)
        (field "humidity" float)
        (field "pressure" float)
        (field "temp_max" float)
        (field "temp_min" float)


data =
    """{"coord":{"lon":139,"lat":35},
"sys":{"country":"JP","sunrise":1369769524,"sunset":1369821049},
"weather":[{"id":804,"main":"clouds","description":"overcast clouds","icon":"04n"}],
"main":{"temp":289.5,"humidity":89,"pressure":1013,"temp_min":287.04,"temp_max":292.04},
"wind":{"speed":7.31,"deg":187.002},
"rain":{"3h":0},
"clouds":{"all":92},
"dt":1369824698,
"id":1851632,
"name":"Shuzenji",
"cod":200}"""
