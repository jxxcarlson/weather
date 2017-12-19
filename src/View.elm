module View exposing (view)

import Html exposing (Html, button, div, input, table, td, text, tr, a)
import Html.Attributes exposing (placeholder, style, type_, href, target)
import Html.Events exposing (onClick, onInput)
import Http
import Types exposing (Model, Msg(..), TemperatureScale(..), Weather)


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
            (toString <| toFahrenheit <| weather.main.temp) ++ " F"


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


toCentigrade : Float -> Float
toCentigrade kelvin =
    kelvin - 273.15 |> round |> toFloat


toFahrenheit : Float -> Float
toFahrenheit kelvin =
    1.8 * (kelvin - 273.15) + 32 |> round |> toFloat
