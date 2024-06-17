module Main exposing (main)

import Browser
import Http
import Html exposing (Html, div, text)
import Bootstrap.Button as BButton

type Model
    = Initial
    | Error (String)
    | Loaded (String)

type Msg
    = Response (Result Http.Error String)
    | ButtonClicked

postRequest : String -> Cmd Msg
postRequest url =
    Http.post
        { body = Http.emptyBody
        , url = url
        , expect = Http.expectString Response
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    ButtonClicked -> ( model, Cmd.batch [
        postRequest "http://localhost:3000/hello" ])
    Response (Ok res) -> ( Loaded res, Cmd.none )
    Response (Err _) -> ( Error "POST request failed", Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( Initial, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

view : Model -> Html Msg
view m =
    case m of
        Initial -> div [] [ BButton.button [ BButton.onClick (ButtonClicked), BButton.primary ] [ text "Click me" ] ]
        Error err -> div [] [ text ("Error: " ++ err) ]
        Loaded resp -> div [] [ text ("Response: " ++ resp) ]
