module HomePage exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Random
import Utils exposing (..)
import View exposing (..)
import Updates exposing (..)

-- MAIN

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- UPDATE 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = Updates.updatePage msg model


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model = div [] [View.viewPage model]