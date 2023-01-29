module HomePage exposing (..)

-- modules génériques
import Browser
import Html exposing (..)

-- modules personnalisés
import Utils exposing (..)
import View exposing (..)
import Updates exposing (..)

-- MAIN

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- UPDATE 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = Updates.updatePage msg model

-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW 

view : Model -> Html Msg
view model = div [] [View.viewPage model]