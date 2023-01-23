module HomePage exposing (main)

import Browser 
import Html exposing (..)
import Html.Attributes exposing (..)


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg

main = 
    Browser.element{init =0, update = update, view = view}

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

type alias Model = String

view model = 
         Html.form []
    [ header
    , definition
    , game_body
    , footer
    ]

header = div [ class "top_banner" ]
        [ h1 [] [ text "Bienvenue à devine-mot !" ]
        , p []
            [ text "Le but du jeu est de deviner le mot dont la définition est donnée ci-dessous (et tout ça en anglais). "],
             strong [] [ text "Bonne Chance !" ]
        ]

definition = div [ class "definition" ]
        [ h1 [] [ text "C'est parti !" ], 
        p [] [ text "C'est un mot qui veut dire bite (mais un autre)"],
        a [] [ text "file:///home/eolia/Documents/ELM/index.html"]
        ]

game_body = div []
        [ input [ placeholder "le mot est : ", type_ "word" ]
            []
        ]

footer = div []
        [ button [] 
            [ text "Submit" ]
        ]