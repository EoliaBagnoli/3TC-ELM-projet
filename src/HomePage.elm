module HomePage exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model = Html.form []
    [ header,
      definition,
      if model.content == "bonjour" then 
        div []
          [ input [ placeholder "Insérez la réponse", value model.content, onInput Change ] []
          , div [] [ text ("BRAVO !! c'est bien ça")]
          ]
      else if model.content == "" then 
        div []
          [ input [ placeholder "Insérez la réponse", value model.content, onInput Change ] []
          , div [] [ text ("Veuillez insérer la réponse dans le champ ci-dessus")]
          ]
      else
        div []
          [ input [ placeholder "Insérez la réponse", value model.content, onInput Change ] []
          , div [] [ text ("Ce n'est pas la bonne réponse")]
          ]
    , footer
    ]
  

header = div [ class "top_banner" ]
        [ h1 [] [ text "Bienvenue à devine-mot !" ]
        , p []
            [ text "Le but du jeu est de deviner le mot dont la définition est donnée ci-dessous. "],
             strong [] [ text "Bonne Chance !" ]
        ]

definition = div [ class "definition" ]
        [ h1 [] [ text "C'est parti !" ], 
        p [] [ text "C'est un mot gentil pour saluer quelqu'un"]
        ]

game_body = div [ class "game_body"]
        [ 
          p [] [ text "\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n"],
          input [ placeholder "le mot est : ", type_ "word" ]
            [],
          button [] 
            [ text "Submit" ],
          p [] [ text "\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n"]
        ]

footer = div [ class "footer"]
        [
          p [] [ text "Toutes les définitions sont tirées du site :"],
          a [] [ text "https://dictionaryapi.dev/"]
        ] 

