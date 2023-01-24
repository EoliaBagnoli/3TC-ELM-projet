module HomePage exposing (..)

-- ICI FAIRE LA REQUETE Http

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)


-- MAIN

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- MODEL 

type Model = 
    Failure | Loading | Success Dictionary

type alias Dictionary = List Context

type alias Context = 
    {
        word : String,
        meanings : List Meaning
    }

type alias Meaning = 
    {
        partOfSpeech : String,
        definitions : List Definition
    }

type alias Definition = 
    {
        definition : String
    }


init : () -> (Model, Cmd Msg)
init _ =
  (
   Loading,
   getDictionary
  )


-- UPDATE 

type Msg = GotDictionary (Result Http.Error Dictionary)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        GotDictionary result ->
            case result of
                Ok dictionary ->
                    (Success dictionary, Cmd.none)
                Err _ ->
                    (Failure, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Hello Definition" ]
    , viewDefinition model
    ]


viewDefinition : Model -> Html Msg
viewDefinition model =
  case model of
    Failure ->
        text "I could not load your definition for some reason. "

    Loading ->
      text "Loading..."

    Success dictionary -> 
        print_word dictionary.affiche_word

print_word : Model -> List(Html Msg) -> Html Msg
print_word model word = 
    div [] 
        [
           [ul [] word]
        ]

affiche_word : Dictionary -> List (Html Msg)
affiche_word list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.word]] ++ (affiche_word xs) 

{- view : Model -> Html Msg
view model = 
    case model of 
        Failure ->
            text "There was a problem encoutered"
        Loading ->
            text "Loading"
        Success dictionary -> 
            text dictionary.meanings.partOfSpeech -}
            



getDictionary : Cmd Msg
getDictionary =
   Http.get
      { url = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"
      , expect = Http.expectJson GotDictionary dictionaryDecoder
      }

dictionaryDecoder : Decoder (List Context)
dictionaryDecoder = 
    Json.Decode.list contextDecoder

contextDecoder : Decoder Context
contextDecoder = 
    map2 Context
        (field "word" string)
        (field "meanings" <| Json.Decode.list meaningDecoder)

meaningDecoder : Decoder Meaning
meaningDecoder = 
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" <| Json.Decode.list definitionDecoder)

definitionDecoder : Decoder Definition
definitionDecoder = 
    Json.Decode.map Definition 
        (field "definition" string)