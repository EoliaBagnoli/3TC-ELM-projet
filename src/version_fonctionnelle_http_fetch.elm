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

type State = Failure | Loading | Success String 

type alias Model = 
    {
        http : State 
        ,json : State 
        ,dico : Dictionary
    }

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
   Model Loading Loading [],
   getHttp
  )


-- UPDATE 

type Msg = GotDictionary (Result Http.Error Dictionary) | GotHttp (Result Http.Error String) 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        GotDictionary result ->
            case result of
                Ok dictionary ->
                    ({ model | json = Success "", dico = dictionary} , Cmd.none)
                Err _ ->
                    ({model | json = Failure}, Cmd.none)
        GotHttp result ->
            case result of 
                Ok hello ->
                    ({model | http = Success hello}, getDictionary)
                Err _ ->
                    ({model | http = Failure}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Hello Definition" ]
    , viewWord model
    ]

viewWord : Model -> Html Msg
viewWord model =
  case model.http of
    Failure -> text ("Error while loading the words :'/")
    Loading -> text "Fetching the http datas..."
    Success good -> overlay model (
      case model.json of
        Success veryGood -> textDatas model.dico
        Loading -> [text "Fetching the json datas..."]
        Failure -> [text ("Error while loading the words :'/")] )
            



getDictionary : Cmd Msg
getDictionary =
    Http.get
      { url = "https://api.dictionaryapi.dev/api/v2/entries/en/cat"
      , expect = Http.expectJson GotDictionary dictionaryDecoder
      }

getHttp : Cmd Msg
getHttp =
    Http.get
      { url = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"
      , expect = Http.expectString GotHttp
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



textDatas : (List Context) -> List (Html Msg)
textDatas datas =
  case datas of
    [] -> []
    (wordToFind :: xs) -> [li [] ([text "Definitions"] ++ [ul [] (textWordMeaning wordToFind.meanings)])] ++ (textDatas xs)
    
textWordMeaning : List Meaning -> List (Html Msg)
textWordMeaning meanings =
  case meanings of
    [] -> []
    (wordToFind :: xs) -> [li [] [text wordToFind.partOfSpeech]] ++ [ol [] (textDef wordToFind.definitions)] ++ (textWordMeaning xs)
    
textDef : List Definition -> List (Html Msg)
textDef def =
  case def of
    [] -> []
    (wordToFind :: xs) -> [li [] [text wordToFind.definition]] ++ (textDef xs)  
    
overlay : Model -> List (Html Msg) -> Html Msg
overlay model txt =
  div [] txt