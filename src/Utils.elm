module Utils exposing(..)

import Html exposing (..)
import Http
import Json.Decode exposing (..)


type State = Failure String | Loading | Success

type alias Model = 
    {
        http_state : State 
        ,json_state : State 
        ,dico : Dictionary
        ,user_input : String
        ,mot_cherche : String
        ,all_the_words : List String
        ,show_answer : Bool
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
   Model Loading Loading [] "" "" [] False,
   getWord
  )

type Msg = Change String | GotDictionary (Result Http.Error Dictionary) | GotHttp (Result Http.Error String) | Word_number Int | GetAnswer | Abandon

toString : Http.Error -> String 
toString erreur = 
  case erreur of 
    Http.BadUrl err -> "BadUrl" ++ err
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.BadStatus err -> "BadStatus" ++ String.fromInt err
    Http.BadBody err -> "BadBody" ++ err

getElementAtIndex : List a -> Int -> Maybe a
getElementAtIndex list index =
    if index < 0 || index >= List.length list then
        Nothing
    else
        List.head (List.drop index list)


getDictionary : Model -> Cmd Msg
getDictionary model =
    Http.get
      { url = urlDef model
      , expect = Http.expectJson GotDictionary dictionaryDecoder
      }

urlDef : Model -> String
urlDef model = ("https://api.dictionaryapi.dev/api/v2/entries/en/hello")


getWord : Cmd Msg
getWord =
    Http.get
      { url = "http://localhost:5016/words.txt"
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



afficheDico : (List Context) -> List (Html Msg)
afficheDico dico =
  case dico of
    [] -> []
    (wordToFind :: xs) -> [li [] ([text "Definitions"] ++ [ul [] (afficheWordMeaning wordToFind.meanings)])] ++ (afficheDico xs)
    
afficheWordMeaning : List Meaning -> List (Html Msg)
afficheWordMeaning meanings =
  case meanings of
    [] -> []
    (wordToFind :: xs) -> [li [] [text wordToFind.partOfSpeech]] ++ [ol [] (afficheDefinition wordToFind.definitions)] ++ (afficheWordMeaning xs)
    
afficheDefinition : List Definition -> List (Html Msg)
afficheDefinition def =
  case def of
    [] -> []
    (wordToFind :: xs) -> [li [] [text wordToFind.definition]] ++ (afficheDefinition xs)  
    
overlay : Model -> List (Html Msg) -> Html Msg
overlay model txt =
  div [] txt