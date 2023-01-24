module HomePage exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Dictionary = List TypeOfMeaning

type alias TypeOfMeaning =
    { word : String
    , meaning : List Meaning
    }

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }

type alias Definition =
    { definition : String
    }

type State = Success String | Failure | Loading

type alias Model = 
    {
          mot_a_deviner : String
        , reponse : String
        , wordsTable : List String
        , httpState : State
        , dictionary : Dictionary
        , dictionaryState : State
    }


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRandomDefinition)



-- UPDATE

type Msg
  = Change String | MorePlease | GotDictionary (Result Http.Error Dictionary)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomDefinition)

    GotDictionary result ->
      case result of
          Ok dictionary -> ({ model | dictionaryState = Success "Ok"} , Cmd.none)
          Err _ -> ({ model | dictionaryState = Failure } , Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Hello definition" ]
    , viewDefinition model
    ]


viewDefinition : Model -> Html Msg
viewDefinition model =
  case model.dictionaryState of
    Failure ->
      div []
        [ text "I could not load a random Definition for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success definition ->
      displayDesc model.dictionary


displayDesc : Dictionary -> List ( Html Msg )
displayDesc list = case list of
    [] -> []
    (x::xs) -> [li [] ([text "Meaning"] ++ [ul [] (displayMeanings x.meaning)])] ++ (displayDesc xs) 

displayMeanings : List Meaning -> List (Html Msg)
displayMeanings list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (displayDefinitions x.definitions)] ++ (displayMeanings xs)

displayDefinitions : List Definition -> List (Html Msg)
displayDefinitions list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.definition]] ++ (displayDefinitions xs)




-- HTTP

getRandomDefinition : Cmd Msg
getRandomDefinition =
  Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"
    , expect = Http.expectJson GotDictionary dictionaryDecoder
    }


-- DECODERS 

dictionaryDecoder : Decoder (List TypeOfMeaning)
dictionaryDecoder = Json.Decode.list typeOfMeaningDecoder 

typeOfMeaningDecoder : Decoder TypeOfMeaning
typeOfMeaningDecoder = 
    map2 TypeOfMeaning
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