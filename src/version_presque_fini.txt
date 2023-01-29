module HomePage exposing (..)

-- ICI FAIRE LA REQUETE Http

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Random




-- MAIN

main = Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}

-- MODEL 

type State = Failure String | Loading | Success String 

type alias Model = 
    {
        http : State 
        ,json : State 
        ,dico : Dictionary
        ,content : String
        ,mot_cherche : String
        ,all_the_words : List String
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
   Model Loading Loading [] "" "" [],
   getHttp
  )


-- UPDATE 

type Msg = Change String | GotDictionary (Result Http.Error Dictionary) | GotHttp (Result Http.Error String) | Word_number Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        GotDictionary result ->
            case result of
                Ok dictionary ->
                    ({ model | json = Success "", dico = dictionary} , Cmd.none)
                Err error ->
                    ({model | json = Failure (toString error)}, Cmd.none)
        GotHttp result ->
            case result of 
                Ok words_txt ->
                    ({model | http = Success "",  all_the_words = String.split " " words_txt  }, Random.generate Word_number (Random.int 1 1000))
                Err error ->
                    ({model | http = Failure (toString error)}, Cmd.none)

        Change newContent ->
            ({ model | content = newContent }, Cmd.none)

        Word_number index -> 
            case (getElementAtIndex model.all_the_words index) of
                  Nothing -> 
                      (model, Cmd.none)
                  Just word_at_index -> 
                      ({ model | mot_cherche = word_at_index}, getDictionary model)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model = Html.form []
    [ header,

    div []
    [ h1 [] [ text (urlDef model)]
    , viewWord model
    ], 

    p [] [ text "\n "],
      if model.content == model.mot_cherche then 
        div []
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
            p [] [ text "\n "]
          , div [] [ h1 [ ]  [text ("BRAVO !! c'est bien ça")]]
          ]
      else if model.content == "" then 
        div []
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
        p [] [ text "\n "]
          , div [] [ text ("Veuillez insérer la réponse dans le champ ci-dessus")]
          ]
      else
        div []
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
            p [] [ text "\n "]
          , div []  [text ("Ce n'est pas la bonne réponse")]
          ],
    p [] [ text "\n "]
    , footer
    ]

viewWord : Model -> Html Msg
viewWord model =
  case model.http of
    Failure error -> text ("Error while loading the words : "  ++ error)
    Loading -> text "Fetching the http datas..."
    Success good -> overlay model (
      case model.json of
        Success veryGood -> textDatas model.dico
        Loading -> [text "Fetching the json datas..."]
        Failure error -> [text ("Error while loading the defintion : "  ++ error)] )
            
header = div [ class "top_banner" ]
        [ h1 [] [ text "Bienvenue à devine-mot !" ]
        , p []
            [ text "Le but du jeu est de deviner le mot dont la définition est donnée ci-dessous. "],
             strong [] [ text "Bonne Chance !" ]
        ]

definition = div [ class "definition" ]
        [ h1 [] [ text "C'est parti !" ], 
        p [] [ text "C'est un mot gentil pour saluer"]
        ]

game_body = div [ class "game_body"]
        [ 
          p [] [ text "\n "],
          input [ placeholder "le mot est : ", type_ "word" ]
            [],
          button [] 
            [ text "Submit" ],
          p [] [ text "\n "]
        ]

footer = div [ class "footer"]
        [
          p [] [ text "Toutes les définitions sont tirées du site :"],
          a [] [ text "https://dictionaryapi.dev/"]
        ] 


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


getHttp : Cmd Msg
getHttp =
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