module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (..)

viewPage : Model -> Html Msg
viewPage model = Html.form []
    [ header
      ,definition model
      ,game_body model
      ,footer
    ]

viewDefinition : Model -> Html Msg
viewDefinition model =
  case model.http of
    Utils.Failure error -> text ("Error while loading the words : "  ++ error)
    Utils.Loading -> text "Fetching the http datas..."
    Utils.Success -> Utils.overlay model (
      case model.json of
        Utils.Success -> Utils.textDatas model.dico
        Utils.Loading -> [text "Fetching the json datas..."]
        Utils.Failure error -> [text ("Error while loading the defintion : "  ++ error)] )
            
header = div [ class "top_banner" ]
        [ h1 [] [ text "Bienvenue à devine-mot !" ]
        , p []
            [ text "Le but du jeu est de deviner le mot dont la définition est donnée ci-dessous. "],
             strong [] [ text "Bonne Chance !" ]
        ]

definition model = div [class "definition"]
    [ h1 [] [ text (Utils.urlDef model)]
    , viewDefinition model
    ]

game_body model = div [class "game_body"] [p [] [ text "\n "],
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
    p [] [ text "\n "]]

footer = div [ class "footer"]
        [
          p [] [ text "Toutes les définitions sont tirées du site :"],
          a [] [ text "https://dictionaryapi.dev/"]
        ] 