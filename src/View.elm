module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (..)
import Bootstrap.Button as Button

viewPage : Model -> Html Msg
viewPage model = Html.form []
    [ header
      ,definition model
      ,game_body model
      ,footer
    ]

viewDefinition : Model -> Html Msg
viewDefinition model =
  case model.http_state of
    Utils.Failure error -> text ("Error while loading the words : "  ++ error)
    Utils.Loading -> text "Fetching the http datas..."
    Utils.Success -> Utils.overlay model (
      case model.json_state of
        Utils.Success -> Utils.afficheDico model.dico
        Utils.Loading -> [text "Fetching the json datas..."]
        Utils.Failure error -> [text ("Error while loading the defintion : "  ++ error)] )
            
header = div [ class "top_banner" ]
        [ h1 [] [ text "Bienvenue à devine-mot !" ]
        , p []
            [ text "Le but du jeu est de deviner le mot dont la définition est donnée ci-dessous. "],
             strong [] [ text "Bonne Chance !" ]
        ]

definition model = div [class "definition"]
    [ 
      if model.show_answer == True then 
        div [] [h1 [] [ text model.mot_cherche]]
      else 
        div [] [h1 [] [ text "Voici la définition à trouver :"]]

    , viewDefinition model
    ]

game_body model = div [class "game_body"] [p [] [ text "\n "],
      if model.user_input == model.mot_cherche then 
        div [style "text-align" "center", style "padding" "20px"]
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
            p [] [ text "\n "]
          , div [] [ h1 [ ]  [text ("BRAVO !! c'est bien ça")]]
          ]
      else if model.user_input == "" then 
        div [style "text-align" "center", style "padding" "20px"]
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
        p [] [ text "\n "]
          , div [] [ text ("Veuillez insérer la réponse dans le champ ci-dessus")]
          ]
      else
        div [style "text-align" "center", style "padding" "20px"]
          [ input [ placeholder "Insérez la réponse",  onInput Change ] [],
            p [] [ text "\n "]
          , div []  [text ("Ce n'est pas la bonne réponse")]
          ],
    p [] [ text "\n "],
    div [style "text-align" "left", style "margin" "20px"] [Button.checkboxButton model.show_answer [  Button.outlinePrimary, Button.attrs[ onClick GetAnswer ]] [text " Voir la réponse" ], Button.button [Button.outlineSuccess, Button.attrs[ onClick Abandon ]] [text "Changer de mot" ]]]

footer = div [ class "footer"]
        [
          p [] [ text "Toutes les définitions sont tirées du site :"],
          Button.linkButton [ Button.outlinePrimary, Button.attrs [ href "https://dictionaryapi.dev/"] ] [ text "API Dictionary" ]
        ] 
