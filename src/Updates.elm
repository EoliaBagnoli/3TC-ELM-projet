module Updates exposing (..)

import Random
import Utils exposing (..)


updatePage : Msg -> Model -> (Model, Cmd Msg)
updatePage msg model = 
    case msg of
        GotDictionary result ->
            case result of
                Ok dictionary ->
                    ({ model | json = Utils.Success, dico = dictionary} , Cmd.none)
                Err error ->
                    ({model | json = Utils.Failure (Utils.toString error)}, Cmd.none)
        GotHttp result ->
            case result of 
                Ok words_txt ->
                    ({model | http = Utils.Success ,  all_the_words = String.split " " words_txt  }, Random.generate Word_number (Random.int 1 1000))
                Err error ->
                    ({model | http = Utils.Failure (Utils.toString error)}, Cmd.none)

        Change newContent ->
            ({ model | content = newContent }, Cmd.none)

        Word_number index -> 
            case (Utils.getElementAtIndex model.all_the_words index) of
                  Nothing -> 
                      (model, Cmd.none)
                  Just word_at_index -> 
                      ({ model | mot_cherche = word_at_index}, Utils.getDictionary model)
