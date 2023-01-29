module Updates exposing (..)

import Random
import Utils exposing (..)


updatePage : Msg -> Model -> (Model, Cmd Msg)
updatePage msg model = 
    case msg of
        GotDictionary result ->
            case result of
                Ok dictionary ->
                    ({ model | json_state = Utils.Success, dico = dictionary} , Cmd.none)
                Err error ->
                    ({model | json_state = Utils.Failure (Utils.toString error)}, Cmd.none)
        GotHttp result ->
            case result of 
                Ok words_txt ->
                    ({model | http_state = Utils.Success ,  all_the_words = String.split " " words_txt  }, Random.generate Word_number (Random.int 1 1000))
                Err error ->
                    ({model | http_state = Utils.Failure (Utils.toString error)}, Cmd.none)

        Change newUser_input ->
            ({ model | user_input = newUser_input }, Cmd.none)

        Word_number index -> 
            case (Utils.getElementAtIndex model.all_the_words index) of
                  Nothing -> 
                      (model, Cmd.none)
                  Just word_at_index -> 
                      ({ model | mot_cherche = word_at_index}, Utils.getDictionary model)

        GetAnswer ->
            if model.show_answer == False then 
                ({model | show_answer = True}, Cmd.none)
            else 
                ({model | show_answer = False}, Cmd.none)

        Abandon ->
            ({model | http_state = Loading }, getWord)

