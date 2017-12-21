module AddTag exposing (..)

import List
import Generated.Api as Api
import Html exposing (a, div, button, text, input, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http

type alias Flags =
  { threadId : String
  , tags : List String
  }

main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> ( Model, Cmd Msg )
init flags = ((Model False "" flags.threadId flags.tags Nothing), Cmd.none)

type alias Model =
  { opened : Bool
  , name : String
  , threadId : String
  , tags : List String
  , error : Maybe String
  }

view model =
    let xs = if model.opened then
                 [div [class "tag-edit"] [
                       input [ type_ "text", placeholder "tag name", onInput Name ] []
                       , button [ onClick PostTag, class "button" ] [ text "add tag" ]
                       , p [] [ text "tag can contains 'a-z and 0-9, -(hypen)'"]]

                 ]
            else
                [div [class "tag-edit"] [
                  button [ onClick OpenInput ] [ text "edit tag" ]
                     ]
                ]
        mkTag tag = [ a [href ("/search?q=tags:" ++ tag)] [ text tag ]]
        tags = [div [class "tags"]
                     (List.concat <| List.map mkTag model.tags)
               ]
        error = case model.error of
                    Just msg -> [p [] [text msg]]
                    _ -> []
    in div [] (tags ++ xs ++ error)

type Msg = OpenInput | PostTag | Name String | PostResult (Result Http.Error String)

update msg model =
  case Debug.log "msg" msg of
    OpenInput -> ({model | opened = True}, Cmd.none)
    Name name -> ({ model | name = name }, Cmd.none)
    PostTag -> (model, postTag model.threadId model.name)
    PostResult (Ok _) -> ({model | tags = model.name :: model.tags, opened = False, error = Nothing}, Cmd.none)
    PostResult (Err e) -> ({model | error = Just "Error has occured"}, Cmd.none)
--    _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

postTag : String -> String -> Cmd Msg
postTag threadId name = Http.send PostResult (Api.postThreadsByIdTags threadId (Api.PostTagRequest name))
