module Generated.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias PostTagRequest =
    { postTagRequestName : String
    }

encodePostTagRequest : PostTagRequest -> Json.Encode.Value
encodePostTagRequest x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.postTagRequestName )
        ]

postThreadsByIdTags : String -> PostTagRequest -> Http.Request (String)
postThreadsByIdTags capture_id body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "threads"
                , capture_id |> Http.encodeUri
                , "tags"
                ]
        , body =
            Http.jsonBody (encodePostTagRequest body)
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }