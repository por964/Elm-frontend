module Project exposing (Project, ProjectID, idToString, projectDecoder, projectEncoder, projectsDecoder)

import Json.Decode as Decode exposing (Decoder, field, int, list, map3, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (RemoteData, WebData)


type alias Project =
    { id : ProjectID
    , title : String
    , duration : Int
    }


type ProjectID
    = ProjectID Int


type alias Model =
    { projects : WebData (List Project)
    }


type Msg
    = FetchProjects
    | ProjectsReceived (WebData (List Project))


projectsDecoder : Decode.Decoder (List Project)
projectsDecoder =
    Decode.field "all" (Decode.list projectDecoder)


projDecoder : Decoder Project
projDecoder =
    Decode.succeed Project
        |> required "id" idDecoder
        |> required "title" string
        |> required "duration" int


projectDecoder : Decoder Project
projectDecoder =
    map3 Project
        (field "id" idDecoder)
        (field "title" string)
        (field "duration" int)


idDecoder : Decoder ProjectID
idDecoder =
    Decode.map ProjectID int


idToString : ProjectID -> String
idToString (ProjectID id) =
    String.fromInt id


projectEncoder : Project -> Encode.Value
projectEncoder project =
    Encode.object
        [ ( "title", Encode.string project.title )
        , ( "duration", Encode.int project.duration )
        ]
