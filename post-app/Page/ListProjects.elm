module Page.ListProjects exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Project exposing (Project, ProjectID, projectsDecoder)
import RemoteData exposing (WebData)


type alias Model =
    { projects : WebData (List Project)
    }


type Msg
    = FetchProjects
    | ProjectsReceived (WebData (List Project))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { projects = RemoteData.Loading }, fetchProjects )


fetchProjects : Cmd Msg
fetchProjects =
    Http.get
        { url = "http://localhost:8080/org/api/org/proj"
        , expect =
            projectsDecoder
                |> Http.expectJson (RemoteData.fromResult >> ProjectsReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchProjects ->
            ( { model | projects = RemoteData.Loading }, fetchProjects )

        ProjectsReceived response ->
            ( { model | projects = response }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchProjects ]
            [ text "Refresh posts" ]
        , viewProjects model.projects
        ]


viewProjects : WebData (List Project) -> Html Msg
viewProjects projects =
    case projects of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualProjects ->
            div []
                [ h3 [] [ text "Projects" ]
                , table []
                    ([ viewTableHeader ] ++ List.map viewProject actualProjects)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Duration" ]
        ]


viewProject : Project -> Html Msg
viewProject project =
    let
        projectPath =
            "/projects/" ++ Project.idToString project.id
    in
    tr []
        [ td []
            [ text (Project.idToString project.id) ]
        , td []
            [ text project.title ]
        , td []
            [ text (String.fromInt project.duration) ]
        , td []
            [ a [ href projectPath ] [ text "Edit" ] ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch projects at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
