module Page.NewProject exposing (Model)

import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Maybe exposing (withDefault)
import Project exposing (Project, ProjectID, projectDecoder, projectEncoder)
import RemoteData exposing (WebData)


type alias Model =
    { project : WebData Project
    , saveError : Maybe String
    }


fetchProject : ProjectID -> Cmd Msg
fetchProject projectId =
    Http.get
        { url = "http://localhost:8080/org/api/org/proj" ++ Project.idToString projectId
        , expect =
            projectDecoder
                |> Http.expectJson (RemoteData.fromResult >> ProjectReceived)
        }


initialModel : Project -> Model
initialModel project =
    { project = RemoteData.Loading
    , saveError = Nothing
    }


type Msg
    = ProjectReceived (WebData Project)
    | UpdateTitle String
    | UpdateDuration String
    | SaveProject
    | ProjectSaved (Result Http.Error Project)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProjectReceived project ->
            ( { model | project = project }, Cmd.none )

        UpdateTitle newTitle ->
            let
                updateTitle =
                    RemoteData.map
                        (\projectData ->
                            { projectData | title = newTitle }
                        )
                        model.project
            in
            ( { model | project = updateTitle }, Cmd.none )

        UpdateDuration durationText ->
            let
                newDuration =
                    withDefault 0 (String.toInt durationText)

                updateDuration =
                    RemoteData.map
                        (\projectData ->
                            { projectData | duration = newDuration }
                        )
                        model.project
            in
            ( { model | project = updateDuration }, Cmd.none )

        SaveProject ->
            ( model, saveProject model.project )

        ProjectSaved (Ok projectData) ->
            let
                project =
                    RemoteData.succeed projectData
            in
            ( { model | project = project, saveError = Nothing }, Cmd.none )

        ProjectSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }
            , Cmd.none
            )


saveProject : WebData Project -> Cmd Msg
saveProject project =
    case project of
        RemoteData.Success projectData ->
            let
                postUrl =
                    "http://localhost:8080/org/api/org/proj"
                        ++ Project.idToString projectData.id
            in
            Http.request
                { method = "POST"
                , headers = []
                , url = postUrl
                , body = Http.jsonBody (projectEncoder projectData)
                , expect = Http.expectJson ProjectSaved projectDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit project" ]
        , viewProject model.project
        , viewSaveError model.saveError
        ]


viewProject : WebData Project -> Html Msg
viewProject project =
    case project of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Project..." ]

        RemoteData.Success projectData ->
            editForm projectData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : Project -> Html Msg
editForm project =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input
                [ type_ "text"
                , value project.title
                , onInput UpdateTitle
                ]
                []
            ]
        , br [] []
        , div []
            [ text "Project duration"
            , br [] []
            , input
                [ type_ "number"
                , value <| String.fromInt project.duration
                , onInput UpdateDuration
                ]
                []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick SaveProject ]
                [ text "Submit" ]
            ]
        ]


sanitize : String -> Maybe Int
sanitize input =
    input
        |> String.toInt


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch project at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save project at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
