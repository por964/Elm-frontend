module Department exposing (..)

import Browser
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map3, map4, string)


type alias Department =
    { id : Int
    , code : String
    , name : String
    , description : String
    }


type alias Model =
    { all : List Department
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get all departments" ]
        , viewDepartmentsOrError model
        ]


viewDepartmentsOrError : Model -> Html Msg
viewDepartmentsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewDepartments model.all


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewDepartments : List Department -> Html Msg
viewDepartments all =
    div []
        [ h3 [] [ text "Departments" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewDepartment all)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Department code" ]
        , th []
            [ text "Department name" ]
        , th []
            [ text "Department description" ]
        ]


viewDepartment : Department -> Html Msg
viewDepartment department =
    tr []
        [ td []
            [ text (String.fromInt department.id) ]
        , td []
            [ text department.code ]
        , td []
            [ text department.name ]
        , td []
            [ text department.description ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Department))


departmentsDecoder : Decode.Decoder (List Department)
departmentsDecoder =
    Decode.field "all" (Decode.list departmentDecoder)


departmentDecoder : Decoder Department
departmentDecoder =
    map4 Department
        (field "id" int)
        (field "code" string)
        (field "name" string)
        (field "description" string)


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:8080/org/api/org/dept"
        , expect = Http.expectJson DataReceived departmentsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok all) ->
            ( { model
                | all = all
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { all = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
