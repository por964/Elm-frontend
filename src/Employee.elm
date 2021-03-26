module Employee exposing (Employee, employeeDecoder, emptyEmployee, newEmployeeEncoder)

import Browser
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map4, string)
import Json.Encode as Encode exposing (int, string)


type alias Employee =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    }


type alias Model =
    { all : List Employee
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewEmployeesOrError model
        ]


viewEmployeesOrError : Model -> Html Msg
viewEmployeesOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewEmployees model.all


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


viewEmployees : List Employee -> Html Msg
viewEmployees all =
    div []
        [ h3 [] [ text "Employees" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewEmployee all)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "First name" ]
        , th []
            [ text "Last name" ]
        , th []
            [ text "Mail" ]
        ]


viewEmployee : Employee -> Html Msg
viewEmployee employee =
    tr []
        [ td []
            [ text (String.fromInt employee.id) ]
        , td []
            [ text employee.firstName ]
        , td []
            [ text employee.lastName ]
        , td []
            [ text employee.email ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Employee))


employeesDecoder : Decode.Decoder (List Employee)
employeesDecoder =
    Decode.field "all" (Decode.list employeeDecoder)


employeeDecoder : Decoder Employee
employeeDecoder =
    map4 Employee
        (field "id" int)
        (field "firstName" string)
        (field "lastName" string)
        (field "mail" string)



{--
emplDecoder : Decoder Employee
emplDecoder =
    Decode.succeed Employee
        |> required "id" int
        |> required "firstName" string
        |> required "lastName" string
        |> required "mail" string
        |> required "projects" projectsDecoder
--}


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:8080/org/api/org/empl"
        , expect = Http.expectJson DataReceived employeesDecoder
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


emptyEmployee : Employee
emptyEmployee =
    { id = -1
    , firstName = ""
    , lastName = ""
    , email = ""
    }


newEmployeeEncoder : Employee -> Encode.Value
newEmployeeEncoder employee =
    Encode.object
        [ ( "firstName", Encode.string employee.firstName )
        , ( "lastName", Encode.string employee.lastName )
        , ( "email", Encode.string employee.email )
        ]
