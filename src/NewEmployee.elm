module NewEmployee exposing (Model)

import Browser
import Employee exposing (Employee, employeeDecoder, emptyEmployee, newEmployeeEncoder)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http


type alias Model =
    { employee : Employee
    , createError : Maybe String
    }


initialModel : employee -> Model
initialModel employee =
    { employee = emptyEmployee
    , createError = Nothing
    }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Create new employee" ]
        , newEmployeeForm
        , viewError model.createError
        ]


newEmployeeForm : Html Msg
newEmployeeForm =
    Html.form []
        [ div []
            [ text "First name"
            , br [] []
            , input [ type_ "text", onInput StoreFname ] []
            ]
        , br [] []
        , div []
            [ text "Last name"
            , br [] []
            , input [ type_ "text", onInput StoreLname ] []
            ]
        , br [] []
        , div []
            [ text "Email"
            , br [] []
            , input [ type_ "text", onInput StoreEmail ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick CreateEmployee ]
                [ text "Submit" ]
            ]
        ]


type Msg
    = StoreFname String
    | StoreLname String
    | StoreEmail String
    | CreateEmployee
    | EmployeeCreated (Result Http.Error Employee)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreFname fName ->
            let
                oldEmployee =
                    model.employee

                updateFname =
                    { oldEmployee | firstName = fName }
            in
            ( { model | employee = updateFname }, Cmd.none )

        StoreLname lName ->
            let
                oldEmployee =
                    model.employee

                updateLname =
                    { oldEmployee | lastName = lName }
            in
            ( { model | employee = updateLname }, Cmd.none )

        StoreEmail email ->
            let
                oldEmployee =
                    model.employee

                updateEmail =
                    { oldEmployee | email = email }
            in
            ( { model | employee = updateEmail }, Cmd.none )

        CreateEmployee ->
            ( model, createEmployee model.employee )

        EmployeeCreated (Ok employee) ->
            ( { model | employee = employee, createError = Nothing }
            , Cmd.none
            )

        EmployeeCreated (Err error) ->
            ( { model | createError = Just (buildErrorMessage error) }
            , Cmd.none
            )


createEmployee : Employee -> Cmd Msg
createEmployee employee =
    Http.post
        { url = "http://localhost:8080/org/api/org/empl"
        , body = Http.jsonBody (newEmployeeEncoder employee)
        , expect = Http.expectJson EmployeeCreated employeeDecoder
        }


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel emptyEmployee, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
