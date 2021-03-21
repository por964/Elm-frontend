module Contractors exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, map5, string)


type alias Contractor =
    { id : Int
    , fName : String
    , lName : String
    , zip : String
    , mail : String
    }


type alias Model =
    { contractors : List Contractor
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewPostsOrError model
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewContractors model.contractors


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


viewContractors : List Contractor -> Html Msg
viewContractors contractors =
    div []
        [ h3 [] [ text "Contractors" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewContractor contractors)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "First" ]
        , th []
            [ text "Last" ]
        , th []
            [ text "ZIP" ]
        , th []
            [ text "Mail" ]
        ]


viewContractor : Contractor -> Html Msg
viewContractor contractor =
    tr []
        [ td []
            [ text (String.fromInt contractor.id) ]
        , td []
            [ text contractor.fName ]
        , td []
            [ text contractor.lName ]
        , td []
            [ text contractor.zip ]
        , td []
            [ text contractor.mail ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Contractor))


contractorsDecoder : Decode.Decoder (List Contractor)
contractorsDecoder =
    Decode.field "contractors" (Decode.list contractorDecoder)


contractorDecoder : Decoder Contractor
contractorDecoder =
    map5 Contractor
        (field "id" int)
        (field "fName" string)
        (field "lName" string)
        (field "zip" string)
        (field "mail" string)


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:8080/org/api/org/con/all"
        , expect = Http.expectJson DataReceived contractorsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok contractors) ->
            ( { model
                | contractors = contractors
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { contractors = []
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
