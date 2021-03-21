module ContractorsTest exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL--


init : () -> ( Model, Cmd Message )
init _ =
    ( Waiting, Cmd.none )


type Model
    = Failure String
    | Waiting
    | Loading
    | Success (List Contractor)


type Message
    = TryAgain
    | ContractorResult (Result Http.Error (List Contractor))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        TryAgain ->
            ( Loading, getContractors )

        ContractorResult result ->
            case result of
                Ok contractors ->
                    ( Success contractors, Cmd.none )

                Err error ->
                    ( Failure (errorToString error), Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    case model of
        Waiting ->
            button [ onClick TryAgain ] [ text "All contractors" ]

        Failure msg ->
            text ("Failure! " ++ msg)

        Loading ->
            text "Loading..."

        Success contractors ->
            div [ style "text-align" "center" ]
                [ table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "ID" ]
                            , th [] [ text "First Name" ]
                            , th [] [ text "Last Name" ]
                            , th [] [ text "ZIP" ]
                            , th [] [ text "E-mail" ]
                            ]
                        ]
                    , tbody [] (List.map viewContractor contractors)
                    ]
                , button [ onClick TryAgain ] [ text "All Contractors" ]
                ]


viewContractor : Contractor -> Html Message
viewContractor contractor =
    tr []
        [ td [] [ text <| String.fromInt contractor.id ]
        , td [] [ text contractor.fName ]
        , td [] [ text contractor.lName ]
        , td [] [ text contractor.zip ]
        , td [] [ text contractor.mail ]
        ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadStatus code ->
            "Code: " ++ String.fromInt code

        Http.NetworkError ->
            "Network Error"

        Http.BadBody err ->
            "Bad Body: " ++ err

        Http.Timeout ->
            "Timeout"

        Http.BadUrl string ->
            "Bad Url: " ++ string


getContractors : Cmd Message
getContractors =
    Http.get
        { url = "http://localhost:8080/org/api/org/empl"
        , expect = Http.expectJson ContractorResult allContractorsDecoder
        }


type alias Contractor =
    { id : Int
    , fName : String
    , lName : String
    , zip : String
    , mail : String
    }


contractorDecoder : Decode.Decoder Contractor
contractorDecoder =
    Decode.map5 Contractor
        (Decode.field "id" Decode.int)
        (Decode.field "fName" Decode.string)
        (Decode.field "lName" Decode.string)
        (Decode.field "zip" Decode.string)
        (Decode.field "mail" Decode.string)


encodeContractor : Contractor -> Encode.Value
encodeContractor contractor =
    Encode.object
        [ ( "firstName", Encode.string contractor.fName )
        , ( "lastName", Encode.string contractor.lName )
        , ( "zip", Encode.string contractor.zip )
        , ( "email", Encode.string contractor.mail )
        ]


allContractorsDecoder : Decode.Decoder (List Contractor)
allContractorsDecoder =
    Decode.list contractorDecoder



--ProjectsDecoder: Decode.Decoder (List Project)
--projectsDecoder =
--    Decode.list projectDecoder


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
