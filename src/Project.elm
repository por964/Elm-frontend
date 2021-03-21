module Project exposing (..)

import Browser
import Employee exposing (Employee, employeeDecoder, employeesDecoder)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, map4, string)
import Json.Decode.Pipeline exposing (decode, optional, optionalAt, required, requiredAt)


type alias Project employee =
    { id : Int
    , title : String
    , duration : Int
    , employees : List employee
    }


type alias Model =
    { posts : List Project
    , errorMessage : Maybe String
    }


projectDecoder : Decoder Project
projectDecoder =
    map4 Project
        (field "id" int)
        (field "title" string)
        (field "duration" int)
        (field "employees" list Employee)


projectsDecoder : Decode.Decoder (List Project)
projectsDecoder =
    Decode.field "projects" (Decode.list projectDecoder)


projDecoder : Decoder Project
projDecoder =
    Decode.succeed Project
        |> required "id" int
        |> required "title" string
        |> required "duration" int
        |> required "employees" employeesDecoder
