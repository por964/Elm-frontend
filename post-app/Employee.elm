module Employee exposing (Employee, EmployeeId, employeeDecoder, employeesDecoder, idToString)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Employee =
    { id : EmployeeId
    , firstName : String
    , lastName : String
    , email : String
    }


type EmployeeId
    = EmployeeId Int


employeesDecoder : Decoder (List Employee)
employeesDecoder =
    list employeeDecoder


employeeDecoder : Decoder Employee
employeeDecoder =
    Decode.succeed Employee
        |> required "id" idDecoder
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string


idDecoder : Decoder EmployeeId
idDecoder =
    Decode.map EmployeeId int


idToString : EmployeeId -> String
idToString (EmployeeId id) =
    String.fromInt id
