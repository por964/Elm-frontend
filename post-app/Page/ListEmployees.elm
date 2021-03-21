module Page.ListEmployees exposing (Model, Msg, init, update, view)

import Employee exposing (Employee, EmployeeId, employeesDecoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
