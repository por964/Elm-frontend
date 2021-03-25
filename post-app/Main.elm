module Main exposing (main)

import Browser
import Page.ListProjects as ListProjects


main : Program () ListProjects.Model ListProjects.Msg
main =
    Browser.element
        { init = ListProjects.init
        , view = ListProjects.view
        , update = ListProjects.update
        , subscriptions = \_ -> Sub.none
        }
