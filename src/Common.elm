module Common exposing (urlPrefix, Photo)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type alias Photo =
    { title       : String
    , url         : String
    , size        : Int
    , relatedUrls : List String
    }