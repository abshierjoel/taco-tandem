module BlogConfig exposing (..)


type alias BlogInfo =
    { gqlUrl : String
    , title : String
    , description : String
    }


getTitle : BlogInfo -> String
getTitle flags =
    flags.title


getFullName : BlogInfo -> String
getFullName flags =
    flags.title ++ " - " ++ flags.description


getPageTitle : BlogInfo -> String -> String
getPageTitle flags title =
    title ++ " - " ++ flags.title
