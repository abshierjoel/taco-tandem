module BlogConfig exposing (..)


type alias Flags =
    { gqlUrl : String
    , title : String
    , description : String
    }


getTitle : Flags -> String
getTitle flags =
    flags.title


getFullName : Flags -> String
getFullName flags =
    flags.title ++ " - " ++ flags.description


getPageTitle : Flags -> String -> String
getPageTitle flags title =
    title ++ " - " ++ flags.title
