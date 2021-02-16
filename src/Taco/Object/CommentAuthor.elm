-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.CommentAuthor exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Taco.Object
import Taco.Interface
import Taco.Union
import Taco.Scalar
import Taco.InputObject
import Taco.ScalarCodecs
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)

{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet Int Taco.Object.CommentAuthor
databaseId =
      Object.selectionForField "Int" "databaseId" [] (Decode.int)


{-| The email for the comment author
-}
email : SelectionSet (Maybe String) Taco.Object.CommentAuthor
email =
      Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


{-| The globally unique identifier for the comment author object
-}
id : SelectionSet Taco.ScalarCodecs.Id Taco.Object.CommentAuthor
id =
      Object.selectionForField "ScalarCodecs.Id" "id" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether the object is restricted from the current viewer
-}
isRestricted : SelectionSet (Maybe Bool) Taco.Object.CommentAuthor
isRestricted =
      Object.selectionForField "(Maybe Bool)" "isRestricted" [] (Decode.bool |> Decode.nullable)


{-| The name for the comment author.
-}
name : SelectionSet (Maybe String) Taco.Object.CommentAuthor
name =
      Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


{-| The url the comment author.
-}
url : SelectionSet (Maybe String) Taco.Object.CommentAuthor
url =
      Object.selectionForField "(Maybe String)" "url" [] (Decode.string |> Decode.nullable)
