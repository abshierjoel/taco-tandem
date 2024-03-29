-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.UserRole exposing (..)

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

{-| The capabilities that belong to this role
-}
capabilities : SelectionSet (Maybe (List (Maybe String))) Taco.Object.UserRole
capabilities =
      Object.selectionForField "(Maybe (List (Maybe String)))" "capabilities" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| The display name of the role
-}
displayName : SelectionSet (Maybe String) Taco.Object.UserRole
displayName =
      Object.selectionForField "(Maybe String)" "displayName" [] (Decode.string |> Decode.nullable)


{-| The globally unique identifier for the user role object.
-}
id : SelectionSet Taco.ScalarCodecs.Id Taco.Object.UserRole
id =
      Object.selectionForField "ScalarCodecs.Id" "id" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether the object is restricted from the current viewer
-}
isRestricted : SelectionSet (Maybe Bool) Taco.Object.UserRole
isRestricted =
      Object.selectionForField "(Maybe Bool)" "isRestricted" [] (Decode.bool |> Decode.nullable)


{-| The registered name of the role
-}
name : SelectionSet (Maybe String) Taco.Object.UserRole
name =
      Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)
