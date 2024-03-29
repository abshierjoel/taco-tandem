-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Interface.HierarchicalTermNode exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Taco.Object
import Taco.Interface
import Taco.Union
import Taco.Scalar
import Taco.InputObject
import Taco.ScalarCodecs
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)


type alias Fragments decodesTo =
    {
    onCategory : SelectionSet decodesTo Taco.Object.Category
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
      Fragments decodesTo
      -> SelectionSet decodesTo Taco.Interface.HierarchicalTermNode
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [
         Object.buildFragment "Category" selections____.onCategory
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    {
      onCategory = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
{-| Database id of the parent node
-}
parentDatabaseId : SelectionSet (Maybe Int) Taco.Interface.HierarchicalTermNode
parentDatabaseId =
      Object.selectionForField "(Maybe Int)" "parentDatabaseId" [] (Decode.int |> Decode.nullable)


{-| The globally unique identifier of the parent node.
-}
parentId : SelectionSet (Maybe Taco.ScalarCodecs.Id) Taco.Interface.HierarchicalTermNode
parentId =
      Object.selectionForField "(Maybe ScalarCodecs.Id)" "parentId" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)
