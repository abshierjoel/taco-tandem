-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Interface.NodeWithPageAttributes exposing (..)

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
    onPage : SelectionSet decodesTo Taco.Object.Page
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
      Fragments decodesTo
      -> SelectionSet decodesTo Taco.Interface.NodeWithPageAttributes
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [
         Object.buildFragment "Page" selections____.onPage
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    {
      onPage = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
{-| A field used for ordering posts. This is typically used with nav menu items or for special ordering of hierarchical content types.
-}
menuOrder : SelectionSet (Maybe Int) Taco.Interface.NodeWithPageAttributes
menuOrder =
      Object.selectionForField "(Maybe Int)" "menuOrder" [] (Decode.int |> Decode.nullable)
