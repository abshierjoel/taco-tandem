-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Union.MenuItemObjectUnion exposing (..)

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
import Taco.ScalarCodecs
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)


type alias Fragments decodesTo =
    {
    onPost : SelectionSet decodesTo Taco.Object.Post,
 onPage : SelectionSet decodesTo Taco.Object.Page,
 onCategory : SelectionSet decodesTo Taco.Object.Category,
 onTag : SelectionSet decodesTo Taco.Object.Tag,
 onPostFormat : SelectionSet decodesTo Taco.Object.PostFormat
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Taco.Union.MenuItemObjectUnion
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [
          Object.buildFragment "Post" selections____.onPost,
 Object.buildFragment "Page" selections____.onPage,
 Object.buildFragment "Category" selections____.onCategory,
 Object.buildFragment "Tag" selections____.onTag,
 Object.buildFragment "PostFormat" selections____.onPostFormat
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    {
      onPost = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPage = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onCategory = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onTag = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPostFormat = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
