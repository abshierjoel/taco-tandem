-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Interface.DatabaseIdentifier exposing (..)

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
    onCategory : SelectionSet decodesTo Taco.Object.Category,
 onUser : SelectionSet decodesTo Taco.Object.User,
 onComment : SelectionSet decodesTo Taco.Object.Comment,
 onMediaItem : SelectionSet decodesTo Taco.Object.MediaItem,
 onPage : SelectionSet decodesTo Taco.Object.Page,
 onPost : SelectionSet decodesTo Taco.Object.Post,
 onPostFormat : SelectionSet decodesTo Taco.Object.PostFormat,
 onTag : SelectionSet decodesTo Taco.Object.Tag,
 onMenu : SelectionSet decodesTo Taco.Object.Menu,
 onMenuItem : SelectionSet decodesTo Taco.Object.MenuItem
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
      Fragments decodesTo
      -> SelectionSet decodesTo Taco.Interface.DatabaseIdentifier
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [
         Object.buildFragment "Category" selections____.onCategory,
 Object.buildFragment "User" selections____.onUser,
 Object.buildFragment "Comment" selections____.onComment,
 Object.buildFragment "MediaItem" selections____.onMediaItem,
 Object.buildFragment "Page" selections____.onPage,
 Object.buildFragment "Post" selections____.onPost,
 Object.buildFragment "PostFormat" selections____.onPostFormat,
 Object.buildFragment "Tag" selections____.onTag,
 Object.buildFragment "Menu" selections____.onMenu,
 Object.buildFragment "MenuItem" selections____.onMenuItem
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    {
      onCategory = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onUser = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onComment = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onMediaItem = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPage = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPost = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPostFormat = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onTag = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onMenu = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onMenuItem = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
{-| The unique identifier stored in the database
-}
databaseId : SelectionSet Int Taco.Interface.DatabaseIdentifier
databaseId =
      Object.selectionForField "Int" "databaseId" [] (Decode.int)