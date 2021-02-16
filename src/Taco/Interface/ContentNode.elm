-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Interface.ContentNode exposing (..)

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
    onMediaItem : SelectionSet decodesTo Taco.Object.MediaItem,
 onPage : SelectionSet decodesTo Taco.Object.Page,
 onPost : SelectionSet decodesTo Taco.Object.Post
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
      Fragments decodesTo
      -> SelectionSet decodesTo Taco.Interface.ContentNode
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [
         Object.buildFragment "MediaItem" selections____.onMediaItem,
 Object.buildFragment "Page" selections____.onPage,
 Object.buildFragment "Post" selections____.onPost
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    {
      onMediaItem = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPage = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing),
 onPost = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
{-| Connection between the ContentNode type and the ContentType type
-}
contentType : SelectionSet decodesTo Taco.Object.ContentNodeToContentTypeConnectionEdge
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
contentType object____ =
      Object.selectionForCompositeField "contentType" [] (object____) (identity >> Decode.nullable)


{-| The ID of the node in the database.
-}
databaseId : SelectionSet Int Taco.Interface.ContentNode
databaseId =
      Object.selectionForField "Int" "databaseId" [] (Decode.int)


{-| Post publishing date.
-}
date : SelectionSet (Maybe String) Taco.Interface.ContentNode
date =
      Object.selectionForField "(Maybe String)" "date" [] (Decode.string |> Decode.nullable)


{-| The publishing date set in GMT.
-}
dateGmt : SelectionSet (Maybe String) Taco.Interface.ContentNode
dateGmt =
      Object.selectionForField "(Maybe String)" "dateGmt" [] (Decode.string |> Decode.nullable)


{-| The desired slug of the post
-}
desiredSlug : SelectionSet (Maybe String) Taco.Interface.ContentNode
desiredSlug =
      Object.selectionForField "(Maybe String)" "desiredSlug" [] (Decode.string |> Decode.nullable)


{-| If a user has edited the node within the past 15 seconds, this will return the user that last edited. Null if the edit lock doesn't exist or is greater than 15 seconds
-}
editingLockedBy : SelectionSet decodesTo Taco.Object.ContentNodeToEditLockConnectionEdge
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
editingLockedBy object____ =
      Object.selectionForCompositeField "editingLockedBy" [] (object____) (identity >> Decode.nullable)


{-| The RSS enclosure for the object
-}
enclosure : SelectionSet (Maybe String) Taco.Interface.ContentNode
enclosure =
      Object.selectionForField "(Maybe String)" "enclosure" [] (Decode.string |> Decode.nullable)


type alias EnqueuedScriptsOptionalArguments = { first : OptionalArgument Int
 , last : OptionalArgument Int
 , after : OptionalArgument String
 , before : OptionalArgument String }

{-| Connection between the ContentNode type and the EnqueuedScript type

  - first - The number of items to return after the referenced "after" cursor
  - after - Cursor used along with the "first" argument to reference where in the dataset to get data
  - before - Cursor used along with the "last" argument to reference where in the dataset to get data

-}
enqueuedScripts : (EnqueuedScriptsOptionalArguments -> EnqueuedScriptsOptionalArguments)
 -> SelectionSet decodesTo Taco.Object.ContentNodeToEnqueuedScriptConnection
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
enqueuedScripts fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, last = Absent, after = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first (Encode.int), Argument.optional "last" filledInOptionals____.last (Encode.int), Argument.optional "after" filledInOptionals____.after (Encode.string), Argument.optional "before" filledInOptionals____.before (Encode.string) ]
                |> List.filterMap identity
    in
      Object.selectionForCompositeField "enqueuedScripts" optionalArgs____ (object____) (identity >> Decode.nullable)


type alias EnqueuedStylesheetsOptionalArguments = { first : OptionalArgument Int
 , last : OptionalArgument Int
 , after : OptionalArgument String
 , before : OptionalArgument String }

{-| Connection between the ContentNode type and the EnqueuedStylesheet type

  - first - The number of items to return after the referenced "after" cursor
  - after - Cursor used along with the "first" argument to reference where in the dataset to get data
  - before - Cursor used along with the "last" argument to reference where in the dataset to get data

-}
enqueuedStylesheets : (EnqueuedStylesheetsOptionalArguments -> EnqueuedStylesheetsOptionalArguments)
 -> SelectionSet decodesTo Taco.Object.ContentNodeToEnqueuedStylesheetConnection
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
enqueuedStylesheets fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, last = Absent, after = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first (Encode.int), Argument.optional "last" filledInOptionals____.last (Encode.int), Argument.optional "after" filledInOptionals____.after (Encode.string), Argument.optional "before" filledInOptionals____.before (Encode.string) ]
                |> List.filterMap identity
    in
      Object.selectionForCompositeField "enqueuedStylesheets" optionalArgs____ (object____) (identity >> Decode.nullable)


{-| The global unique identifier for this post. This currently matches the value stored in WP_Post->guid and the guid column in the "post_objects" database table.
-}
guid : SelectionSet (Maybe String) Taco.Interface.ContentNode
guid =
      Object.selectionForField "(Maybe String)" "guid" [] (Decode.string |> Decode.nullable)


{-| The globally unique identifier of the node.
-}
id : SelectionSet Taco.ScalarCodecs.Id Taco.Interface.ContentNode
id =
      Object.selectionForField "ScalarCodecs.Id" "id" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether the object is a node in the preview state
-}
isPreview : SelectionSet (Maybe Bool) Taco.Interface.ContentNode
isPreview =
      Object.selectionForField "(Maybe Bool)" "isPreview" [] (Decode.bool |> Decode.nullable)


{-| Whether the object is restricted from the current viewer
-}
isRestricted : SelectionSet (Maybe Bool) Taco.Interface.ContentNode
isRestricted =
      Object.selectionForField "(Maybe Bool)" "isRestricted" [] (Decode.bool |> Decode.nullable)


{-| The user that most recently edited the node
-}
lastEditedBy : SelectionSet decodesTo Taco.Object.ContentNodeToEditLastConnectionEdge
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
lastEditedBy object____ =
      Object.selectionForCompositeField "lastEditedBy" [] (object____) (identity >> Decode.nullable)


{-| The permalink of the post
-}
link : SelectionSet (Maybe String) Taco.Interface.ContentNode
link =
      Object.selectionForField "(Maybe String)" "link" [] (Decode.string |> Decode.nullable)


{-| The local modified time for a post. If a post was recently updated the modified field will change to match the corresponding time.
-}
modified : SelectionSet (Maybe String) Taco.Interface.ContentNode
modified =
      Object.selectionForField "(Maybe String)" "modified" [] (Decode.string |> Decode.nullable)


{-| The GMT modified time for a post. If a post was recently updated the modified field will change to match the corresponding time in GMT.
-}
modifiedGmt : SelectionSet (Maybe String) Taco.Interface.ContentNode
modifiedGmt =
      Object.selectionForField "(Maybe String)" "modifiedGmt" [] (Decode.string |> Decode.nullable)


{-| The database id of the preview node
-}
previewRevisionDatabaseId : SelectionSet (Maybe Int) Taco.Interface.ContentNode
previewRevisionDatabaseId =
      Object.selectionForField "(Maybe Int)" "previewRevisionDatabaseId" [] (Decode.int |> Decode.nullable)


{-| Whether the object is a node in the preview state
-}
previewRevisionId : SelectionSet (Maybe Taco.ScalarCodecs.Id) Taco.Interface.ContentNode
previewRevisionId =
      Object.selectionForField "(Maybe ScalarCodecs.Id)" "previewRevisionId" [] (Taco.ScalarCodecs.codecs |> Taco.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


{-| The uri slug for the post. This is equivalent to the WP_Post->post_name field and the post_name column in the database for the "post_objects" table.
-}
slug : SelectionSet (Maybe String) Taco.Interface.ContentNode
slug =
      Object.selectionForField "(Maybe String)" "slug" [] (Decode.string |> Decode.nullable)


{-| The current status of the object
-}
status : SelectionSet (Maybe String) Taco.Interface.ContentNode
status =
      Object.selectionForField "(Maybe String)" "status" [] (Decode.string |> Decode.nullable)


{-| The template assigned to a node of content
-}
template : SelectionSet decodesTo Taco.Interface.ContentTemplate
 -> SelectionSet (Maybe decodesTo) Taco.Interface.ContentNode
template object____ =
      Object.selectionForCompositeField "template" [] (object____) (identity >> Decode.nullable)


{-| URI path for the resource
-}
uri : SelectionSet String Taco.Interface.ContentNode
uri =
      Object.selectionForField "String" "uri" [] (Decode.string)