-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.UpdateSettingsPayload exposing (..)

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

{-| 
-}
allSettings : SelectionSet decodesTo Taco.Object.Settings
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateSettingsPayload
allSettings object____ =
      Object.selectionForCompositeField "allSettings" [] (object____) (identity >> Decode.nullable)


{-| 
-}
clientMutationId : SelectionSet (Maybe String) Taco.Object.UpdateSettingsPayload
clientMutationId =
      Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| 
-}
discussionSettings : SelectionSet decodesTo Taco.Object.DiscussionSettings
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateSettingsPayload
discussionSettings object____ =
      Object.selectionForCompositeField "discussionSettings" [] (object____) (identity >> Decode.nullable)


{-| 
-}
generalSettings : SelectionSet decodesTo Taco.Object.GeneralSettings
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateSettingsPayload
generalSettings object____ =
      Object.selectionForCompositeField "generalSettings" [] (object____) (identity >> Decode.nullable)


{-| 
-}
readingSettings : SelectionSet decodesTo Taco.Object.ReadingSettings
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateSettingsPayload
readingSettings object____ =
      Object.selectionForCompositeField "readingSettings" [] (object____) (identity >> Decode.nullable)


{-| 
-}
writingSettings : SelectionSet decodesTo Taco.Object.WritingSettings
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateSettingsPayload
writingSettings object____ =
      Object.selectionForCompositeField "writingSettings" [] (object____) (identity >> Decode.nullable)