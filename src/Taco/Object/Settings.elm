-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.Settings exposing (..)

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

{-| Allow people to submit comments on new posts.
-}
discussionSettingsDefaultCommentStatus : SelectionSet (Maybe String) Taco.Object.Settings
discussionSettingsDefaultCommentStatus =
      Object.selectionForField "(Maybe String)" "discussionSettingsDefaultCommentStatus" [] (Decode.string |> Decode.nullable)


{-| Allow link notifications from other blogs (pingbacks and trackbacks) on new articles.
-}
discussionSettingsDefaultPingStatus : SelectionSet (Maybe String) Taco.Object.Settings
discussionSettingsDefaultPingStatus =
      Object.selectionForField "(Maybe String)" "discussionSettingsDefaultPingStatus" [] (Decode.string |> Decode.nullable)


{-| A date format for all date strings.
-}
generalSettingsDateFormat : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsDateFormat =
      Object.selectionForField "(Maybe String)" "generalSettingsDateFormat" [] (Decode.string |> Decode.nullable)


{-| Site tagline.
-}
generalSettingsDescription : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsDescription =
      Object.selectionForField "(Maybe String)" "generalSettingsDescription" [] (Decode.string |> Decode.nullable)


{-| This address is used for admin purposes, like new user notification.
-}
generalSettingsEmail : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsEmail =
      Object.selectionForField "(Maybe String)" "generalSettingsEmail" [] (Decode.string |> Decode.nullable)


{-| WordPress locale code.
-}
generalSettingsLanguage : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsLanguage =
      Object.selectionForField "(Maybe String)" "generalSettingsLanguage" [] (Decode.string |> Decode.nullable)


{-| A day number of the week that the week should start on.
-}
generalSettingsStartOfWeek : SelectionSet (Maybe Int) Taco.Object.Settings
generalSettingsStartOfWeek =
      Object.selectionForField "(Maybe Int)" "generalSettingsStartOfWeek" [] (Decode.int |> Decode.nullable)


{-| A time format for all time strings.
-}
generalSettingsTimeFormat : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsTimeFormat =
      Object.selectionForField "(Maybe String)" "generalSettingsTimeFormat" [] (Decode.string |> Decode.nullable)


{-| A city in the same timezone as you.
-}
generalSettingsTimezone : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsTimezone =
      Object.selectionForField "(Maybe String)" "generalSettingsTimezone" [] (Decode.string |> Decode.nullable)


{-| Site title.
-}
generalSettingsTitle : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsTitle =
      Object.selectionForField "(Maybe String)" "generalSettingsTitle" [] (Decode.string |> Decode.nullable)


{-| Site URL.
-}
generalSettingsUrl : SelectionSet (Maybe String) Taco.Object.Settings
generalSettingsUrl =
      Object.selectionForField "(Maybe String)" "generalSettingsUrl" [] (Decode.string |> Decode.nullable)


{-| Blog pages show at most.
-}
readingSettingsPostsPerPage : SelectionSet (Maybe Int) Taco.Object.Settings
readingSettingsPostsPerPage =
      Object.selectionForField "(Maybe Int)" "readingSettingsPostsPerPage" [] (Decode.int |> Decode.nullable)


{-| Default post category.
-}
writingSettingsDefaultCategory : SelectionSet (Maybe Int) Taco.Object.Settings
writingSettingsDefaultCategory =
      Object.selectionForField "(Maybe Int)" "writingSettingsDefaultCategory" [] (Decode.int |> Decode.nullable)


{-| Default post format.
-}
writingSettingsDefaultPostFormat : SelectionSet (Maybe String) Taco.Object.Settings
writingSettingsDefaultPostFormat =
      Object.selectionForField "(Maybe String)" "writingSettingsDefaultPostFormat" [] (Decode.string |> Decode.nullable)


{-| Convert emoticons like :-) and :-P to graphics on display.
-}
writingSettingsUseSmilies : SelectionSet (Maybe Bool) Taco.Object.Settings
writingSettingsUseSmilies =
      Object.selectionForField "(Maybe Bool)" "writingSettingsUseSmilies" [] (Decode.bool |> Decode.nullable)