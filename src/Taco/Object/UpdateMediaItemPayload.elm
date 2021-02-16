-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Taco.Object.UpdateMediaItemPayload exposing (..)

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
clientMutationId : SelectionSet (Maybe String) Taco.Object.UpdateMediaItemPayload
clientMutationId =
      Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| 
-}
mediaItem : SelectionSet decodesTo Taco.Object.MediaItem
 -> SelectionSet (Maybe decodesTo) Taco.Object.UpdateMediaItemPayload
mediaItem object____ =
      Object.selectionForCompositeField "mediaItem" [] (object____) (identity >> Decode.nullable)
