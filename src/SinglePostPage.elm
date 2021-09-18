port module SinglePostPage exposing (..)

import Accessibility as Html exposing (Html, button, div, h1, h3, img, inputText, label, p, span, text, textarea)
import Browser
import FontAwesome.Icon as Icon
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, nonNullElementsOrFail, nonNullOrFail, with)
import Html exposing (a, form)
import Html.Attributes exposing (class, disabled, id, required, src, type_, value)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Parser exposing (Node(..))
import Html.Parser.Util
import Iso8601
import Json.Decode exposing (string)
import RemoteData exposing (RemoteData)
import SocialLinks exposing (viewShareButtons)
import Taco.Enum.PostObjectFieldFormatEnum
import Taco.InputObject
import Taco.Interface exposing (Commenter)
import Taco.Interface.Commenter as Commenter
import Taco.Mutation as Mutation exposing (CreateCommentRequiredArguments)
import Taco.Object
    exposing
        ( Avatar
        , Comment
        , MediaItem
        , Post
        , User
        )
import Taco.Object.Avatar as Avatar
import Taco.Object.Comment as Comment
import Taco.Object.CommentToCommenterConnectionEdge as CommentToCommenterConnectionEdge
import Taco.Object.CreateCommentPayload as CreateCommentPayload
import Taco.Object.MediaItem as MediaItem
import Taco.Object.NodeWithAuthorToUserConnectionEdge as NodeWithAuthorToUserConnectionEdge
import Taco.Object.NodeWithFeaturedImageToMediaItemConnectionEdge as NodeWithFeaturedImageToMediaItemConnectionEdge
import Taco.Object.Post as Post
import Taco.Object.PostToCommentConnection as PostToCommentConnection
import Taco.Object.PostToCommentConnectionEdge as PostToCommentConnectionEdge
import Taco.Object.User as User
import Taco.Query as Query exposing (postBy)
import Taco.ScalarCodecs
import Time exposing (Month(..))



---- PROGRAM ----


port sendNewOpenGraph : Maybe Post -> Cmd msg


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



---- INIT ----


init : String -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | slug = flags }, getPost flags )


initialModel : Model
initialModel =
    { slug = "i-sure-love-tacos"
    , post = RemoteData.Loading
    , newComment = RemoteData.NotAsked
    , commentName = ""
    , commentEmail = ""
    , commentContent = ""
    }


graphqlEndpoint =
    "/wordpress/graphql"



---- MODEL ----


type alias Model =
    { slug : String
    , post : RemoteData (Graphql.Http.Error (Maybe Post)) (Maybe Post)
    , newComment : RemoteData (Graphql.Http.Error (Maybe CreateCommentPayload)) (Maybe CreateCommentPayload)
    , commentName : String
    , commentEmail : String
    , commentContent : String
    }


type alias Post =
    { date : Maybe String
    , commentCount : Maybe Int
    , uri : String
    , title : Maybe String
    , content : Maybe String
    , author : Maybe User
    , excerpt : Maybe String
    , featuredImage : Maybe MediaItem
    , comments : Maybe (List Comment)
    , databaseId : Int
    }


type alias User =
    { firstName : Maybe String
    , nickname : Maybe String
    , avatar : Maybe Avatar
    , description : Maybe String
    }


type alias MediaItem =
    { sourceUrl : Maybe String
    }


type alias Avatar =
    { url : Maybe String
    , isRestricted : Maybe Bool
    }


type alias Comment =
    { content : Maybe String
    , date : Maybe String
    , author : Maybe Commenter
    }


type alias Commenter =
    { name : Maybe String
    , email : Maybe String
    }


type alias CreateCommentPayload =
    { success : Maybe Bool
    , comment : Maybe Comment
    }



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error (Maybe Post)) (Maybe Post))
    | ChangedNameInput String
    | ChangedEmailInput String
    | ChangedContentInput String
    | SubmittedNewComment
    | GotAddCommentResponse (RemoteData (Graphql.Http.Error (Maybe CreateCommentPayload)) (Maybe CreateCommentPayload))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success data ->
                    ( { model | post = response }, sendNewOpenGraph data )

                _ ->
                    ( model, Cmd.none )

        ChangedNameInput value ->
            ( { model | commentName = value }, Cmd.none )

        ChangedEmailInput value ->
            ( { model | commentEmail = value }, Cmd.none )

        ChangedContentInput value ->
            ( { model | commentContent = value }, Cmd.none )

        SubmittedNewComment ->
            case model.post of
                RemoteData.Success maybePost ->
                    case maybePost of
                        Just thePost ->
                            ( model
                            , addComment thePost.databaseId
                                ( model.commentName
                                , model.commentEmail
                                , model.commentContent
                                )
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotAddCommentResponse response ->
            ( { model | newComment = response }, Cmd.none )



---- QUERY ----


addComment : Int -> ( String, String, String ) -> Cmd Msg
addComment postId ( name, email, content ) =
    addCommentQuery postId ( name, email, content )
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotAddCommentResponse)


addCommentQuery : Int -> ( String, String, String ) -> SelectionSet (Maybe CreateCommentPayload) RootMutation
addCommentQuery postId ( name, email, content ) =
    Mutation.createComment (addCommentArgs postId ( name, email, content )) addCommentSelection


addCommentArgs : Int -> ( String, String, String ) -> CreateCommentRequiredArguments
addCommentArgs postId ( name, email, content ) =
    { input =
        Taco.InputObject.buildCreateCommentInput
            (\optionals ->
                { optionals
                    | commentOn = Graphql.OptionalArgument.Present postId
                    , author = Graphql.OptionalArgument.Present name
                    , authorEmail = Graphql.OptionalArgument.Present email
                    , content = Graphql.OptionalArgument.Present content
                }
            )
    }


addCommentSelection : SelectionSet CreateCommentPayload Taco.Object.CreateCommentPayload
addCommentSelection =
    SelectionSet.map2 CreateCommentPayload
        CreateCommentPayload.success
        (CreateCommentPayload.comment commentSelection)


getPost : String -> Cmd Msg
getPost slug =
    postQuery slug
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


postQuery : String -> SelectionSet (Maybe Post) RootQuery
postQuery slug =
    Query.postBy (getPostOptArgs slug) postSelection


getPostOptArgs : String -> Query.PostByOptionalArguments -> Query.PostByOptionalArguments
getPostOptArgs slug args =
    { id = Null
    , postId = Null
    , uri = Null
    , slug = Graphql.OptionalArgument.Present slug
    }


postSelection : SelectionSet Post Taco.Object.Post
postSelection =
    SelectionSet.succeed Post
        |> with Post.date
        |> with Post.commentCount
        |> with Post.uri
        |> with (Post.title postTitleArgs)
        |> with (Post.content postContentArgs)
        |> with (Post.author authorSelectionEdge)
        |> with (Post.excerpt (\optionals -> optionals))
        |> with (Post.featuredImage featuredImageSelectionEdge)
        |> with (Post.comments (\optionals -> optionals) commentsSelectionEdges)
        |> with Post.databaseId


commentsSelectionEdges : SelectionSet (List Comment) Taco.Object.PostToCommentConnection
commentsSelectionEdges =
    PostToCommentConnection.edges
        (PostToCommentConnectionEdge.node commentSelection
            |> SelectionSet.nonNullOrFail
        )
        |> nonNullOrFail
        |> nonNullElementsOrFail


commentSelection : SelectionSet Comment Taco.Object.Comment
commentSelection =
    SelectionSet.map3 Comment
        (Comment.content (\optionals -> optionals))
        Comment.date
        (Comment.author commentAuthorSelectionEdge)


commentAuthorSelectionEdge : SelectionSet Commenter Taco.Object.CommentToCommenterConnectionEdge
commentAuthorSelectionEdge =
    CommentToCommenterConnectionEdge.node commentAuthorSelection
        |> nonNullOrFail


commentAuthorSelection : SelectionSet Commenter Taco.Interface.Commenter
commentAuthorSelection =
    SelectionSet.map2 Commenter
        Commenter.name
        Commenter.email


featuredImageSelectionEdge : SelectionSet MediaItem Taco.Object.NodeWithFeaturedImageToMediaItemConnectionEdge
featuredImageSelectionEdge =
    NodeWithFeaturedImageToMediaItemConnectionEdge.node featuredImageSelection
        |> nonNullOrFail


featuredImageSelection : SelectionSet MediaItem Taco.Object.MediaItem
featuredImageSelection =
    SelectionSet.map MediaItem
        (MediaItem.sourceUrl (\optionals -> optionals))


authorSelectionEdge : SelectionSet User Taco.Object.NodeWithAuthorToUserConnectionEdge
authorSelectionEdge =
    NodeWithAuthorToUserConnectionEdge.node authorSelection
        |> nonNullOrFail


authorSelection : SelectionSet User Taco.Object.User
authorSelection =
    SelectionSet.map4 User
        User.firstName
        User.nickname
        (User.avatar avatarOptArgs avatarSelection)
        User.description


avatarSelection : SelectionSet Avatar Taco.Object.Avatar
avatarSelection =
    SelectionSet.map2 Avatar
        Avatar.url
        Avatar.isRestricted


avatarOptArgs : User.AvatarOptionalArguments -> User.AvatarOptionalArguments
avatarOptArgs args =
    { size = Graphql.OptionalArgument.Present 256
    , forceDefault = Graphql.OptionalArgument.Null
    , rating = Graphql.OptionalArgument.Null
    }


postTitleArgs : Post.TitleOptionalArguments -> Post.TitleOptionalArguments
postTitleArgs args =
    { format = Graphql.OptionalArgument.Null }


postContentArgs : Post.ContentOptionalArguments -> Post.ContentOptionalArguments
postContentArgs args =
    { format = Graphql.OptionalArgument.Present Taco.Enum.PostObjectFieldFormatEnum.Rendered }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        content =
            case model.post of
                RemoteData.Success data ->
                    case data of
                        Just res ->
                            div [] [ viewPost res model ]

                        _ ->
                            div []
                                [ h1 []
                                    [ text "Sorry! This is not the taco you're looking for" ]
                                , text "The blog post you're looking for couldn't be found."
                                ]

                RemoteData.Loading ->
                    div [ class "page-spinner" ]
                        [ viewSpinner
                        , span [ class "sr-only" ] [ text "Loading posts..." ]
                        , span [ ariaHidden True ] [ text " Prepping Tacos" ]
                        ]

                _ ->
                    div []
                        [ h1 []
                            [ text "Sorry! This is not the taco you're looking for" ]
                        , text "The blog post you're looking for couldn't be found."
                        ]
    in
    div [ class "page animate__animated animate__backInUp" ]
        [ div [] [ content ] ]


viewPost : Post -> Model -> Html Msg
viewPost post model =
    let
        title =
            justOrEmpty post.title

        dateResult =
            Iso8601.toTime (justOrEmpty post.date)

        timeString =
            case dateResult of
                Ok res ->
                    getTime res

                _ ->
                    "Unknown Date"

        postInfo =
            case post.author of
                Just res ->
                    "By " ++ justOrEmpty res.firstName ++ " on " ++ timeString

                _ ->
                    "Posted on " ++ timeString

        author =
            case post.author of
                Just res ->
                    viewAuthor res

                _ ->
                    text ""

        content =
            justOrEmpty post.content

        comments =
            case post.comments of
                Just res ->
                    res

                _ ->
                    []

        commentCount =
            case post.commentCount of
                Just res ->
                    res

                _ ->
                    0

        addCommentArea =
            case model.newComment of
                RemoteData.NotAsked ->
                    viewAddComment ( model.commentName, model.commentEmail, model.commentContent )

                RemoteData.Loading ->
                    div [ class "alert info" ] [ viewSpinner, text " Submitting Your Taco Comment" ]

                RemoteData.Failure _ ->
                    div [ class "alert error" ]
                        [ Icon.viewStyled [] Icon.timesCircle
                        , text "Failed to submit your comment, sorry!"
                        ]

                RemoteData.Success result ->
                    div [ class "alert success" ]
                        [ Icon.viewStyled [] Icon.checkCircle
                        , text "Your comment has been received and is awaiting approval! Thanks! :)"
                        ]

        ------- This code is for later, if we auto-approv
        -- case result ofe comments
        -- Just commentPayload ->
        --     case commentPayload.comment of
        --         Just comment ->
        --             viewComment comment
        --         _ ->
        --             span [] []
        -- Nothing ->
        --     span [] []
    in
    div [ class "post" ]
        [ h1 [ class "post-title" ] [ text title ]
        , div [ class "post-subtitle" ]
            [ div [ class "post-info" ] [ text postInfo ]
            , viewShareButtons post.uri title
            ]
        , div [ class "post-body" ]
            (Html.Parser.Util.toVirtualDom (getParsedHtml content))
        , author
        , viewComments comments commentCount
        , div [ class "add-comment", id "add-comment" ]
            [ h3 [] [ text "Post a Comment" ], addCommentArea ]
        ]


viewAuthor : User -> Html Msg
viewAuthor author =
    let
        firstName =
            justOrEmpty author.firstName

        descriptionLines =
            String.split "\n" (justOrEmpty author.description)

        avatar =
            case author.avatar of
                Just res ->
                    justOrEmpty res.url

                _ ->
                    ""
    in
    div [ class "post-about" ]
        [ div [ class "about-image" ]
            [ img avatar [ src avatar ] ]
        , div [ class "about-text" ]
            [ span [ class "text-bold text-large stack-m" ] [ text <| firstName ]
            , span [ class "text-italic text-light stack-s" ] [ text "Taco Consumer, Part-time Genius, Taco Developer, Author" ]
            , span [] (List.map (\line -> p [] [ text line ]) descriptionLines)
            ]
        ]


viewComments : List Comment -> Int -> Html msg
viewComments comments count =
    div [ class "comments" ]
        [ h3 [] [ text <| "Comments (" ++ String.fromInt count ++ ")" ]
        , div [] (List.map viewComment comments)
        ]


viewComment : Comment -> Html msg
viewComment comment =
    let
        author =
            case comment.author of
                Just res ->
                    justOr res.name "Anonymous"

                _ ->
                    "Anonymous"

        dateResult =
            case comment.date of
                Just res ->
                    String.slice 0 10 res ++ "T" ++ String.slice 11 21 res

                _ ->
                    "Unknown Date"

        isoTime =
            Iso8601.toTime dateResult

        timeString =
            case isoTime of
                Ok string ->
                    getDateTime string

                _ ->
                    ""

        content =
            justOrEmpty comment.content
    in
    div [ class "comment" ]
        [ span [ class "comment-author" ] [ text author ]
        , span [ class "comment-date" ] [ text timeString ]
        , span [ class "comment-content" ] (Html.Parser.Util.toVirtualDom (getParsedHtml content))
        ]


viewAddComment : ( String, String, String ) -> Html Msg
viewAddComment ( name, email, content ) =
    let
        isDisabled =
            if String.isEmpty name || String.isEmpty email || String.isEmpty content then
                True

            else
                False
    in
    form [ onSubmit SubmittedNewComment, class "new-comment" ]
        [ label []
            [ text "Name (required):"
            , inputText "" [ onInput ChangedNameInput, required True, value name ]
            ]
        , label []
            [ text "Email (required):"
            , inputText "" [ onInput ChangedEmailInput, required True, value email ]
            ]
        , label []
            [ text "Comment:"
            , textarea [ onInput ChangedContentInput, required True ] [ text content ]
            ]
        , button [ type_ "button", onClick SubmittedNewComment, disabled isDisabled, ariaLabel "Post Comment" ]
            [ text "Comment" ]
        ]


monthToString : Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


getParsedHtml : String -> List Node
getParsedHtml html =
    case Html.Parser.run html of
        Ok tree ->
            tree

        _ ->
            []


getTime : Time.Posix -> String
getTime dateTime =
    let
        day =
            Time.toDay Time.utc dateTime

        month =
            Time.toMonth Time.utc dateTime

        year =
            Time.toYear Time.utc dateTime
    in
    monthToString month ++ " " ++ String.fromInt day ++ ", " ++ String.fromInt year


getDateTime : Time.Posix -> String
getDateTime dateTime =
    let
        date =
            getTime dateTime

        twentyFourHour =
            Time.toHour Time.utc dateTime

        hour =
            if twentyFourHour > 12 then
                twentyFourHour - 12

            else if twentyFourHour == 0 then
                12

            else
                twentyFourHour

        amPm =
            if twentyFourHour >= 12 then
                "PM"

            else
                "AM"

        minute =
            Time.toMinute Time.utc dateTime
    in
    date ++ " at " ++ String.fromInt hour ++ ":" ++ String.fromInt minute ++ amPm


viewSpinner : Html msg
viewSpinner =
    span [ class "spinner" ] [ Icon.viewStyled [] IconReg.lemon ]


justOrEmpty : Maybe String -> String
justOrEmpty maybe =
    case maybe of
        Just res ->
            res

        Nothing ->
            ""


justOr : Maybe String -> String -> String
justOr maybe alt =
    case maybe of
        Just res ->
            res

        Nothing ->
            alt


viewIf : Bool -> Html Msg -> Html Msg
viewIf condition element =
    if condition then
        element

    else
        text ""
