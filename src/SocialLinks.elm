module SocialLinks exposing (viewShareButtons)

import Accessibility as Html exposing (Html, button, div, h1, span, text)
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Regular as IconReg
import FontAwesome.Solid as Icon
import Html exposing (Html, a)
import Html.Attributes exposing (class, disabled, href)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)


viewShareButtons : String -> String -> Html msg
viewShareButtons postUri postTitle =
    let
        baseUrl =
            "http://tacotandom.com"

        postUrl =
            baseUrl ++ postUri

        facebookUrl =
            "https://www.facebook.com/sharer/sharer.php?u=" ++ postUrl

        pinterestUrl =
            "https://pinterest.com/pin/create/button/?url=" ++ postUrl ++ "&media=&description=" ++ postTitle

        twitterUrl =
            "http://twitter.com/share?text=" ++ postTitle ++ "&url=" ++ postUrl ++ "&hashtags=TacoTandem"

        linkedInUrl =
            "https://www.linkedin.com/sharing/share-offsite/?url=" ++ postUrl
    in
    div [ class "post-share" ]
        [ viewSocialButton "Facebook" "social-fb" facebookUrl Icon.facebook
        , viewSocialButton "Pinterest" "social-pinterest" pinterestUrl Icon.pinterest
        , viewSocialButton "Twitter" "social-twitter" twitterUrl Icon.twitter
        , viewSocialButton "LinkedIn" "social-linkedin" linkedInUrl Icon.linkedin
        ]


viewSocialButton : String -> String -> String -> Icon -> Html msg
viewSocialButton name class_ url icon =
    a [ href url, class class_, ariaLabel <| "Share to " ++ name ++ " (Opens New Window)" ] [ Icon.viewStyled [] icon ]
