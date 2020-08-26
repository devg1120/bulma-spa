module Page exposing (Page(..), view, viewErrors)

import Api exposing (Cred)
import Avatar
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Username exposing (Username)
import Viewer exposing (Viewer)

import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
-- import Bulma.Columns as Columns exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)

import Html exposing ( Html, node, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i )
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class )

{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | Home
    | Login
    | Register
    | Settings
    | Profile Username
    | NewArticle


type Msg = NoOp 
    | DocMenuOpen | AppMenuOpen

{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title ++ " - Conduit"
    -- , body = viewHeader page maybeViewer :: content :: [ viewFooter ]
    , body = viewHeader page maybeViewer :: content :: []
    }

---------------- START
commonNavbarModifiers
    = {navbarModifiers
       | color = Info
       , transparent = False
       }
viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    fixedNavbar Top commonNavbarModifiers 
    [style "background-color" "#0066bb"]
    [ navbarBrand []
      ( navbarBurger False []
        [ span [] []
        , span [] []
        , span [] []
        ]
      )
      [ navbarItem False []
        [ img [ src "https://bulma.io/images/bulma-logo.png" ] []
        ]
      ]
    , navbarMenu False []
      [ navbarStart [] 
        [ navbarItemLink False [] [ text "Home"  ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown False Down [] 
                ( navbarLink [ class "is-arrowless"
                             ] [ text "Docs" ] )
          [ navbarDropdown True Left [] 
            [ navbarItemLink False [] [ text "Crud"     ]
            , navbarItemLink False [] [ text "Detritus" ]
            , navbarItemLink True  [] [ text "Refuse"   ]
            , navbarItemLink False [] [ text "Trash"    ]
            ]
          ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown True Down [] 
                ( navbarLink [ class "is-arrowless"
                             -- , onClick AppMenuOpen
                             ] [ text "App" ] )
          [ navbarDropdown True Left [] 
            [ navbarItemLink False [] [ text "Article" ]
            , navbarItemLink False [Route.href Route.Home] [ text "Home" ]
            , navbarItemLink True  [Route.href Route.Settings] [ text "Settings" ]
            , navbarItemLink True  [Route.href Route.Register] [ text "Register" ]
            , navbarItemLink False [Route.href Route.Login] [ text "Login" ]
            ]
          ]
        ]
      , navbarEnd [] 
        [ navbarItem False []
          [ fields Left []
            [ controlButton { buttonModifiers | color = Info    } [] [Route.href Route.Login]  [ icon Standard [] [ i [ class "fa fa-twitter"  ] [] ], span [] [ text "Login"    ] ]
            , controlButton { buttonModifiers | color = Primary } [] [Route.href Route.Home] [ icon Standard [] [ i [ class "fa fa-download" ] [] ], span [] [ text "Home" ] ]
            ]
            , controlButton { buttonModifiers | color = Info  } [] [] [ icon Standard [] [ i [ class "fa fa-download" ] [] ], span [] [ text "Signin" ] ]
          ]
        ]
      ]
    ]

-----------END
{--
_viewHeader : Page -> Maybe Viewer -> Html msg
_viewHeader page maybeViewer =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                navbarLink page Route.Home [ text "Home" ]
                    :: viewMenu page maybeViewer
            ]
        ]
--}
{--
viewMenu : Page -> Maybe Viewer -> List (Html msg)
viewMenu page maybeViewer =
    let
        linkTo =
            navbarLink page
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                avatar =
                    Viewer.avatar viewer
            in
            [ linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text "\u{00A0}New Post" ]
            , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text "\u{00A0}Settings" ]
            , linkTo
                (Route.Profile username)
                [ img [ class "user-pic", Avatar.src avatar ] []
                , Username.toHtml username
                ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.Register [ text "Sign up" ]
            ]


viewFooter : Html msg
viewFooter =
    Html.footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]

navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]

--}


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Register, Route.Register ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        ( Profile pageUsername, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        ( NewArticle, Route.NewArticle ) ->
            True

        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ Html.button [ onClick dismissErrors ] [ text "Ok" ] ]
