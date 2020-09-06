module Page exposing (Page(..),  Navbar, Msg, view, viewErrors )
-- module Page exposing (Page(..), Model, view, viewErrors)

import Dict exposing (Dict)

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

type alias Navbar = {
      docmenu_open : Bool
     ,appmenu_open : Bool
     }




type Msg = NoOp                                                             
     | DocMenuOpen | AppMenuOpen

{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}

view : Maybe Viewer -> Page -> Navbar -> { title : String, content : Html msg } -> Document msg
view maybeViewer page navbar { title, content } =
    -- let _ = Debug.log "TITLE:" title in
    let
      content_ = basePanel content
    in
    { title = title ++ " - Conduit"
    -- , body = viewHeader page maybeViewer :: content :: [ viewFooter ]
    -- , body = viewHeader page maybeViewer navbar :: content :: []
    , body = viewHeader title page maybeViewer navbar :: content_ :: []
    }

basePanel: Html msg -> Html msg
basePanel content =
   section NotSpaced
        [ style  "margin-top" "200px"
        , style  "margin-left" "0px"
        , style  "margin-right" "0px"
        , style  "margin-bottom" "0px"
        , paddingless
        ]
        --[ container  []
        --[ fullHDContainer  [marginless,paddingless]
        [ fluidContainer  [marginless,paddingless]
          [content]
        ]

basePanel_: Html msg -> Html msg
basePanel_ content =
   section NotSpaced
        [ style  "display" "inline-block"
        , style  "margin-top" "40px"
                                                                            
        ]
        [ container  []
          [content]
        ]
---------------- START
commonNavbarModifiers
    = {navbarModifiers
       | color = Info
       , transparent = False
       }

commonButtonModifiers
    = {buttonModifiers
       | size = Small
       }

viewHeader : String -> Page -> Maybe Viewer -> Navbar -> Html msg
viewHeader title page maybeViewer navbar =
    case maybeViewer of
        Just viewer ->
            -- let _ = Debug.log "LogOn:" page in
            viewHeaderLogOn  title page viewer navbar
        Nothing ->
            -- let _ = Debug.log "LogOff:" page in
            viewHeaderLogOff title page  navbar


viewHeaderLogOn : String -> Page ->  Viewer -> Navbar -> Html msg
viewHeaderLogOn title page viewer navbar =
    let
        username =
            Viewer.username viewer

        avatar =
            Viewer.avatar viewer
    in
    fixedNavbar Top commonNavbarModifiers 
    [style "background-color" "#0066bb"
    ,style "z-index" "1000"
    ]
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
    ,
      navbarItem False []
        [ text title
        ]
    , navbarMenu False []
      [ navbarStart [] 
        [ navbarItemLink False [] [ text "Home"  ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown navbar.docmenu_open Down []
                ( navbarLink [ class "is-arrowless"
                              , href "/navbar/docmenu_open"
                              ,  attribute "data-target" "Docs"
                             ] [ text "Docs" ] )
          [ navbarDropdown True Left []
            [ navbarItemLink False [] [ text "Crud"     ]
            , navbarItemLink False [] [ text "Detritus" ]
            , navbarItemLink True  [] [ text "Refuse"   ]
            , navbarItemLink False [] [ text "Trash"    ]
            ]
          ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown navbar.appmenu_open Down [] 
                ( navbarLink [ class "is-arrowless"
                              , href "/navbar/appmenu_open"
                              -- , href "http://navbar/appmenu_open"
                              ,  attribute "data-target" "App"
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
            [ 
              controlButton { commonButtonModifiers | color = Primary } [] [Route.href Route.Home ]             [ text "Home" ] 
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href Route.NewArticle]        [ text "New Post" ]
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href Route.Settings]          [ text "Settings" ]
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href (Route.Profile username) ] [ text (Username.toString username) ]
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href Route.Logout]            [ text "Sign out" ] 
            ]
          ]
        ]
      ]
    ]


viewHeaderLogOff : String -> Page ->  Navbar -> Html msg
viewHeaderLogOff title page  navbar =
    fixedNavbar Top commonNavbarModifiers 
    [style "background-color" "#5CB661"
    ,style "z-index" "1000"
    ]
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
    ,
      navbarItem False []
        [ text title
        ]
      
    , navbarMenu False []
      [ navbarStart [] 
        [ navbarItemLink False [] [ text "Home"  ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown navbar.docmenu_open Down [] 
                ( navbarLink [ class "is-arrowless"
                              , href "/navbar/docmenu_open"
                              ,  attribute "data-target" "Docs"
                             ] [ text "Docs" ] )
          [ navbarDropdown True Left [] 
            [ navbarItemLink False [] [ text "Crud"     ]
            , navbarItemLink False [] [ text "Detritus" ]
            , navbarItemLink True  [] [ text "Refuse"   ]
            , navbarItemLink False [] [ text "Trash"    ]
            ]
          ]
        -- , navbarItemDropdown False Down [] 
        , navbarItemDropdown navbar.appmenu_open Down [] 
                ( navbarLink [ class "is-arrowless"
                              , href "/navbar/appmenu_open"
                              -- , href "http://navbar/appmenu_open"
                              ,  attribute "data-target" "App"
                             ] [ text "App" ] )
          [ navbarDropdown True Left [] 
            [ navbarItemLink False [] [ text "Article" ]
--ADD COMPONENT-- navbar
            , navbarItemLink False [Route.href Route.Comp01] [ text "comp01" ]
            , navbarItemLink False [Route.href Route.Comp02] [ text "comp02" ]
            , navbarItemLink False [Route.href Route.Comp03] [ text "comp03" ]
            , navbarItemLink False [Route.href Route.Comp04] [ text "comp04" ]
            , navbarItemLink True  [Route.href Route.Settings] [ text "Settings" ]
            , navbarItemLink True  [Route.href Route.Register] [ text "Register" ]
            , navbarItemLink False [Route.href Route.Login] [ text "Login" ]
            ]
          ]
        ]
      , navbarEnd [] 
        [ navbarItem False []
          [ fields Left []
            [ 
              controlButton { commonButtonModifiers | color = Primary } [] [Route.href Route.Home ] [ text "Home" ] 
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href Route.Login] [ text "Sign in" ]
             ,controlButton { commonButtonModifiers | color = Info    } [] [Route.href Route.Register] [ text "Sign up" ] 
            ]
          ]
        ]
      ]
    ]

-----------END

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
