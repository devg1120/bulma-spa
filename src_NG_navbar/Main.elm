module Main exposing (main)

import Api exposing (Cred)
import Article.Slug exposing (Slug)
import Avatar exposing (Avatar)
-- import Browser exposing (Document)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page, Navbar )
-- import Page exposing (Page, Msg)
-- import Page exposing (Page, Model)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Blank as Blank
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import Viewer exposing (Viewer)

import Html.Attributes exposing (href, rel)

-- NOTE: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, it's possible
-- that most of this file may become unnecessary in a future release of Elm.
-- Avoid putting things in this module unless there is no alternative!
-- See https://discourse.elm-lang.org/t/elm-spa-in-0-19/1800/2 for more.

stylesheet : Html msg
stylesheet =
  node "link"
  [ rel  "stylesheet"
  -- , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
  , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.0/css/bulma.min.css"
  -- , href "https://demo.productionready.io/main.css"
  ]
  []


type Model
    = Redirect Session Page.Navbar
    | NotFound Session Page.Navbar
--    | Page Page.Model
    | Home Home.Model Page.Navbar
    | Settings Settings.Model Page.Navbar
    | Login Login.Model Page.Navbar
    | Register Register.Model Page.Navbar
    | Profile Username Profile.Model Page.Navbar
    | Article Article.Model Page.Navbar
    | Editor (Maybe Slug) Editor.Model Page.Navbar

{--
type alias Navbar = {
     docmenu_open : Bool
     ,appmenu_open : Bool
     }
--}

-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model ->  Document Msg
view model  =
    let
        viewer =
            Session.viewer (toSession model)
{--
        navbar = 
           {
            docmenu_open = False
           ,appmenu_open = True
           }
--}
        viewPage navbar page toMsg config =
            let
                { title, body } =
                    Page.view viewer page navbar config
            in
            { title = title
            , body = [stylesheet] ++ List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ navbar ->
            Page.view viewer Page.Other navbar Blank.view

        NotFound _ navbar ->
            Page.view viewer Page.Other navbar NotFound.view

        Settings settings navbar ->
            viewPage navbar Page.Other GotSettingsMsg (Settings.view settings)

        Home home navbar ->
            viewPage navbar Page.Home GotHomeMsg (Home.view home)

        Login login navbar ->
            viewPage navbar Page.Other GotLoginMsg (Login.view login)

        Register register navbar ->
            viewPage navbar Page.Other GotRegisterMsg (Register.view register)

        Profile username profile navbar ->
            viewPage navbar (Page.Profile username) GotProfileMsg (Profile.view profile)

        Article article navbar ->
            viewPage navbar Page.Other GotArticleMsg (Article.view article)

        Editor Nothing editor navbar ->
            viewPage navbar Page.NewArticle GotEditorMsg (Editor.view editor)

        Editor (Just _) editor navbar ->
            viewPage navbar Page.Other GotEditorMsg (Editor.view editor)



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
--    | GotPageMsg Page.Msg
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotProfileMsg Profile.Msg
    | GotArticleMsg Article.Msg
    | GotEditorMsg Editor.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session navbar ->
            session

        NotFound session navbar ->
            session

        Home home navbar ->
            Home.toSession home 

        Settings settings navbar ->
            Settings.toSession settings 

        Login login navbar ->
            Login.toSession login 

        Register register navbar ->
            Register.toSession register 

        Profile _ profile navbar ->
            Profile.toSession profile

        Article article navbar ->
            Article.toSession article

        Editor _ editor navbar ->
            Editor.toSession editor


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        -- _ = Debug.log "** changeRouteTo"
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.NewArticle ->
            Editor.initNew session
                |> updateWith (Editor Nothing) GotEditorMsg model

        Just (Route.EditArticle slug) ->
            Editor.initEdit session slug
                |> updateWith (Editor (Just slug)) GotEditorMsg model

        Just Route.Settings ->
            Settings.init session
                |> updateWith Settings GotSettingsMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model

        Just (Route.Profile username) ->
            Profile.init session username
                |> updateWith (Profile username) GotProfileMsg model

        Just (Route.Article slug) ->
            Article.init session slug
                |> updateWith Article GotArticleMsg model


update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model  =
    let    _ = Debug.log "** Main update msg:" msg  in
    -- let    _ = Debug.log "** Main update model:" model  in
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            let _ = Debug.log "-- Main update msg:  ClickedLink urlRequest" msg in

            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                -- Browser.External href ->
                Browser.External href ->
                    -- ( model
                    -- , Nav.load href
                    -- )
                    let    _ = Debug.log "** Main update msg:  Browser.External href:" href  in

                    ( model,  Cmd.none )


--        ( ClickedLink , _ ) ->
--                    let    _ = Debug.log "** Main update msg:  ClickedLink External"   in
--                    ( model, Cmd.none )
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model
       

        ( GotSettingsMsg subMsg, Settings settings navbar) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotLoginMsg subMsg, Login login navbar) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register navbar) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

        ( GotHomeMsg subMsg, Home home navbar) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotProfileMsg subMsg, Profile username profile navbar) ->
            Profile.update subMsg profile
                |> updateWith (Profile username) GotProfileMsg model

        ( GotArticleMsg subMsg, Article article navbar) ->
            Article.update subMsg article
                |> updateWith Article GotArticleMsg model

        ( GotEditorMsg subMsg, Editor slug editor navbar) ->
            Editor.update subMsg editor
                |> updateWith (Editor slug) GotEditorMsg model

        ( GotSession session, Redirect _ navbar) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            let    _ = Debug.log "** Main update msg: default"   in
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ _ ->
            Sub.none

        Redirect _ _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Settings settings _ ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home _ ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login _ ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register _ ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Profile _ profile _ ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)

        Article article _ ->
            Sub.map GotArticleMsg (Article.subscriptions article)

        Editor _ editor _ ->
            Sub.map GotEditorMsg (Editor.subscriptions editor)



-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
