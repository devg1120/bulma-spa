module Main exposing (main)

-- import Page exposing (Page, Model)
--ADD COMPONENT-- import

import Animation exposing (px)
import Api exposing (Cred)
import Article.Slug exposing (Slug)
import Avatar exposing (Avatar)
import Browser exposing (..)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (href, rel)
import Json.Decode as Decode exposing (Value)
import Page exposing (Msg, Navbar, Page)
import Page.Article as Article
import Page.Article.Editor as Editor
import Page.Blank as Blank
import Page.Component01.Home as Comp01
import Page.Component02.Home as Comp02
import Page.Component03.Home as Comp03
import Page.Component04.Home as Comp04
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Register as Register
import Page.Settings as Settings
import Route exposing (Route)
import Save exposing (SaveModel)
import Session exposing (Session)
import Task
import Time
import Url exposing (Url)
import Username exposing (Username)
import Viewer exposing (Viewer)



-- NOTE: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, it's possible
-- that most of this file may become unnecessary in a future release of Elm.
-- Avoid putting things in this module unless there is no alternative!
-- See https://discourse.elm-lang.org/t/elm-spa-in-0-19/1800/2 for more.


stylesheet : Html msg
stylesheet =
    node "link"
        [ rel "stylesheet"

        -- , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
        , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.0/css/bulma.min.css"

        -- , href "https://demo.productionready.io/main.css"
        ]
        []


type Model
    = Redirect Session Navbar SaveModel
    | NotFound Session Navbar SaveModel
      --    | Page Page.Model
    | Home Home.Model Navbar SaveModel
    | Settings Settings.Model Navbar SaveModel
    | Login Login.Model Navbar SaveModel
    | Register Register.Model Navbar SaveModel
    | Profile Username Profile.Model Navbar SaveModel
    | Article Article.Model Navbar SaveModel
    | Editor (Maybe Slug) Editor.Model Navbar SaveModel
      --ADD COMPONENT-- Type Model
    | Comp01 Comp01.Model Navbar SaveModel
    | Comp02 Comp02.Model Navbar SaveModel
    | Comp03 Comp03.Model Navbar SaveModel
    | Comp04 Comp04.Model Navbar SaveModel



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        navbar =
            { docmenu_open = False
            , appmenu_open = False

            -- ,save_model = Dict.empty
            }

      --ADD COMPONENT-- init
        savemodel =
            { comp01 = { save = False, model = {} }
            , comp02 = { save = False, model = {} }
            , comp03 = { save = False, model = { counter = 0 } }
            , comp04 = { save = False, model = { counter = 0 } }
            }
    in
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer) navbar savemodel)



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage navbar_ page toMsg config =
            let
                { title, body } =
                    Page.view viewer page navbar_ config
            in
            { title = title
            , body = [ stylesheet ] ++ List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ navbar savemodel ->
            Page.view viewer Page.Other navbar Blank.view

        NotFound _ navbar savemodel ->
            Page.view viewer Page.Other navbar NotFound.view

        Settings settings navbar savemodel ->
            viewPage navbar Page.Other GotSettingsMsg (Settings.view settings)

        Home home navbar savemodel ->
            viewPage navbar Page.Home GotHomeMsg (Home.view home)

        Login login navbar savemodel ->
            viewPage navbar Page.Other GotLoginMsg (Login.view login)

        Register register navbar savemodel ->
            viewPage navbar Page.Other GotRegisterMsg (Register.view register)

        Profile username profile navbar savemodel ->
            viewPage navbar (Page.Profile username) GotProfileMsg (Profile.view profile)

        Article article navbar savemodel ->
            viewPage navbar Page.Other GotArticleMsg (Article.view article)

        Editor Nothing editor navbar savemodel ->
            viewPage navbar Page.NewArticle GotEditorMsg (Editor.view editor)

        Editor (Just _) editor navbar savemodel ->
            viewPage navbar Page.Other GotEditorMsg (Editor.view editor)

        --ADD COMPONENT-- view
        Comp01 comp01 navbar savemodel ->
            viewPage navbar Page.Other GotComp01Msg (Comp01.view comp01)

        Comp02 comp02 navbar savemodel ->
            viewPage navbar Page.Other GotComp02Msg (Comp02.view comp02)

        Comp03 comp03 navbar savemodel ->
            viewPage navbar Page.Other GotComp03Msg (Comp03.view comp03)

        Comp04 comp04 navbar savemodel ->
            viewPage navbar Page.Other GotComp04Msg (Comp04.view comp04)



-- UPDATE


type MenuId
    = DocMenu
    | AppMenu
    | AllOff


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
      --    | NavMenu Page.Msg
    | MenuOpen MenuId
      --ADD COMPONENT-- type Msg
    | GotComp01Msg Comp01.Msg
    | GotComp02Msg Comp02.Msg
    | GotComp03Msg Comp03.Msg
    | GotComp04Msg Comp04.Msg


toSession : Model -> Session
toSession page =
    case page of
        Redirect session _ savemodel ->
            session

        NotFound session _ savemodel ->
            session

        Home home _ savemodel ->
            Home.toSession home

        Settings settings _ savemodel ->
            Settings.toSession settings

        Login login _ savemodel ->
            Login.toSession login

        Register register _ saveodel ->
            Register.toSession register

        Profile _ profile _ savemodel ->
            Profile.toSession profile

        Article article _ savemodel ->
            Article.toSession article

        Editor _ editor _ savemodel ->
            Editor.toSession editor

        --ADD COMPONENT-- toSession
        Comp01 comp01 _ savemodel ->
            Comp01.toSession comp01

        Comp02 comp02 _ savemodel ->
            Comp02.toSession comp02

        Comp03 comp03 _ savemodel ->
            Comp03.toSession comp03

        Comp04 comp04 _ savemodel ->
            Comp04.toSession comp04


toNavbar : Model -> Page.Navbar
toNavbar page =
    case page of
        Redirect session navbar savemodel ->
            navbar

        NotFound session navbar savemodel ->
            navbar

        Home home _ savemodel ->
            Home.toNavbar home

        Settings settings _ savemodel ->
            Settings.toNavbar settings

        Login login _ savemodel ->
            Login.toNavbar login

        Register register _ savemodel ->
            Register.toNavbar register

        Profile _ profile _ savemodel ->
            Profile.toNavbar profile

        Article article _ savemodel ->
            Article.toNavbar article

        Editor _ editor _ savemodel ->
            Editor.toNavbar editor

        --ADD COMPONENT-- toNavbar
        Comp01 comp01 _ savemodel ->
            Comp01.toNavbar comp01

        Comp02 comp02 _ savemodel ->
            Comp02.toNavbar comp02

        Comp03 comp03 _ savemodel ->
            Comp03.toNavbar comp03

        Comp04 comp04 _ savemodel ->
            Comp04.toNavbar comp04


toSaveModel : Model -> Save.SaveModel
toSaveModel page =
    case page of
        Redirect session navbar savemodel ->
            savemodel

        NotFound session navbar savemodel ->
            savemodel

        Home home _ savemodel ->
            Home.toSaveModel home

        Settings settings _ savemodel ->
            Settings.toSaveModel settings

        Login login _ savemodel ->
            Login.toSaveModel login

        Register register _ savemodel ->
            Register.toSaveModel register

        Profile _ profile _ savemodel ->
            Profile.toSaveModel profile

        Article article _ savemodel ->
            Article.toSaveModel article

        Editor _ editor _ savemodel ->
            Editor.toSaveModel editor

        --ADD COMPONENT-- toSaveModel
        Comp01 comp01 _ savemodel ->
            -- Comp01.toSaveModel comp01
            savemodel

        Comp02 comp02 _ savemodel ->
            -- Comp02.toSaveModel comp02
            savemodel

        Comp03 comp03 _ savemodel ->
            -- Comp03.toSaveModel comp03
            savemodel

        Comp04 comp04 _ savemodel ->
            -- Comp04.toSaveModel comp04
            savemodel


updateNavbar : Model -> Navbar -> Model
updateNavbar page new_navbar =
    case page of
        Redirect session navbar savemodel ->
            Redirect session new_navbar savemodel

        NotFound session navbar savemodel ->
            NotFound session new_navbar savemodel

        Home home _ savemodel ->
            let
                new_home =
                    Home.setNavbar home new_navbar
            in
            Home new_home new_navbar savemodel

        Settings settings _ savemodel ->
            let
                new_settings =
                    Settings.setNavbar settings new_navbar
            in
            Settings new_settings new_navbar savemodel

        Login login _ savemodel ->
            let
                new_login =
                    Login.setNavbar login new_navbar
            in
            Login new_login new_navbar savemodel

        Register register _ savemodel ->
            let
                new_register =
                    Register.setNavbar register new_navbar
            in
            Register new_register new_navbar savemodel

        Profile a profile _ savemodel ->
            let
                new_profile =
                    Profile.setNavbar profile new_navbar
            in
            Profile a new_profile new_navbar savemodel

        Article article _ savemodel ->
            let
                new_article =
                    Article.setNavbar article new_navbar
            in
            Article new_article new_navbar savemodel

        Editor a editor _ savemodel ->
            let
                new_editor =
                    Editor.setNavbar editor new_navbar
            in
            Editor a new_editor new_navbar savemodel

        --ADD COMPONENT-- updateNavbar
        Comp01 comp01 _ savemodel ->
            let
                new_comp01 =
                    Comp01.setNavbar comp01 new_navbar
            in
            Comp01 new_comp01 new_navbar savemodel

        Comp02 comp02 _ savemodel ->
            let
                new_comp02 =
                    Comp02.setNavbar comp02 new_navbar
            in
            Comp02 new_comp02 new_navbar savemodel

        Comp03 comp03 _ savemodel ->
            let
                new_comp03 =
                    Comp03.setNavbar comp03 new_navbar
            in
            Comp03 new_comp03 new_navbar savemodel

        Comp04 comp04 _ savemodel ->
            let
                new_comp04 =
                    Comp04.setNavbar comp04 new_navbar
            in
            Comp04 new_comp04 new_navbar savemodel




changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        _ =
            Debug.log "** changeRouteTo model:" model

        session =
            toSession model

        navbar =
            toNavbar model

        savemodel =
            toSaveModel model

        _ =
            Debug.log "changeRouteTo savemodel: " savemodel
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session navbar savemodel, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.NewArticle ->
            Editor.initNew session navbar savemodel
                |> updateWith (Editor Nothing) navbar savemodel GotEditorMsg model

        Just (Route.EditArticle slug) ->
            Editor.initEdit session slug navbar savemodel
                |> updateWith (Editor (Just slug)) navbar savemodel GotEditorMsg model

        Just Route.Settings ->
            Settings.init session navbar savemodel
                |> updateWith Settings navbar savemodel GotSettingsMsg model

        Just Route.Home ->
            Home.init session navbar savemodel
                |> updateWith Home navbar savemodel GotHomeMsg model

        Just Route.Login ->
            Login.init session navbar savemodel
                |> updateWith Login navbar savemodel GotLoginMsg model

        Just Route.Register ->
            Register.init session navbar savemodel
                |> updateWith Register navbar savemodel GotRegisterMsg model

        Just (Route.Profile username) ->
            Profile.init session username navbar savemodel
                |> updateWith (Profile username) navbar savemodel GotProfileMsg model

        Just (Route.Article slug) ->
            Article.init session slug navbar savemodel
                |> updateWith Article navbar savemodel GotArticleMsg model

        --ADD COMPONENT-- changeRouteTo
        Just Route.Comp01 ->
            Comp01.init session navbar savemodel
                |> updateWith Comp01 navbar savemodel GotComp01Msg model

        Just Route.Comp02 ->
            Comp02.init session navbar savemodel
                |> updateWith Comp02 navbar savemodel GotComp02Msg model

        Just Route.Comp03 ->
            Comp03.init session navbar savemodel
                |> updateWith Comp03 navbar savemodel GotComp03Msg model

        Just Route.Comp04 ->
            Comp04.init session navbar savemodel
                |> updateWith Comp04 navbar savemodel GotComp04Msg model




saveModel : Model -> Model
saveModel model =
    --ADD COMPONENT-- saveModel
    case model of
        Comp03 comp03 navbar savemodel ->
            let
                new_savemodel =
                    { savemodel | comp03 = { save = True, model = { counter = comp03.counter } } }

                new_model =
                    Comp03 comp03 navbar new_savemodel
            in
            new_model

        Comp04 comp04 navbar savemodel ->
            let
                new_savemodel =
                    { savemodel | comp04 = { save = True, model = { counter = comp04.counter } } }

                new_model =
                    Comp04 comp04 navbar new_savemodel
            in
            new_model

        _ ->
            -- let _ = Debug.log "** Other"   0 in
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --let    _ = Debug.log "** Main update msg:" msg  in
    --let    _ = Debug.log "** Main update model:" model  in
    case ( msg, model ) of
        ( MenuOpen menuid, _ ) ->
            -- let _ = Debug.log "-- Main update msg:  MenuOpen" menuid in
            -- let _ = Debug.log "MenuOpen DocMenu" menuid in
            let
                navbar =
                    toNavbar model

                model_ =
                    updateNavbar model (toggleMenu menuid navbar)
            in
            ( model_, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            -- let _ = Debug.log "-- Main update msg:  ClickedLink urlRequest" msg in
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            if url.path == "/navbar/docmenu_open" then
                                update (MenuOpen DocMenu) model

                            else if url.path == "/navbar/appmenu_open" then
                                update (MenuOpen AppMenu) model

                            else
                                ( model, Cmd.none )

                        Just _ ->
                            let
                                new_model =
                                    saveModel model

                                ( model_, cmd_ ) =
                                    update (MenuOpen AllOff) new_model

                            in
                            ( model_
                            , Nav.pushUrl (Session.navKey (toSession model_)) (Url.toString url)
                            )

                -- Browser.External href ->
                Browser.External href ->
                    update (MenuOpen AllOff) model

        ( ChangedUrl url, _ ) ->
            let
                _ =
                    Debug.log "** ChangedUrl:" model
            in
            changeRouteTo (Route.fromUrl url) model

        ( GotSettingsMsg subMsg, Settings settings navbar savemodel ) ->
            Settings.update subMsg settings
                |> updateWith Settings navbar savemodel GotSettingsMsg model

        ( GotLoginMsg subMsg, Login login navbar savemodel ) ->
            Login.update subMsg login
                |> updateWith Login navbar savemodel GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register navbar savemodel ) ->
            Register.update subMsg register
                |> updateWith Register navbar savemodel GotRegisterMsg model

        ( GotHomeMsg subMsg, Home home navbar savemodel ) ->
            Home.update subMsg home
                |> updateWith Home navbar savemodel GotHomeMsg model

        ( GotProfileMsg subMsg, Profile username profile navbar savemodel ) ->
            Profile.update subMsg profile
                |> updateWith (Profile username) navbar savemodel GotProfileMsg model

        ( GotArticleMsg subMsg, Article article navbar savemodel ) ->
            Article.update subMsg article
                |> updateWith Article navbar savemodel GotArticleMsg model

        ( GotEditorMsg subMsg, Editor slug editor navbar savemodel ) ->
            Editor.update subMsg editor
                |> updateWith (Editor slug) navbar savemodel GotEditorMsg model

        ( GotSession session, Redirect _ navbar savemodel ) ->
            ( Redirect session navbar savemodel
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        --ADD COMPONENT-- update
        ( GotComp01Msg subMsg, Comp01 comp01 navbar savemodel ) ->
            Comp01.update subMsg comp01
                |> updateWith Comp01 navbar savemodel GotComp01Msg model

        ( GotComp02Msg subMsg, Comp02 comp02 navbar savemodel ) ->
            Comp02.update subMsg comp02
                |> updateWith Comp02 navbar savemodel GotComp02Msg model

        ( GotComp03Msg subMsg, Comp03 comp03 navbar savemodel ) ->
            Comp03.update subMsg comp03
                |> updateWith Comp03 navbar savemodel GotComp03Msg model

        ( GotComp04Msg subMsg, Comp04 comp04 navbar savemodel ) ->
            Comp04.update subMsg comp04
                |> updateWith Comp04 navbar savemodel GotComp04Msg model

        ( _, _ ) ->
            let
                _ =
                    Debug.log "** Main update msg: default"
            in
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


toggleMenu : MenuId -> Navbar -> Navbar
toggleMenu menuid navbar =
    case menuid of
        DocMenu ->
            let
                new_navbar =
                    { docmenu_open = not navbar.docmenu_open
                    , appmenu_open = False
                    }
            in
            new_navbar

        AppMenu ->
            let
                new_navbar =
                    { -- docmenu_open = navbar.docmenu_open
                      docmenu_open = False
                    , appmenu_open = not navbar.appmenu_open
                    }
            in
            new_navbar

        AllOff ->
            let
                new_navbar =
                    { docmenu_open = False
                    , appmenu_open = False
                    }
            in
            new_navbar


updateWith : (subModel -> Navbar -> SaveModel -> Model) -> Navbar -> SaveModel -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel navbar savemodel toMsg model ( subModel, subCmd ) =
    ( toModel subModel navbar savemodel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ navbar savemodel ->
            Sub.none

        Redirect _ navbar savemodel ->
            Session.changes GotSession (Session.navKey (toSession model))

        Settings settings navbar savemodel ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home navbar savemodel ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login navbar savemodel ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register navbar savemodel ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Profile _ profile navbar savemodel ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)

        Article article navbar savemodel ->
            Sub.map GotArticleMsg (Article.subscriptions article)

        Editor _ editor navbar savemodel ->
            Sub.map GotEditorMsg (Editor.subscriptions editor)

        --ADD COMPONENT-- subscriptions
        Comp01 comp01 navbar savemodel ->
            Sub.map GotComp01Msg (Comp01.subscriptions comp01)

        Comp02 comp02 navbar savemodel ->
            Sub.map GotComp02Msg (Comp02.subscriptions comp02)

        Comp03 comp03 navbar savemodel ->
            Sub.map GotComp03Msg (Comp03.subscriptions comp03)

        Comp04 comp04 navbar savemodel ->
            Sub.map GotComp04Msg (Comp04.subscriptions comp04)



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
