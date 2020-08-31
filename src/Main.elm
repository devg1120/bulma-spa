module Main exposing (main )

import Dict exposing (Dict)

import Api exposing (Cred)
import Article.Slug exposing (Slug)
import Avatar exposing (Avatar)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page, Navbar )
import Page exposing (Page, Msg )
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

import Animation exposing (px)

--ADD COMPONENT-- import
import Page.Component01.Home as Comp01
import Page.Component02.Home as Comp02
import Page.Component03.Home as Comp03
import Page.Component04.Home as Comp04

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
    = Redirect Session Navbar
    | NotFound Session Navbar
--    | Page Page.Model
    | Home Home.Model Navbar
    | Settings Settings.Model Navbar 
    | Login Login.Model Navbar
    | Register Register.Model Navbar
    | Profile Username Profile.Model Navbar 
    | Article Article.Model Navbar
    | Editor (Maybe Slug) Editor.Model Navbar
--ADD COMPONENT-- Type Model
    | Comp01 Comp01.Model Navbar
    | Comp02 Comp02.Model Navbar
    | Comp03 Comp03.Model Navbar
    | Comp04 Comp04.Model Navbar



type alias SaveModel  
      =  { 
            comp01: Comp01.Model
           ,comp02: Comp02.Model
           ,comp03: Comp03.Model
           ,comp04: Comp04.Model
         }

-- MODEL


init : Maybe Viewer ->  Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer  url navKey =
    let
       navbar = { docmenu_open = False
                , appmenu_open = False
                      -- ,save_model = Dict.empty 
                }
    in 
    changeRouteTo (Route.fromUrl url)
        (Redirect  (Session.fromViewer navKey maybeViewer) navbar)



-- VIEW


view : Model ->  Document Msg
view model  =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage navbar_ page toMsg config =
            let
                { title, body } =
                    Page.view viewer page navbar_ config
            in
            { title = title
            , body = [stylesheet] ++ List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _  navbar ->
            Page.view viewer Page.Other navbar Blank.view

        NotFound _  navbar ->
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

--ADD COMPONENT-- view
        Comp01 comp01 navbar ->
            viewPage navbar Page.Other GotComp01Msg (Comp01.view comp01)
        Comp02 comp02 navbar ->
            viewPage navbar Page.Other GotComp02Msg (Comp02.view comp02)
        Comp03 comp03 navbar ->
            viewPage navbar Page.Other GotComp03Msg (Comp03.view comp03)
        Comp04 comp04 navbar ->
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
        Redirect session _ ->
            session

        NotFound session _ ->
            session

        Home home _ ->
            Home.toSession home 

        Settings settings _ ->
            Settings.toSession settings 

        Login login _ ->
            Login.toSession login 

        Register register _ ->
            Register.toSession register 

        Profile _ profile _ ->
            Profile.toSession profile

        Article article _ ->
            Article.toSession article

        Editor _ editor _ ->
            Editor.toSession editor

--ADD COMPONENT-- toSession
        Comp01 comp01 _ ->
            Comp01.toSession comp01
        Comp02 comp02 _ ->
            Comp02.toSession comp02
        Comp03 comp03 _ ->
            Comp03.toSession comp03
        Comp04 comp04 _ ->
            Comp04.toSession comp04

toNavbar : Model -> Page.Navbar
toNavbar page =
    case page of
        Redirect session navbar ->
            navbar

        NotFound session navbar ->
            navbar

        Home home _ ->
            Home.toNavbar home 

        Settings settings _ ->
            Settings.toNavbar settings 

        Login login _ ->
            Login.toNavbar login 

        Register register _ ->
            Register.toNavbar register 

        Profile _ profile _ ->
            Profile.toNavbar profile

        Article article _ ->
            Article.toNavbar article

        Editor _ editor _ ->
            Editor.toNavbar editor

--ADD COMPONENT-- toNavbar
        Comp01 comp01 _ ->
            Comp01.toNavbar comp01
        Comp02 comp02 _ ->
            Comp02.toNavbar comp02
        Comp03 comp03 _ ->
            Comp03.toNavbar comp03
        Comp04 comp04 _ ->
            Comp04.toNavbar comp04

updateNavbar : Model -> Navbar -> Model
updateNavbar page new_navbar =
    case page of
        Redirect session navbar ->
            ( Redirect session new_navbar )

        NotFound session navbar ->
            ( NotFound session new_navbar )

        Home home _ ->
            -- ( Home home new_navbar )
            let
               new_home = Home.setNavbar home new_navbar
            in
            ( Home new_home new_navbar )

        Settings settings _ ->
            -- ( Settings settings new_navbar )
            let
               new_settings = Settings.setNavbar settings new_navbar
            in
            ( Settings new_settings new_navbar )

        Login login _ ->
            -- ( Login login new_navbar )
            let
               new_login = Login.setNavbar login new_navbar
            in
            ( Login new_login new_navbar )

        Register register _ ->
            -- ( Register register new_navbar )
            let
               new_register = Register.setNavbar register new_navbar
            in
            ( Register new_register new_navbar )

        Profile a profile _ ->
            -- ( Profile a profile new_navbar )
            let
               new_profile = Profile.setNavbar profile new_navbar
            in
            ( Profile a new_profile new_navbar )

        Article article _ ->
            -- ( Article article new_navbar )
            let
               new_article = Article.setNavbar article new_navbar
            in
            ( Article new_article new_navbar )

        Editor a editor _ ->
            -- ( Editor a editor new_navbar )
            let
               new_editor = Editor.setNavbar editor new_navbar
            in
            ( Editor a new_editor new_navbar )

--ADD COMPONENT-- updateNavbar
        Comp01 comp01 _ ->
            let
               new_comp01 = Comp01.setNavbar comp01 new_navbar
            in
            ( Comp01 new_comp01 new_navbar )
        Comp02 comp02 _ ->
            let
               new_comp02 = Comp02.setNavbar comp02 new_navbar
            in
            ( Comp02 new_comp02 new_navbar )
        Comp03 comp03 _ ->
            let
               new_comp03 = Comp03.setNavbar comp03 new_navbar
            in
            ( Comp03 new_comp03 new_navbar )
        Comp04 comp04 _ ->
            let
               new_comp04 = Comp04.setNavbar comp04 new_navbar
            in
            ( Comp04 new_comp04 new_navbar )

changeRouteTo : Maybe Route  ->Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute  model =
    let
        -- _ = Debug.log "** changeRouteTo"
        session =
            toSession model
        navbar =
            toNavbar model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session navbar, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.NewArticle ->
            Editor.initNew session navbar
                |> updateWith (Editor Nothing) navbar GotEditorMsg model

        Just (Route.EditArticle slug) ->
            Editor.initEdit session slug navbar
                |> updateWith (Editor (Just slug)) navbar GotEditorMsg model

        Just Route.Settings ->
            Settings.init session navbar
                |> updateWith Settings navbar GotSettingsMsg model

        Just Route.Home ->
            Home.init session navbar
                |> updateWith Home navbar GotHomeMsg model

        Just Route.Login ->
            Login.init session navbar
                |> updateWith Login navbar GotLoginMsg model

        Just Route.Register ->
            Register.init session navbar
                |> updateWith Register navbar GotRegisterMsg model

        Just (Route.Profile username ) ->
            Profile.init session username navbar
                |> updateWith  (Profile username) navbar GotProfileMsg model

        Just (Route.Article slug) ->
            Article.init session slug navbar
                |> updateWith Article navbar GotArticleMsg model

--ADD COMPONENT-- changeRouteTo
        Just Route.Comp01 ->
            Comp01.init session navbar
                |> updateWith Comp01 navbar GotComp01Msg model
        Just Route.Comp02 ->
            Comp02.init session navbar
                |> updateWith Comp02 navbar GotComp02Msg model
        Just Route.Comp03 ->
            Comp03.init session navbar
                |> updateWith Comp03 navbar GotComp03Msg model
        Just Route.Comp04 ->
            Comp04.init session navbar
                |> updateWith Comp04 navbar GotComp04Msg model

saveModel : Model -> (Model, Cmd Msg)
saveModel model =
    let
      _  = case model of
             Comp03  comp03 navbar ->
               Debug.log "** Comp03:"  comp03.counter
               --Dict.insert "Comp03" comp03 navbar.save_model
             Comp04  comp04 navbar ->
               Debug.log "** Comp04:"  comp04.counter
             _ ->
               Debug.log "** Other"   0

    in
    (model, Cmd.none)

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model  =
    --let    _ = Debug.log "** Main update msg:" msg  in
    --let    _ = Debug.log "** Main update model:" model  in
    case ( msg, model ) of
        ( MenuOpen menuid, _ ) ->
            -- let _ = Debug.log "-- Main update msg:  MenuOpen" menuid in
            -- let _ = Debug.log "MenuOpen DocMenu" menuid in
            let
                navbar = toNavbar( model)
                model_ =  updateNavbar model (toggleMenu menuid navbar)
            in
            ( model_, Cmd.none )
{-
            case menuid of
               DocMenu ->
                  let _ = Debug.log "MenuOpen DocMenu" menuid in
                  let
                      navbar = toNavbar( model)
                      model_ =  updateNavbar model (toggleMenu menuid navbar)
                  in
                  ( model_, Cmd.none )
               AppMenu ->
                  let _ = Debug.log "MenuOpen AppMenu" menuid in
                  let
                      navbar = toNavbar( model)
                      model_ =  updateNavbar model (toggleMenu menuid navbar)
                  in
                  ( model_, Cmd.none )
-}

        ( ClickedLink urlRequest, _ ) ->
            -- let _ = Debug.log "-- Main update msg:  ClickedLink urlRequest" msg in

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

                            -- let    _ = Debug.log "**  Browser.Internal Nothing:" msg  in

                            if  url.path == "/navbar/docmenu_open" then 
                                -- let _ = Debug.log "**  doc:" url.path in
                                update  (MenuOpen DocMenu)  model
                                -- ( model, Cmd.none )

                            else if  url.path == "/navbar/appmenu_open" then 
                                -- let _ = Debug.log "**  app:"  url.path in
                                update  (MenuOpen AppMenu)  model
                                -- ( model, Cmd.none )

                            else
                                -- let _ = Debug.log "**  ---:" url.path in
                                ( model, Cmd.none )

                            -- ( model, Cmd.none )

                        Just _ ->
                           -- let _ = Debug.log "*** internal click:" url.path in
                           let _ = Debug.log "*** internal click:"  (Url.toString url) in
                           let  _ = Debug.log "** Main update model:" model  in
                           let _ = saveModel model in
                           {-
                           let _ =  case model of
                                  Comp03  comp03 navbar ->
                                          Debug.log "** Comp03:"  comp03.counter 
                                  Comp04  comp04 navbar ->
                                          Debug.log "** Comp04:"  comp04.counter 
                                  _  ->
                                          Debug.log "** Other"   0
                           in
                           -}
  
  
                           let
                            (model_, cmd_) = update  (MenuOpen AllOff)  model
                           in
                            ( model_
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                -- Browser.External href ->
                Browser.External href ->
                    -- ( model
                    -- , Nav.load href
                    -- )
                    -- ( model ,  Cmd.none )
                    update  (MenuOpen AllOff)  model

{-
                    let    _ = Debug.log "**  Browser.External href:" href  in
                    let
                      navbar2 = toNavbar( model)
                      _ = Debug.log "**  navbar :" navbar2

                      navbar3 = {
                            docmenu_open = not navbar2.docmenu_open
                           ,appmenu_open = not navbar2.appmenu_open 
                           }
                      model_ =  updateNavbar model navbar3
                    in
                    ( model_ ,  Cmd.none )
-}

--        ( ClickedLink , _ ) ->
--                    let    _ = Debug.log "** Main update msg:  ClickedLink External"   in
--                    ( model, Cmd.none )
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model
            --let
            --  (model_, cmd_) = update  (MenuOpen AllOff)  model
            --in
            --changeRouteTo (Route.fromUrl url) model_
       

        ( GotSettingsMsg subMsg, Settings settings navbar ) ->
            Settings.update subMsg settings
                |> updateWith Settings navbar GotSettingsMsg model

        ( GotLoginMsg subMsg, Login login navbar ) ->
            Login.update subMsg login
                |> updateWith Login navbar  GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register navbar ) ->
            Register.update subMsg register
                |> updateWith Register navbar  GotRegisterMsg model

        ( GotHomeMsg subMsg, Home home navbar ) ->
            Home.update subMsg home
                |> updateWith Home navbar GotHomeMsg model

        ( GotProfileMsg subMsg, Profile username profile navbar ) ->
            Profile.update subMsg profile
                |> updateWith (Profile username ) navbar GotProfileMsg model 

        ( GotArticleMsg subMsg, Article article navbar) ->
            Article.update subMsg article
                |> updateWith Article  navbar GotArticleMsg model 

        ( GotEditorMsg subMsg, Editor slug editor navbar ) ->
            Editor.update subMsg editor
                |> updateWith (Editor slug ) navbar GotEditorMsg model  

        ( GotSession session, Redirect _ navbar ) ->
            ( Redirect session navbar
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

--ADD COMPONENT-- update
        ( GotComp01Msg subMsg, Comp01 comp01 navbar ) ->
            Comp01.update subMsg comp01
                |> updateWith Comp01 navbar GotComp01Msg model

        ( GotComp02Msg subMsg, Comp02 comp02 navbar ) ->
            Comp02.update subMsg comp02
                |> updateWith Comp02 navbar GotComp02Msg model

        ( GotComp03Msg subMsg, Comp03 comp03 navbar ) ->
            Comp03.update subMsg comp03
                |> updateWith Comp03 navbar GotComp03Msg model

        ( GotComp04Msg subMsg, Comp04 comp04 navbar ) ->
            Comp04.update subMsg comp04
                |> updateWith Comp04 navbar GotComp04Msg model

        ( _, _ ) ->
            let    _ = Debug.log "** Main update msg: default"   in
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


toggleMenu: MenuId -> Navbar -> Navbar
toggleMenu menuid navbar  =
        case menuid of
           DocMenu ->
              -- let _ = Debug.log "MenuOpen DocMenu" menuid in
              let
               new_navbar = {
                           docmenu_open = not navbar.docmenu_open
                          -- ,appmenu_open = navbar.appmenu_open 
                          ,appmenu_open = False
                         -- ,save_model = Dict.empty 
                          }
              in
              new_navbar

           AppMenu ->
              -- let _ = Debug.log "MenuOpen AppMenu" menuid in
              let
               new_navbar = {
                        -- docmenu_open = navbar.docmenu_open
                        docmenu_open = False
                       ,appmenu_open = not navbar.appmenu_open 
                      -- ,save_model = Dict.empty 
                       }
              in
              new_navbar

           AllOff ->
              -- let _ = Debug.log "MenuOpen AllOff" menuid in
              let
               new_navbar = {
                        docmenu_open = False
                       ,appmenu_open = False
                      -- ,save_model = Dict.empty 
                       }
              in
              new_navbar

updateWith : (subModel -> Navbar -> Model )  -> Navbar ->  (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel navbar  toMsg model ( subModel, subCmd ) =
    ( toModel subModel  navbar
    , Cmd.map toMsg subCmd
    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _  navbar ->
            Sub.none

        Redirect _ navbar ->
            Session.changes GotSession (Session.navKey (toSession model))

        Settings settings navbar ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home navbar  ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login navbar  ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register navbar  ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Profile _ profile navbar ->
            Sub.map GotProfileMsg (Profile.subscriptions profile)

        Article article  navbar ->
            Sub.map GotArticleMsg (Article.subscriptions article)

        Editor _ editor  navbar ->
            Sub.map GotEditorMsg (Editor.subscriptions editor)

--ADD COMPONENT-- subscriptions
        Comp01 comp01 navbar  ->
            Sub.map GotComp01Msg (Comp01.subscriptions comp01)
        Comp02 comp02 navbar  ->
            Sub.map GotComp02Msg (Comp02.subscriptions comp02)
        Comp03 comp03 navbar  ->
            Sub.map GotComp03Msg (Comp03.subscriptions comp03)
        Comp04 comp04 navbar  ->
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
