module Page.Component02.Home exposing (Model, Msg, init, subscriptions, toSession, toNavbar, setNavbar, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.Tag as ArticleTag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, style)
import Html.Events exposing (onClick)
import Http
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)


import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
-- import Bulma.Columns as Columns exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)


-- MODEL


type alias Model =
    { session : Session
    , navbar  : Page.Navbar
    , timeZone : Time.Zone
    , feedTab : FeedTab
    , feedPage : Int

    -- Loaded independently from server
    , tags : Status (List ArticleTag.Tag)
    , feed : Status Feed.Model
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type FeedTab
    = YourFeed Cred
    | GlobalFeed
    | TagFeed ArticleTag.Tag


init : Session -> Page.Navbar -> ( Model, Cmd Msg )
init session navbar =
    let
        feedTab =
            case Session.cred session of
                Just cred ->
                    YourFeed cred

                Nothing ->
                    GlobalFeed

        loadTags =
            Http.toTask ArticleTag.list
    in
    ( { session = session
      , navbar = navbar
      , timeZone = Time.utc
      , feedTab = feedTab
      , feedPage = 1
      , tags = Loading
      , feed = Loading
      }
    , Cmd.batch
        [ fetchFeed session feedTab 1
            |> Task.attempt CompletedFeedLoad
        , ArticleTag.list
            |> Http.send CompletedTagsLoad
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Comp02"
    , content =
        div [ class "home-page " 
             ,marginless
             ,paddingless
            ]
            [ div [
                     style "position" "fixed"  --screen bottom position fixed //GUSA
                    , style "top" "52px"
                    , style "z-index" "100"
                    , style "background-color" "#FFFFFF"
                    ]
                   [viewBanner
                    , viewTabs
                          (Session.cred model.session)
                          model.feedTab
                    ]
            
            , div [ class "container-page"
                   ,style "margin-top" "160px"
                    ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        case model.feed of
                            Loaded feed ->
                                [ div [ class "feed-toggle" ] <|
                                    List.concat
                                        [ 
                                       --  [div [ class "viewtabs"
                                       --         , style "position" "fixed"  --screen bottom position fixed //GUSA
                                       --         , style "top" "100px"
                                       --         , style "z-index" "1000"
                                       --         ]      
                                       --      [ viewTabs
                                       --            (Session.cred model.session)
                                       --            model.feedTab
                                       --      ]
                                       --   ]
                                       -- , 
                                        Feed.viewArticles model.timeZone feed
                                            |> List.map (Html.map GotFeedMsg)
                             --           , [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
                                          , [div [ class "pagination"
                                                , style "position" "fixed"  --screen bottom position fixed //GUSA
                                                , style "bottom" "0"
                                                , style "background-color" "#FFFFFF"
                                                ]      
                                                [ Feed.viewPagination ClickedFeedPage model.feedPage feed ]
                                             ]
                                        ]
                                ]

                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "feed" ]
{-
                    , div [ class "col-md-3" ] <|
                        case model.tags of
                            Loaded tags ->
                                let _ = Debug.log "tags!!" model.tags in
                                [ div [ class "sidebar" ] <|
                                    [ p [] [ text "Popular Tags" ]
                                    , viewTags tags
                                    ]
                                ]
-}
                    , div [] <|
                        case model.tags of
                            Loaded tags ->
                                let _ = Debug.log "tags!!" model.tags in
                                [ card [
                                                  style "position" "fixed"  --screen bottom position fixed //GUSA
                                                , style "top" "200px"
                                                , style "right" "200px"
                                                , style "z-index" "100"
                                                , style "width" "200px"
                                ] <|
                                    [ cardContent []
                                        [ p [] [ text "Popular Tags" ]
                                        , viewTags tags
                                        ]
                                    ]
                                ]
                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Failed ->
                                [ Loading.error "tags" ]
                    ]
                ]
            ]
    }

{-
viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]
-}

viewBanner : Html msg
viewBanner =
   panel [] 
    [ panelHeading [] [ text "Component02" ] 
    , panelBlock False []
      [ controlInput controlInputModifiers [] [] []
      ] 
    {-
    , panelTabs []
      [ panelTab False [] [ text "all"     ]
      , panelTab True  [] [ text "public"  ]
      , panelTab True  [] [ text "private" ]
      ]
    , panelLink False [] [ text "bulma"     ]
    , panelLink False [] [ text "marksheet" ]
    , panelLink True  [] [ text "test"      ]
    , panelLink False [] [ text "horsin"    ]
    -}
    ]

-- TABS


viewTabs : Maybe Cred -> FeedTab -> Html Msg
viewTabs maybeCred tab =
    case tab of
        YourFeed cred ->
            Feed.viewTabs [] (yourFeed cred) [ globalFeed ]

        GlobalFeed ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred ]

                        Nothing ->
                            []
            in
            Feed.viewTabs otherTabs globalFeed []

        TagFeed tag ->
            let
                otherTabs =
                    case maybeCred of
                        Just cred ->
                            [ yourFeed cred, globalFeed ]

                        Nothing ->
                            [ globalFeed ]
            in
            Feed.viewTabs otherTabs (tagFeed tag) []


yourFeed : Cred -> ( String, Msg )
yourFeed cred =
    ( "Your Feed", ClickedTab (YourFeed cred) )


globalFeed : ( String, Msg )
globalFeed =
    ( "Global Feed", ClickedTab GlobalFeed )


tagFeed : ArticleTag.Tag -> ( String, Msg )
tagFeed tag =
    ( "#" ++ ArticleTag.toString tag, ClickedTab (TagFeed tag) )



-- TAGS


viewTags : List ArticleTag.Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : ArticleTag.Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag"
        -- , style "padding" "6px"
        , style "margin" "3px"
        , onClick (ClickedTag tagName)

        -- The RealWorld CSS requires an href to work properly.
        , href ""
        ]
        [ text (ArticleTag.toString tagName) ]


{-
viewTags : List ArticleTag.Tag -> Html Msg
viewTags tags =
    -- div [ class "tag-list" ] (List.map viewTag tags)
      tabs { tabsModifiers | style = Boxed } [] [] (List.map viewTag tags) 


viewTag : ArticleTag.Tag -> Html Msg
viewTag tagName =
    -- a
    -- tab True  [] [] [ icon Standard [] [ i [ class "fa fa-image"      ] [] ], text (ArticleTag.toString tagName)  ]
    tab True  [] [] [ icon Standard [] [ i [ class "fa fa-image"      ] [] ], text "TEST" ]
    {-
    tab
        [ class "tag-pill tag-default"
        , onClick (ClickedTag tagName)

        -- The RealWorld CSS requires an href to work properly.
        , href ""
        ]
        [ text (ArticleTag.toString tagName) ]
     -}

-}

-- UPDATE


type Msg
    = ClickedTag ArticleTag.Tag
    | ClickedTab FeedTab
    | ClickedFeedPage Int
    | CompletedFeedLoad (Result Http.Error Feed.Model)
    | CompletedTagsLoad (Result Http.Error (List ArticleTag.Tag))
    | GotTimeZone Time.Zone
    | GotFeedMsg Feed.Msg
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedTag tag ->
            let
                feedTab =
                    TagFeed tag
            in
            ( { model | feedTab = feedTab }
            , fetchFeed model.session feedTab 1
                |> Task.attempt CompletedFeedLoad
            )

        ClickedTab tab ->
            ( { model | feedTab = tab }
            , fetchFeed model.session tab 1
                |> Task.attempt CompletedFeedLoad
            )

        ClickedFeedPage page ->
            ( { model | feedPage = page }
            , fetchFeed model.session model.feedTab page
                |> Task.andThen (\feed -> Task.map (\_ -> feed) scrollToTop)
                |> Task.attempt CompletedFeedLoad
            )

        CompletedFeedLoad (Ok feed) ->
            ( { model | feed = Loaded feed }, Cmd.none )

        CompletedFeedLoad (Err error) ->
            ( { model | feed = Failed }, Cmd.none )

        CompletedTagsLoad (Ok tags) ->
            ( { model | tags = Loaded tags }, Cmd.none )

        CompletedTagsLoad (Err error) ->
            ( { model | tags = Failed }
            , Log.error
            )

        GotFeedMsg subMsg ->
            case model.feed of
                Loaded feed ->
                    let
                        ( newFeed, subCmd ) =
                            Feed.update (Session.cred model.session) subMsg feed
                    in
                    ( { model | feed = Loaded newFeed }
                    , Cmd.map GotFeedMsg subCmd
                    )

                Loading ->
                    ( model, Log.error )

                LoadingSlowly ->
                    ( model, Log.error )

                Failed ->
                    ( model, Log.error )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                feed =
                    case model.feed of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                tags =
                    case model.tags of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | feed = feed, tags = tags }, Cmd.none )



-- HTTP


fetchFeed : Session -> FeedTab -> Int -> Task Http.Error Feed.Model
fetchFeed session feedTabs page =
    let
        maybeCred =
            Session.cred session

        decoder =
            Feed.decoder maybeCred articlesPerPage

        params =
            PaginatedList.params { page = page, resultsPerPage = articlesPerPage }

        request =
            case feedTabs of
                YourFeed cred ->
                    Api.get (Endpoint.feed params) maybeCred decoder

                GlobalFeed ->
                    Api.get (Endpoint.articles params) maybeCred decoder

                TagFeed tag ->
                    let
                        firstParam =
                            Url.Builder.string "tag" (ArticleTag.toString tag)
                    in
                    Api.get (Endpoint.articles (firstParam :: params)) maybeCred decoder
    in
    Http.toTask request
        |> Task.map (Feed.init session)


articlesPerPage : Int
articlesPerPage =
    10


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0
        -- It's not worth showing the user anything special if scrolling fails.
        -- If anything, we'd log this to an error recording service.
        |> Task.onError (\_ -> Task.succeed ())



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

toNavbar : Model -> Page.Navbar
toNavbar model =
    model.navbar

setNavbar : Model -> Page.Navbar -> Model
setNavbar model navbar_ =
     { model | navbar = navbar_ } 

