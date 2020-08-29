module Page.Component03.Home exposing (Model, Msg, init, subscriptions, toSession, toNavbar, setNavbar, update, view)


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
    , counter  : Int
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed



init : Session -> Page.Navbar -> ( Model, Cmd Msg )
init session navbar =
--    let
--        feedTab =
--            case Session.cred session of
--                Just cred ->
--                    YourFeed cred
--
--                Nothing ->
--                    GlobalFeed
--
--        loadTags =
--            Http.toTask ArticleTag.list
--    in
    ( { session = session
      , navbar = navbar
      , timeZone = Time.utc
      , counter = 0
      }
     , Cmd.none
--    , Cmd.batch
--        [ fetchFeed session feedTab 1
--            |> Task.attempt CompletedFeedLoad
--        , ArticleTag.list
--            |> Http.send CompletedTagsLoad
--        , Task.perform GotTimeZone Time.here
--        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
--        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Comp03"
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
                   [ Html.button [ onClick Decrement ] [ text "-" ]
                    , div [] [ text (String.fromInt model.counter) ]
                    , Html.button [ onClick Increment ] [ text "+" ]
                   ]
            ]
    }



-- UPDATE


type Msg 
    = GotSession Session
    | GotTimeZone Time.Zone
    | Increment 
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )





-- HTTP

{-
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

-}

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

