module Page.Component04.Home exposing (Model, Msg, init, subscriptions, toSession, toNavbar, setNavbar, SaveModel, toSaveModel, toInitModel,  update, view)


import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.Tag as ArticleTag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, style, name, type_, min, max, value)
import Html.Events exposing (onClick, onInput)
import Http
import Loading
import Log
import Page
import Save
import PaginatedList exposing (PaginatedList)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)

import Color exposing (Color)


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
    , savemodel : Save.SaveModel
    , timeZone : Time.Zone
    , counter  : Int
    , color1 : Color.Color
    , color2 : Color.Color
    }

{--
init : Session -> Page.Navbar -> Save.SaveModel -> ( Model, Cmd Msg )
init session navbar savemodel =
    let _ = Debug.log "-- Comp04 Home savemodel:" savemodel in
    let
       init_counter = if savemodel.comp04.save then savemodel.comp04.model.counter else 0
    in
    ( { session = session
      , navbar = navbar
      , savemodel = savemodel
      , timeZone = Time.utc
      , counter = init_counter
      , color1 = Color.rgb 50 200 100
      , color2 = Color.rgb 0 0 0
      }
     , Cmd.none
    )
--}

init : Session -> Page.Navbar -> Save.SaveModel -> ( Model, Cmd Msg )
init session navbar savemodel =
    let _ = Debug.log "-- Comp04 Home savemodel:" savemodel in
    let
       init_counter = if savemodel.comp04.save then savemodel.comp04.model.counter else 0
       init_color1  = if savemodel.comp04.save then savemodel.comp04.model.color1  else Color.rgb 50 200 100
       init_color2  = if savemodel.comp04.save then savemodel.comp04.model.color2  else Color.rgb 0 0 0
    in
    ( { session = session
      , navbar = navbar
      , savemodel = savemodel
      , timeZone = Time.utc
      , counter = init_counter
      -- , color1 = Color.rgb 50 200 100
      -- , color2 = Color.rgb 0 0 0
      , color1 = init_color1
      , color2 = init_color2
      }
     , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Comp04"
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
                    -- , div [] [ text (String.fromInt model.data.counter) ]
                    , Html.button [ onClick Increment ] [ text "+" ]
                   ]
              , div []
                 [ Html.h1 [ style "color" (toColorCss model.color1) ] [ Html.text "Title 1" ]
                 , colorPicker model.color1 UpdateColor1
                 , Html.hr [] []
                 , Html.h1 [ style "color" (toColorCss model.color2) ] [ Html.text "Title 2" ]
                 , colorPicker model.color2 UpdateColor2
                 ]
            ]
    }



-- UPDATE


type Msg 
    = GotSession Session
    | GotTimeZone Time.Zone
    | Increment 
    | Decrement
      -- Color Picker
    | UpdateColor1 Color.Color
    | UpdateColor2 Color.Color


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

        UpdateColor1 newColor1 ->
            ( { model | color1 = newColor1 }, Cmd.none )

        UpdateColor2 newColor2 ->
            ( { model | color2 = newColor2 }, Cmd.none )

-- COLOR

colorSlider : String -> Int -> (Int -> msg) -> Html.Html msg
colorSlider name colorValue toMsg =
    Html.div []
        [ Html.p [] [ Html.text name ]
        , Html.input
            [ type_ "range"
            , Html.Attributes.name ("color" ++ name)
            , Html.Attributes.min "0"
            , Html.Attributes.max "255"
            , value (String.fromInt colorValue)
            , onInput (toMsg << toInt colorValue)
            ]
            []
        , Html.span [] [ Html.text (String.fromInt colorValue) ]
        ]


colorPicker : Color.Color -> (Color.Color -> msg) -> Html.Html msg
colorPicker color toMsg =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Html.div []
        [ colorSlider "Red" red (toMsg << redToColour color)
        , colorSlider "Green" green (toMsg << greenToColour color)
        , colorSlider "Blue" blue (toMsg << blueToColour color)
        , Html.div
            [ style "border" "1px solid black"
            , style "width" "100px"
            , style "height" "100px"
            , style "background-color" (toColorCss color)
            ]
            []
        ]


toInt : Int -> String -> Int
toInt defaultValue strValue =
    strValue
        |> String.toInt
        |> Maybe.withDefault defaultValue


redToColour : Color.Color -> Int -> Color.Color
redToColour color newRed =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb newRed green blue


greenToColour : Color.Color -> Int -> Color.Color
greenToColour color newGreen =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb red newGreen blue


blueToColour : Color.Color -> Int -> Color.Color
blueToColour color newBlue =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb red green newBlue


toColorCss : Color.Color -> String
toColorCss color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    "rgb("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"


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


type alias SaveModel
     =  {
          counter : Int
        , color1  : Color.Color
        , color2  : Color.Color
        }

-- toSaveModel : Model -> Save.Comp04_SaveModel
toSaveModel : Model -> SaveModel
toSaveModel model =
             { counter = model.counter 
             , color1 = model.color1
             , color2 = model.color2
             }


-- toInitModel :  Save.Comp04_SaveModel
toInitModel :  SaveModel
toInitModel  =
             { counter = 0 
             , color1 = Color.rgb 50 200 100
             , color2 = Color.rgb 0 0 0
             }
