module Page.Component05.Home exposing (Model, Msg, init, subscriptions, toSession, toNavbar, setNavbar, toSaveModel, toInitModel,  update, view)


import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Preview)
import Article.Feed as Feed
import Article.Tag as ArticleTag exposing (Tag)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, style)
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


import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
-- import Bulma.Columns as Columns exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)

import Lib.Table exposing (..)

-- MODEL


type alias Model =
    { session : Session
    , navbar  : Page.Navbar
    , savemodel : Save.SaveModel
    , timeZone : Time.Zone
    , counter  : Int
    , people : List Person
    , tableState : Lib.Table.State
    , query : String
    }


init : Session -> Page.Navbar -> Save.SaveModel -> ( Model, Cmd Msg )
init session navbar savemodel =
-- init : Session -> Page.Navbar -> Save.SaveModel -> List Person -> ( Model, Cmd Msg )
-- init session navbar savemodel people =
    let _ = Debug.log "-- Comp05 Home savemodel:" savemodel in
    let
       init_counter = if savemodel.comp05.save then savemodel.comp05.model.counter else 0
    in
    ( { session = session
      , navbar = navbar
      , savemodel = savemodel
      , timeZone = Time.utc
      , counter = init_counter
      , people = presidents
      , tableState = Lib.Table.initialSort "Year"
      , query = ""
      }
     , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
  let
    lowerQuery =
      String.toLower model.query

    acceptablePeople =
      List.filter (String.contains lowerQuery << String.toLower << .name) model.people
  in

    { title = "Comp05"
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
                  [ h1 [] [ text "Birthplaces of U.S. Presidents" ]
                  , input [ placeholder "Search by Name", onInput SetQuery ] []
                  , Lib.Table.view config model.tableState acceptablePeople
                  ]
            ]
              
    }



-- UPDATE


type Msg 
    = GotSession Session
    | GotTimeZone Time.Zone
    | Increment 
    | Decrement
      -- Table
    | SetQuery String
    | SetTableState Lib.Table.State


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

        SetQuery newQuery ->
          ( { model | query = newQuery }
          , Cmd.none
          )

        SetTableState newState ->
          ( { model | tableState = newState }
          , Cmd.none
          )



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


toSaveModel : Model -> Save.Comp05_Model
toSaveModel model =
             { counter = model.counter }


toInitModel :  Save.Comp05_Model
toInitModel  =
             { counter = 0 }


------------------------------------------ Table Setting
-- CONFIG

config : Lib.Table.Config Person Msg
config =
  Lib.Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Lib.Table.stringColumn "Name" .name
        , Lib.Table.intColumn "Year" .year
        , Lib.Table.stringColumn "City" .city
        , Lib.Table.stringColumn "State" .state
        ]
    }

-- PEOPLE


type alias Person =
  { name : String
  , year : Int
  , city : String
  , state : String
  }


presidents : List Person
presidents =
  [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
  , Person "John Adams" 1735 "Braintree" "Massachusetts"
  , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
  , Person "James Madison" 1751 "Port Conway" "Virginia"
  , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
  , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
  , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
  , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
  , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
  , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
  , Person "John Tyler" 1790 "Charles City County" "Virginia"
  , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
  , Person "James K. Polk" 1795 "Pineville" "North Carolina"
  , Person "Millard Fillmore" 1800 "Summerhill" "New York"
  , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
  , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
  , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
  , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
  , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
  , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
  , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
  , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
  , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
  , Person "William McKinley" 1843 "Niles" "Ohio"
  , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
  , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
  , Person "Theodore Roosevelt" 1858 "New York City" "New York"
  , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
  , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
  , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
  , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
  , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
  , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
  , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
  , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
  , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
  , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
  , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
  , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
  , Person "Jimmy Carter" 1924 "Plains" "Georgia"
  , Person "George W. Bush" 1946 "New Haven" "Connecticut"
  , Person "Bill Clinton" 1946 "Hope" "Arkansas"
  , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
  ]

