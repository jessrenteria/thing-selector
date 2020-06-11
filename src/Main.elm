module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Random
import Random.List
import Url
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , thing : Maybe String
    , content : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case Parser.parse urlParser url of
        Nothing ->
            ( Model key url Nothing "", Cmd.none )

        Just things ->
            ( Model key url Nothing (String.join "\n" things), Cmd.none )


urlParser : Parser.Parser (List String -> a) a
urlParser =
    Parser.oneOf
        -- GitHub Pages
        [ Parser.s "thing-selector"

        -- Local deployment
        , Parser.s "src" </> Parser.s "Main.elm"
        ]
        <?> thingParser


thingParser : Query.Parser (List String)
thingParser =
    Query.custom "thing" identity



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectThing
    | NewThing (Maybe String)
    | ChangeContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        SelectThing ->
            ( model
            , Random.generate NewThing <|
                Random.map Tuple.first (Random.List.choose <| processThings model.content)
            )

        NewThing thing ->
            ( { model | thing = thing }
            , Cmd.none
            )

        ChangeContent content ->
            ( { model | content = content }
            , Cmd.none
            )


processThings : String -> List String
processThings content =
    content |> String.lines |> List.filter (not << String.isEmpty)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


black : Color
black =
    rgb255 10 10 10


white : Color
white =
    rgb255 225 225 225


blue : Color
blue =
    rgb255 21 101 192


purple : Color
purple =
    rgb255 69 39 160


view : Model -> Browser.Document Msg
view model =
    { title = "Thing Selector"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            , padding 10
            , spacing 7
            ]
          <|
            column [ width fill, height fill ]
                [ row [ width fill, height <| fillPortion 1 ]
                    [ el [ width fill ] <| text "Things"
                    , el [ width fill ] <| text "Selection"
                    ]
                , row [ width fill, height <| fillPortion 3 ]
                    [ Input.multiline [ width fill, Font.color black ]
                        { onChange = ChangeContent
                        , text = model.content
                        , placeholder = Just <| Input.placeholder [] <| text "Things to select, one thing per line."
                        , label = Input.labelHidden "Things"
                        , spellcheck = False
                        }
                    , el [ width fill, Font.italic ] <| text <| Maybe.withDefault "" model.thing
                    ]
                , row [ width fill, height <| fillPortion 1 ]
                    [ Input.button
                        [ width fill
                        , height fill
                        , Background.color blue
                        , mouseOver [ Background.color purple ]
                        ]
                        { onPress = Just SelectThing
                        , label = text "Select"
                        }
                    ]
                ]
        ]
    }
