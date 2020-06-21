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
    , thingType : String
    , selection : Maybe String
    , content : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case Parser.parse urlParser url of
        Nothing ->
            ( { key = key
              , url = url
              , thingType = "Thing"
              , selection = Nothing
              , content = ""
              }
            , Cmd.none
            )

        Just urlParams ->
            ( { key = key
              , url = url
              , thingType = Maybe.withDefault "Thing" urlParams.thingType
              , selection = Nothing
              , content = String.join "\n" urlParams.things
              }
            , Cmd.none
            )


type alias UrlParams =
    { thingType : Maybe String
    , things : List String
    }


urlParser : Parser.Parser (UrlParams -> a) a
urlParser =
    Parser.map UrlParams <|
        Parser.oneOf
            -- GitHub Pages
            [ Parser.s "thing-selector"

            -- Local deployment
            , Parser.s "src" </> Parser.s "Main.elm"
            ]
            <?> Query.string "thingType"
            <?> Query.custom "thing" identity



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectThing
    | Shuffle
    | NewSelection (Maybe String)
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
            , processThings model.content
                |> Random.List.choose
                |> Random.map Tuple.first
                |> Random.generate NewSelection
            )

        Shuffle ->
            ( model
            , processThings model.content
                |> Random.List.shuffle
                |> Random.map (Just << String.join "\n")
                |> Random.generate NewSelection
            )

        NewSelection selection ->
            ( { model | selection = selection }
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
    { title = model.thingType ++ " Selector"
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
                    [ el [ width fill ] <| text <| model.thingType ++ " Pool"
                    , el [ width fill ] <| text "Selection"
                    ]
                , row [ width fill, height <| fillPortion 3 ]
                    [ Input.multiline [ width fill, Font.color black ]
                        { onChange = ChangeContent
                        , text = model.content
                        , placeholder =
                            Just <|
                                Input.placeholder [] <|
                                    text "Possibilities, one per line."
                        , label = Input.labelHidden <| model.thingType ++ " Pool"
                        , spellcheck = False
                        }
                    , el [ width fill, Font.italic ] <|
                        text <|
                            Maybe.withDefault "" model.selection
                    ]
                , row [ width fill, height <| fillPortion 1, spacing 10 ]
                    [ Input.button
                        [ width fill
                        , height fill
                        , Background.color blue
                        , mouseOver [ Background.color purple ]
                        ]
                        { onPress = Just SelectThing
                        , label = text "Select"
                        }
                    , Input.button
                        [ width fill
                        , height fill
                        , Background.color blue
                        , mouseOver [ Background.color purple ]
                        ]
                        { onPress = Just Shuffle
                        , label = text "Shuffle"
                        }
                    ]
                ]
        ]
    }
