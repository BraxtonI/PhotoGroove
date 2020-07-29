port module PhotoGallery exposing (init, Model, Msg, subscriptions, update, view, reload)

import Browser
import Common               exposing (urlPrefix, Photo)
import Element              exposing (..)
import Element.Background   as Background
import Element.Border       as Border
import Element.Events       as Events
import Element.Font         as Font
import Element.Input        as Input
import Element.Lazy         as Lazy
import Element.Region       as Region
import Html                 exposing (Html, div, canvas, label, input, node)
import Html.Attributes      as Attr exposing (checked, class, id, name, type_)
import Html.Events          exposing (on, onClick)
import Http
import Json.Decode          exposing (Decoder, at, string, int, list, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode          as Encode
import Random
import UI



main : Program Float Model Msg
main =
    Browser.element
        { init          = init
        , view          = (\_ -> div [] [])
        , update        = update
        , subscriptions = subscriptions
        }



subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity



init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
        ( { initialModel | activity = activity }, initialCmd )


type alias Model =
    { status     : Status
    , activity   : String
    , chosenSize : ThumbnailSize
    , hue        : Int
    , ripple     : Int
    , noise      : Int
    }


initialModel : Model
initialModel =
    { status     = Loading
    , activity   = ""
    , chosenSize = Medium
    , hue        = 5
    , ripple     = 5
    , noise      = 5
    }



type Msg
    = ClickedPhoto   String
    | ClickedSize    ThumbnailSize
    | ClickedSurpriseMe
    | GotActivity    String
    | GotPhotos      (Result Http.Error (List Photo))
    | GotRandomPhoto Photo
    | SlidHue        Int
    | SlidNoise      Int
    | SlidRipple     Int



view : Model -> Element Msg
view model =
    row UI.photoGalleryContent <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> Model -> List (Element Msg)
viewLoaded photos selectedUrl model =
    [ column
        []
        [ row -- Activity top right
            UI.activity
            [ text model.activity ]
        , row -- First row (Size, Sliders, Button)
            UI.galleryOptions
            [ column
                UI.thumbnailLabel
                [ text "Thumbnail Size:" ]
            , row
                UI.chosenSize
                (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
            , column
                UI.filters
                [ viewFilter SlidHue    "Hue"    model.hue
                , viewFilter SlidRipple "Ripple" model.ripple
                , viewFilter SlidNoise  "Noise"  model.noise
                ]
            , Input.button
                UI.button
                { onPress = Just ClickedSurpriseMe
                , label   = (text "Surprise Me!")
                }
            ]
        , row -- Main content (Thumbnails and Canvas)
            []
            [ wrappedRow
                UI.thumbnails
                (List.map (viewThumbnail selectedUrl (sizeToString model.chosenSize)) photos)
            , Element.html
                (
                    canvas
                    [ id "main-canvas", class "large" ]
                    []
                )
            ]
        ]
    ]


viewThumbnail : String -> String -> Photo -> Element Msg
viewThumbnail selectedUrl size thumb =
    image
        (
            (UI.thumbnail size (selectedUrl == thumb.url))
            ++ [ Events.onClick (ClickedPhoto thumb.url) ]
        )
        { src = (urlPrefix ++ thumb.url)
        , description = (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        }


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Element Msg
viewSizeChooser chosenSize size =
    {--Input.radio
        []
        { onChange =
        ,
        }--}
    Element.html (label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ,
            if size == chosenSize then
                checked True
            else
                checked False
        ] []
        , Html.text (sizeToString size)
        ])


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


type ThumbnailSize
    = Small
    | Medium
    | Large


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url     : String
    , filters : List { name : String, amount : Float }
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> optional "title"       string        "(untitled)"
        |> required "url"         string
        |> required "size"        int
        |> optional "requiredUrl" (list string) []


type Status
    = Loading
    | Loaded  (List Photo) String
    | Errored String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        GotPhotos (Ok photos) ->
            applyFilters
                { model
                    | status =
                        case photos of
                            first :: rest ->
                                Loaded photos first.url

                            [] ->
                                Loaded [] ""
                }

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue",    amount = toFloat model.hue    / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise",  amount = toFloat model.noise  / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


reload : Model -> (Model, Cmd Msg)
reload newModel =
    applyFilters newModel


viewFilter : (Int -> Msg) -> String -> Int -> Element Msg
viewFilter toMsg name magnitude =
    Element.html (div [ class "filter-slider" ]
        [ label [] [ Html.text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ Html.text (String.fromInt magnitude) ]
        ])


rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Html.Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"