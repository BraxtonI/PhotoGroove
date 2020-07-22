module Main               exposing (main)

import Browser            exposing (Document)
import Browser.Navigation as Nav
import Element            exposing (Attribute, column, el, Element, layout, layoutWith, link, none, rgb255, row, text)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Lazy       exposing (lazy)
import Element.Region     as Region
import Html               exposing (Html)
import PhotoFolders       as Folders
import PhotoGallery       as Gallery
import UI
import Url                exposing (Url)
import Url.Parser         as Parser exposing ((</>), Parser, s)



main : Program Float Model Msg
main =
    Browser.application
        { init          = init
        , view          = view
        , update        = update
        , subscriptions = subscriptions
        , onUrlChange   = ChangedUrl
        , onUrlRequest  = ClickedLink
        }



init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url
        { page         = NotFound
        , galleryModel = Tuple.first (Gallery.init version)
        , foldersModel = Tuple.first (Folders.init Nothing)
        , key          = key
        , version      = version
        }


type alias Model =
    { page         : Page
    , galleryModel : Gallery.Model
    , foldersModel : Folders.Model
    , key          : Nav.Key
    , version      : Float
    }


type Page
    = FoldersPage
    | GalleryPage
    | NotFound


type Route
    = Gallery
    | Folders
    | SelectedPhoto String



view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage ->
                    Folders.view model.foldersModel
                        |> Html.map GotFoldersMsg

                GalleryPage ->
                    Gallery.view model.galleryModel
                        |> Html.map GotGalleryMsg

                NotFound ->
                    Html.text "Not Found"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ layout UI.body
            <| lazy viewHeader model.page
        , content
        , layout []
            <| viewFooter
        ]
    }


viewHeader : Page -> Element Msg
viewHeader page =
    let
        logo =
            column UI.h1 [text "Photo Groove"]

        links =
            row []
                [ navLink Folders { url = "/",        caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Route -> { url : String, caption : String } -> Element msg
        navLink route { url, caption } =
            column (isActive { link = route, page = page })
                [ link []
                    { url = url
                    , label = text caption
                    }
                ]
    in
    row [ Region.navigation ] [ logo, links ]


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders        Parser.top
        , Parser.map Gallery        (s "gallery")
        , Parser.map SelectedPhoto  (s "photos" </> Parser.string)
        ]


isActive : { link : Route, page : Page } -> List (Attribute msg)
isActive { link, page } =
    case ( link, page ) of
        ( Gallery        , GalleryPage ) -> UI.nav True

        ( Gallery        , _           ) -> UI.nav False

        ( Folders        , FoldersPage ) -> UI.nav True

        ( Folders        , _           ) -> UI.nav False

        ( SelectedPhoto _, _           ) -> UI.nav False

viewFooter : Element msg
viewFooter =
    el UI.footer
        <| text "One is never alone with a rubber duck. -Douglas Adams"



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage ->
                    toFolders model (Folders.update foldersMsg model.foldersModel)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage ->
                    toGallery model (Gallery.update galleryMsg model.galleryModel)

                _ ->
                    ( model, Cmd.none )


type Msg
    = ClickedLink   Browser.UrlRequest
    | ChangedUrl    Url
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage, foldersModel = folders }
    , Cmd.map GotFoldersMsg cmd
    )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( gallery, cmd ) =
    ( { model | page = GalleryPage, galleryModel = gallery }
    , Cmd.map GotGalleryMsg cmd
    )


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Gallery ->
            toGallery model <|
                let
                    initGalleryModel =
                        (Gallery.init model.version)

                in
                -- if the gallery has not been initialized, proceed with regular initialization
                -- if the gallery has been initialized, model.galleryModel has the previous build, and should be used
                -- to reload the page via Gallery.reload, which also called Gallery.applyFilters to load the main-canvas
                if model.galleryModel == Tuple.first initGalleryModel then
                    initGalleryModel

                else
                    Gallery.reload model.galleryModel

        Just Folders ->
            toFolders model <|
                let
                    initFoldersModel =
                        (Folders.init Nothing)

                in
                -- if the folders page has not been initialized, proceed with regular initialization
                -- if the folders page has been initialized, model.foldersModel has the previous build, and should be used instead
                if model.foldersModel == Tuple.first initFoldersModel then
                    initFoldersModel

                else
                    ( model.foldersModel, Cmd.none )

        Just (SelectedPhoto filename) ->
            let
                foldersModel = model.foldersModel

            in
            -- Update model.foldersModel.selectPhotoUrl to filename
            toFolders model ( { foldersModel | selectedPhotoUrl = (Just filename) }, Cmd.none )


        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage ->
            Gallery.subscriptions model.galleryModel
                |> Sub.map GotGalleryMsg
        _ ->
            Sub.none