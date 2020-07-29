module UI exposing (..)

import Element            exposing (..)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input
import Element.Lazy       as Lazy
import Element.Region     as Region


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top    = 0
    , right  = 0
    , bottom = 0
    , left   = 0
    }


fontColor : Color
fontColor =
    rgb255 250 250 250


backgroundColor : Color
backgroundColor =
    rgb255 44  44  44


white : Color
white =
    rgb255 255 255 255


blue : Color
blue =
    rgb255 96 181 204


grey : Color
grey =
    rgb255 85 85 85


{--
body
=  font-family: Verdana;
=  background-color: rgb(44, 44, 44);
=  color: rgb(250, 250, 250); }
--}
body : List (Attribute msg)
body =
    [ Font.family
        [ Font.typeface "Verdana" ]
    , Background.color  backgroundColor
    , Font.color        fontColor
    ]


{--
h1
  color: #60b5cc; margin-bottom: 0;
--}
h1 : List (Attribute msg)
h1 =
    [ Font.size   32
    , Font.bold
    , Font.color  blue
    , paddingEach { edges | top = 10 , left = 5, right = 20 }
    ]


{--
nav a
  padding: 5px 15px;
  color: white;
  font-weight: bold;

nav .active
  text-decoration: underline;

nav a:hover
  text-decoration: underline;
--}
nav : Bool -> List (Attribute msg)
nav active =
    let
        maybeActive : List (Attribute msg)
        maybeActive =
            if active then
                [ Font.underline ]
            else
                []

    in
    List.append
        maybeActive
        [ paddingEach { edges | top = 10, left = 15, right = 15 }
        , Font.color  white
        , Font.bold
        ]


hoverUnderline : List (Attribute msg)
hoverUnderline =
    [
    Border.widthEach { edges | bottom = 2 }
    , Border.color   backgroundColor
    , mouseOver      [ Border.color white ]
    ]


photoFoldersContent : List (Attribute msg)
photoFoldersContent =
    [ padding 40
    , centerX
    ]



{--
.folder
  margin-left: 30px;

.folder > label
  background-color: #555;
  margin: 6px 8px;
  padding: 6px 8px;
  display: inline-block;
  cursor: pointer;

.folder > label:hover
  background-color: #60b5cc;

.folder.expanded label::before
  content: "▸";
  margin-right: 12px;

.folder.collapsed label::before
  content: "▾";
  margin-right: 12px;
--}
folderLabel : List (Attribute msg)
folderLabel =
    [ paddingEach      { edges | top = 6, left = 8, right = 8, bottom = 6 }
    , Background.color grey
    , mouseOver        [ Background.color blue ]
    , pointer
    ]


cascade : List (Attribute msg)
cascade =
    [ paddingEach { edges | left = 12, top = 8 }
    ]


{--
.photo
  padding: 6px 8px;
  margin-left: 30px;
  cursor: pointer;
  color: white;
  display: block;
  text-decoration: none;

.photo:hover
  background-color: #60b5cc;
--}
photo : List (Attribute msg)
photo =
    [ paddingEach { edges | top = 6, left = 8, right = 8}
    , pointer
    , Font.color  white
    , mouseOver   [ Background.color blue ]
    ]


{--
.folders, .selected-photo
  float: left;
  min-height: 400px;
  width: 360px;

.selected-photo h3
  margin-top: 60px;
  margin-bottom: 20px;
  color: #60b5cc;

.selected-photo img
  display: block;

.selected-photo
  width: 600px;
--}
h3 : List (Attribute msg)
h3 =
    [ paddingEach { edges | top = 60, bottom = 20 }
    , Font.color  blue
    , Font.bold
    ]


selectedPhoto : List (Attribute msg)
selectedPhoto =
    [ alignLeft
    , height
        (fill
            |> minimum 400
        )
    , width (px 600)
    ]


folders : List (Attribute msg)
folders =
    [ alignLeft
    , alignTop
    , height
        (fill
            |> minimum 400
        )
    , width (px 360)
    ]


{--
img, canvas
    border: 1px solid white;
    margin: 5px;
--}
image : List (Attribute msg)
image =
    [ Border.width 1
    , Border.solid
    , Border.color white
    ]


{--
.related-photos
  clear: both;

.related-photo
  float: left;

.related-photo:hover
  float: left;
  border-color: #60b5cc;

.related-photo
  cursor: pointer;
--}
relatedPhoto : List (Attribute msg)
relatedPhoto =
    List.append image
        [ alignLeft
        , mouseOver [ Border.color blue ]
        , pointer
        ]


h2 : List (Attribute msg)
h2 =
    [ Font.size   24
    , paddingEach { edges | top = 13, bottom = 13 }
    , Font.bold
    ]



{--
.content
    margin: 40px auto;
    width: 960px;
--}
photoGalleryContent : List (Attribute msg)
photoGalleryContent =
    [ padding 40
    , width (px 960)
    ]


galleryOptions : List (Attribute msg)
galleryOptions =
    [ explain Debug.todo
    ]


thumbnailLabel : List (Attribute msg)
thumbnailLabel =
    [ paddingEach { edges | top = 20, bottom = 20 }
    , Font.color  blue
    , Font.bold
    , centerY
    ]


{--
button
  float: right; background-color: #60b5cc; border: 0; color: rgb(44, 44, 44);
  font-size: 24px;
  cursor: pointer;
  padding: 10px 30px;

button:hover
  background-color: white;
--}
button : List (Attribute msg)
button =
    [ Background.color blue
    , alignRight
    , Border.widthEach edges
    , Font.color       backgroundColor
    , Font.size        24
    , pointer
    , padding          10
    , mouseOver        [ Background.color white ]
    ]


{--
.activity
  position: absolute;
  top: 40px;
  right: 100px;
--}
activity : List (Attribute msg)
activity =
    [ alignRight
    ]


{--
.filters
  width: 318px;
  float: right;
--}
filters : List (Attribute msg)
filters =
    [ width (px 318)
    , alignRight
    ]


{--
#choose-size
  float: left;
  margin-left: 20px;

#choose-size > span
  display: inline-block;
  margin: 0 10px;
--}
chosenSize : List (Attribute msg)
chosenSize =
    [ alignLeft
    , paddingEach { edges | left = 20 }
    ]


chosenSizeSpan : List(Attribute msg)
chosenSizeSpan =
    [ alignLeft
    , paddingEach { edges | left = 10, right = 10 }
    ]


{--
#thumbnails { width: 440px; float: left; clear: both; }
#thumbnails.small img { width: 50px; }
#thumbnails.med img { width: 100px; }
#thumbnails.large img { width: 200px; }
.selected { margin: 0; border: 6px solid #60b5cc; }
--}
thumbnails : List (Attribute msg)
thumbnails =
    [ width (px 440)
    , alignLeft
    , spacing 5
    ]


thumbnail : String -> Bool -> List (Attribute msg)
thumbnail size selected =
    let
        maxSize : Int
        maxSize =
            case size of
                "small" ->
                    50

                "med" ->
                    100

                "large" ->
                    200

                _ ->
                    100

        border : List (Attribute msg)
        border =
            if selected then
                [ Border.width 6
                , Border.solid
                , Border.color blue
                ]
            else
                image

    in
    border ++
    [ width (px maxSize)
    ]



{--
footer
=  color: #bbb;
=  margin: 20px;
=  margin-top: 60px;
--}
footer : List (Attribute msg)
footer =
    [ Font.color <| rgb255 187 187 187
    , paddingEach { top = 60, right = 20, bottom = 20, left = 20 }
    , Region.footer
    ]


{--
Things to keep or look into:

  list-style: none;
  display: inline-block;
--}