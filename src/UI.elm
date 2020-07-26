module UI exposing (..)

import Element            exposing (..)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input
import Element.Lazy       as Lazy
import Element.Region     as Region
import Html               as Html


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
    [ Font.size 32
    , Font.bold
    , Font.color blue
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
        , Font.color white
        , Font.bold
        ]


hoverUnderline : List (Attribute msg)
hoverUnderline =
    [
    Border.widthEach { edges | bottom = 2 }
    , Border.color backgroundColor
    , mouseOver  [ Border.color white ]
    ]


{--
.content
    margin: 40px auto;
    width: 960px;
--}
content : List (Attribute msg)
content =
    [ padding 40
    , width (px 960)
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
    [ paddingEach { edges | top = 6, left = 8, right = 8}
    , mouseOver [ Background.color blue ]
    , Background.color grey
    , pointer
    ]


expanded : List (Attribute msg)
expanded =
    [ onLeft
        ( el
            [ paddingEach { edges | left = 12 } ]
            ( text "▸" )
        )
    ]


collapsed : List (Attribute msg)
collapsed =
    [ onLeft
        ( el
            [ paddingEach { edges | left = 12 } ]
            ( text "▾" )
        )
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
    , Font.color white
    , mouseOver [ Background.color blue ]
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
    , Font.color blue
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
    , spacing 5
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
    [ Font.size 24
    , paddingEach { edges | top = 13, bottom = 13 }
    , Font.bold
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
--    , explain Debug.todo
    ]


{--
Things to keep or look into:

  list-style: none;
  display: inline-block;
--}