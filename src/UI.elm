module UI exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input
import Element.Lazy       as Lazy
import Element.Region     as Region


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top    = 60
    , right  = 20
    , bottom = 20
    , left   = 20
    }


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
    , Background.color <| rgb255 44  44  44
    , Font.color       <| rgb255 250 250 250
    ]


{--
h1
=  color: #60b5cc; margin-bottom: 0;
+  display: inline-block;
--}
h1 : List (Attribute msg)
h1 =
    [ Region.heading 1
    , Font.color <| rgb255 96 181 204
    , paddingEach {edges | bottom = 0}
    -- display: inline-block
    ]


{--
nav ul
+  list-style: none;
+  display: inline-block;

nav li
+  display: inline-block;

nav a
+  text-decoration: none;
=  padding: 5px 15px;
=  color: white;
=  font-weight: bold;

nav .active
=  text-decoration: underline;

nav a:hover
+  text-decoration: underline;
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
        [ paddingEach { edges | top = 5, left = 15, right = 15, bottom = 0 }
        , Font.color <| rgb255 255 255 255
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
    , paddingEach edges
    , Region.footer
    ]


{--
Things to keep or look into:

  list-style: none;
  display: inline-block;
--}