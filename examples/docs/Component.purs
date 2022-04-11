module WAGS.Example.Docs.Component where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Control (text_)
import Deku.Core (Element)
import Deku.DOM as D
import WAGS.Example.Docs.Types (Page(..))
import WAGS.Example.Docs.Util (scrollToTop)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy """<div>
  <h1>A Simple Component</h1>

  <h3>Slightly more bells and whistles</h3>
  <p>
    Let's look at an example with several different DOM tags. There are also a few different syntax options depending on what tune your fingers wish to type.
  </p>

  ~code~

  <p>And here's what it produces:</p>

  <blockquote> ~result~ </blockquote>

  <h1>Elements all the way down</h1>
  <p>Deku encodes the DOM using functions that represent elements. To create an element with a custom tag called <code>foo</code>, you can call <code>elementify "foo"</code> and that will creat a custom <code>foo</code> tag in your DOM.</p>
  <p>The encoding of elements in Deku is not unlike that in React or Halogen. It's a tree, like the Deku tree. So go nuts!</p>

  <h1>Attributes</h1>
  <p>Elements postfixed with an underscore do not take attributes. That is un-fun, so in this example, you see attributes as well! Attributes like an element's style or id are specified as the first argument to an underscore-less element. Note that, if you have multiple attributes, you have to separate them with <code>alt</code>, aka "the tie fighter." Also, the many <code>bang</code>-s you'll see around attributes are from the <code>purescript-event</code> library. Bang means <i>right now</i>, which is the answer to the question "When should we set this attribute?"</p>

  <p>As an example, we made the input a range slider using <code>bang (Xtype := "range")</code>. Unlike Halogen, there are no checks to make sure you give a valid string. So if you want your range slider to have the value of true, you can. One day, I may build some validators, but passing strings works decently well here.</p>

  <h2>Next steps</h2>
  <p>In this section, we built a simple component. In the next section, we'll recreate the exact same element using a different input syntax called <a ~next~ style="cursor:pointer;">Pursx</a>.</p>
</div>"""

components :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
components dpage = px ~~
  { code: nut
      ( D.pre_ [D.code_
          [ text_
              """module Main where

import Prelude

import Deku.Control (flatten, text_)
import Deku.DOM as D
import Deku.Attribute ((:=))
import Deku.Toplevel ((🚀))
import Effect (Effect)

main :: Effect Unit
main = unit 🚀 \_ _ ->
  flatten
    [ D.button_ [ text_ "I do nothing" ]
    , D.ul_ $ map (D.li_ <<< pure <<< text_) [ "A", "B", "C" ]
    , D.div_
        [ D.a (bang $ D.Href := "https://example.com")
            [ text_ "foo " ]
        , D.i_ [ text_ " bar " ]
        , D.span (bang $ D.Style := "font-weight: 800;")
            [ text_ " baz" ]
        ]
    , D.div_
        [ D.div_
            [ D.div_ [ D.input (bang $ D.Xtype := "range") [] ]
            ]
        ]
    ]"""
          ]]
      )
  , result: nut
      ( D.div_
          [ D.button_ [ text_ "I do nothing" ]
          , D.ul_ $ map (D.li_ <<< pure <<< text_) [ "A", "B", "C" ]
          , D.div_
              [ D.a (bang $ D.Href := "https://example.com")
                  [ text_ "foo " ]
              , D.i_ [ text_ " bar " ]
              , D.span (bang $ D.Style := "font-weight: 800;")
                  [ text_ " baz" ]
              ]
          , D.div_
              [ D.div_
                  [ D.div_ [ D.input (bang $ D.Xtype := "range") [] ]
                  ]
              ]
          ]
      )
    , next: bang (D.OnClick := (cb (const $ dpage PURSX1 *> scrollToTop)))
  }