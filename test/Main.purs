module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen.Aff as HA
import Halogen.Autocomplete.Component as AC
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

conf ∷ AC.Config String
conf = AC.defaultConfig

type Person = { name ∷ String, age ∷ Int }

customElemConf ∷ AC.Config Person
customElemConf = AC.defaultConfig
  { itemText = _.name
  , itemDisplay = \ {name, age} → HH.div_ [ HH.text name, HH.br_ , HH.code_ [HH.text ("Age: " <> show age)] ]
  , itemFilter = \input {name} → String.contains (String.Pattern input) name
  }

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff $ unsafePartial do
  body <- HA.awaitBody
  Just simpleDiv ← HA.selectElement (QuerySelector "#simpleExample")
  Just customElementDiv ← HA.selectElement (QuerySelector "#customElement")
  _ ← runUI (AC.component conf) ["Hello", "Wello", "Yellow", "Darkness", "My old friend"] simpleDiv
  runUI (AC.component customElemConf) [{name: "Carl", age: 25}, {name: "Peter", age: 33}] customElementDiv
