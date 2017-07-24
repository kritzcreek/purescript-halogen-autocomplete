module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Autocomplete.Component as AC

conf ∷ AC.Config String
conf = AC.defaultConfig

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (AC.component conf) ["Hello", "Wello", "Yellow", "Darkness", "My old friend"] body
