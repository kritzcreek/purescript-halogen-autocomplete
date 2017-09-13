module Halogen.Autocomplete.Component where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.KeyboardEvent as KeyEv
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.MouseEvent as MouseEvent
import DOM.HTML.HTMLElement (offsetTop)
import DOM.HTML.Types (HTMLElement, htmlElementToElement, htmlElementToNode)
import DOM.Node.Element (clientHeight, setScrollTop)
import DOM.Node.Node (childNodes)
import DOM.Node.NodeList as NodeList
import Data.Array (filter, length, mapWithIndex, null, (!!))
import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String as String
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Unsafe.Coerce (unsafeCoerce)

data Query item a
  = Init a
  | UpdateItems (Array item) a
  | Input String a
  | Select item a
  | ItemClick item MouseEvent a
  | Blur a
  | Previous a
  | Next a
  | Open a
  | Close Reason a
  | KeyDown KeyboardEvent a

data Reason
  = CuzEscape
  | CuzBlur
  | CuzNoMatches
  | CuzSelect

showReason ∷ Reason → String
showReason = case _ of
  CuzEscape → "CuzEscape"
  CuzBlur → "CuzBlur"
  CuzNoMatches → "CuzNoMatches"
  CuzSelect → "CuzSelect"

type State item =
  { open ∷ Boolean
  , statusText ∷ String
  , index ∷ Maybe Int
  , items ∷ Array item
  , inputText ∷ String
  }

data Message item = Changed String | Selected item
type Input = Array

type HTML item = H.ComponentHTML (Query item)
type DSL item m = H.ComponentDSL (State item) (Query item) (Message item) m

type Config item =
  { containerClass ∷ HH.ClassName
  , itemFilter ∷ String → item → Boolean
  , itemText ∷ item → String
  , itemDisplay ∷ item → H.HTML Void (Const Void)
  }

defaultConfig ∷ Config String
defaultConfig =
  { containerClass: HH.ClassName "halogen-autocomplete"
  , itemFilter: \input item → not String.null input && String.contains (String.Pattern input) item
  , itemText: id
  , itemDisplay: \item → HH.text item
  }

component
  ∷ ∀ item e m
  . MonadEff (dom ∷ DOM | e) m
  ⇒ Config item
  → H.Component HH.HTML (Query item) (Input item) (Message item) m
component { containerClass, itemFilter, itemText, itemDisplay } =
  H.lifecycleComponent
   { initialState
   , render
   , eval
   , initializer: Just (H.action Init)
   , finalizer: Nothing
   , receiver: HE.input UpdateItems
   }
  where
    initialState =
      { open: false
      , statusText: ""
      , index: Nothing
      , items: _
      , inputText: ""
      }

    render ∷ State item → HTML item
    render state =
      HH.div
        [ HP.class_ containerClass
        ]
        [ HH.input [ HP.value state.inputText
                   , HE.onValueInput (HE.input Input)
                   , HE.onBlur (HE.input_ Blur)
                   , HE.onKeyDown (HE.input KeyDown)
                   ]
        , HH.ul
            (join
              [ guard (not state.open) $> Aria.hidden ""
              , guard (not state.open) $> HP.class_ (H.ClassName "hidden")
              , pure (HP.ref ulRef)
              ])
            (mapWithIndex mkSelection (filter (itemFilter state.inputText) state.items))
        , HH.span [ className "visually-hidden"
                  , Aria.role "status"
                  , Aria.live "assertive"
                  , Aria.relevant "additions"
                  ] [HH.text state.statusText]
        ]
      where
        mkSelection ix item =
          HH.li
            [ HE.onMouseDown (HE.input (ItemClick item))
            , Aria.selected (if Just ix == state.index then "true" else "false")
            ]
            [ bimap absurd (absurd <<< un Const) (itemDisplay item) ]

    eval ∷ Query item ~> DSL item m
    eval = case _ of
     Init a → pure a
     UpdateItems items a → do
       H.modify (_ { items = items })
       pure a
     Input input a → do
       H.modify (_ { inputText = input })
       { items } ← H.get
       H.raise (Changed input)
       if null (filter (itemFilter input) items)
         then do
           close CuzNoMatches
           pure a
         else eval (Open a)
     Blur a → do
       close CuzBlur
       pure a
     ItemClick item ev a → do
       case MouseEvent.button ev of
        0 -> do
         H.liftEff (preventDefault (MouseEvent.mouseEventToEvent ev))
         close CuzSelect
         eval (Select item a)
        _ -> pure a
     Select item a → do
       let newInput = itemText item
       H.modify (_ { inputText = newInput })
       H.raise (Changed newInput)
       H.raise (Selected item)
       pure a
     Previous a → do
       { index } ← H.get
       case index of
         Nothing → pure a
         Just ix → do
           count ← length <$> displayedItems
           goto itemText (if ix == 0 then count - 1 else ix - 1)
           pure a
     Next a → do
       { index } ← H.get
       case index of
         Nothing → pure a
         Just ix → do
           count ← length <$> displayedItems
           goto itemText (if ix == count then 0 else ix + 1)
           pure a
     Open a → do
       H.modify (_ { open = true, index = Just 0 })
       pure a
     Close reason a → do
       close reason
       pure a
     KeyDown ev a → do
       case KeyEv.code ev of
         "Enter" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           { index } ← H.get
           items ← displayedItems
           case (items !! _) =<< index of
             Just item → do
              close CuzSelect
              eval (Select item a)
             Nothing → pure a
         "Escape" → do
           close CuzEscape
           pure a
         "ArrowUp" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           eval (Previous a)
         "ArrowDown" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           eval (Next a)
         _ → pure a

    displayedItems = do
      { items, inputText } ← H.get
      pure (filter (itemFilter inputText) items)

goto
  ∷ ∀ m e item
  . MonadEff (dom ∷ DOM | e) m
  ⇒ (item → String)
  → Int
  → DSL item m Unit
goto itemText index = do
  H.modify \state →
    state
      { index = Just index
      , statusText = fromMaybe "" (itemText <$> (state.items !! index))
      }
  H.getHTMLElementRef ulRef >>= traverse_ (scrollListToIndex index)

close ∷ ∀ item m. Reason → DSL item m Unit
close reason = do
  whenM (H.gets _.open) do
    H.modify (_ { index = Nothing, open = false })

scrollListToIndex ∷ ∀ m e. MonadEff (dom ∷ DOM | e) m ⇒ Int → HTMLElement → m Unit
scrollListToIndex index el = H.liftEff do
  lis ← childNodes (htmlElementToNode el)
  NodeList.item index lis >>= traverse_ \item → do
    let
      -- TODO: Let's try to do better here
      itemElement ∷ HTMLElement
      itemElement = unsafeCoerce item
    itemTop ← offsetTop itemElement
    ulHeight ← clientHeight (htmlElementToElement el)
    itemHeight ← clientHeight (htmlElementToElement itemElement)
    setScrollTop (itemTop - ulHeight + itemHeight) (htmlElementToElement el)


ulRef ∷ H.RefLabel
ulRef = H.RefLabel "autocomplete-ul"

className ∷ ∀ r a. String → HP.IProp ("class" ∷ String | r) a
className = HP.class_ <<< HH.ClassName
