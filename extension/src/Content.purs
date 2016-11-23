module Content where

import Prelude
import DOM.HTML.Window as Window
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.Types (Event, EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.Node.Document (createElement, getElementsByTagName)
import DOM.Node.Element (getAttribute)
import DOM.Node.HTMLCollection (length, item)
import DOM.Node.Node (setTextContent, insertBefore, parentNode)
import DOM.Node.Types (Document, elementToNode, elementToEventTarget, Element, HTMLCollection)
import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonEmptyObject, (~>), (:=))
import Data.Array (groupBy, filter, catMaybes, snoc, reverse)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.NonEmpty (foldl1)
import Data.Nullable (toMaybe)
import Data.String (joinWith)
import Data.String.Regex (test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse_, sequence, traverse)
import Data.Tuple (Tuple(Tuple), fst, swap)

htmlCollectionToArray :: forall e. HTMLCollection -> Eff (dom :: DOM | e) (Array Element)
htmlCollectionToArray col = go (pure []) =<< (_ - 1) <$> (length col)
  where go arr index = do
          i <- toMaybe <$> item index col
          case i of
            Nothing -> reverse <$> arr
            Just i' -> go ((_ `snoc` i') <$> arr) (index - 1)

urlRegExs :: Array String
urlRegExs = ["(www|m)\\.youtube\\.com/(watch|playlist)"
            ,"youtu\\.be"
            ,"player\\.vimeo\\.com"
            ,"vimeo\\.com"
            ,"www\\.twitch\\.tv/\\w+$"
            ,"www\\.twitch\\.tv/.+/v/\\d+$"
            ,"clips.twitch.tv/.+/.+"
            ,"streamable\\.com"
            ,".*\\.streamable\\.com/video"
            ,"vid\\.me"
            ,"www.liveleak.com/view"
            ,"twitter\\.com/.+/video/"
            ,"www.facebook\\.com/.+/videos/"]

extractHref :: forall e. Element -> Eff (dom :: DOM | e) (Tuple (Maybe String) Element)
extractHref element = do
  h <- getAttribute "href" element
  pure $ Tuple (toMaybe h) element

testHref :: (Tuple String Element) -> Boolean
testHref (Tuple href element) =
  case regex ("https?://(" <> joinWith "|" urlRegExs <> ")") noFlags of
    Left _ -> false
    Right r -> test r href

addButton :: forall e. Document
             -> Tuple String Element
             -> Eff (dom :: DOM, browser :: BROWSER | e) Unit
addButton document (Tuple href element) = do
  let n = elementToNode element
  p <- parentNode n
  case toMaybe p of
    Nothing -> pure unit
    Just p' -> do
      be <- createElement "button" document
      let bn = elementToNode be
      setTextContent "▶" bn
      addEventListener
        (EventType "click")
        (eventListener (sendLink href))
        false
        (elementToEventTarget be)
      _ <- insertBefore bn n p'
      pure unit

foreign import data BROWSER :: !
foreign import sendMessage :: forall e. Json -> Eff (browser :: BROWSER | e) Unit

newtype Msg = Msg {link :: String}
instance encodeMsg :: EncodeJson Msg where
  encodeJson (Msg {link}) = "link" := link ~> jsonEmptyObject

sendLink :: forall e. String -> Event -> Eff (browser :: BROWSER | e) Unit
sendLink href _ = sendMessage (encodeJson (Msg {link: href}))

main :: forall e. Eff (browser :: BROWSER, dom :: DOM | e) Unit
main = do
  document <- (window >>= Window.document) <#> htmlDocumentToDocument
  getElementsByTagName "a" document
    >>= htmlCollectionToArray
    >>= traverse extractHref
    <#> (map (swap >>> sequence)
         >>> catMaybes
         >>> map swap
         >>> filter testHref
         >>> groupBy (\a b -> fst a == fst b)
         >>> map (foldl1 \_ b -> b))
    >>= traverse_ (addButton document)
