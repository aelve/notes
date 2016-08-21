{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Main
(
  main,
)
where


import BasePrelude hiding (on, Control)
-- Monads
import Control.Monad.IO.Class
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Interpolation
import NeatInterpolation
-- GTK
import Graphics.UI.Gtk hiding (get, set)
import qualified Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.DOM.Document (querySelector)
import Graphics.UI.Gtk.WebKit.Types hiding (Text)
import qualified Graphics.UI.Gtk.WebKit.DOM.EventM as Ev
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as E
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as Node

import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLTextAreaElement as TextAreaElement
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as InputElement


main :: IO ()
main = do

  -- Init GTK.
  initGUI

  -- Create a window which would finish the GTK loop
  -- after being closed.
  window <- windowNew
  window `after` objectDestroy $
    mainQuit

  v   <- vBoxNew False 2
  bar <- entryNew
  sw  <- scrolledWindowNew Nothing Nothing
  wv  <- webViewNew
  set window [
    windowDefaultWidth := 700,
    windowDefaultHeight := 500,
    containerChild := v,
    containerBorderWidth := 2 ]
  wvs <- get wv webViewWebSettings
  set wvs [
    webSettingsEnableDeveloperExtras := True ]
  wvi <- get wv webViewInspector
  wvi `on` inspectWebView $ \_ -> do
    iw <- windowNew
    iwv <- webViewNew
    set iw [
      containerChild := iwv ]
    widgetShowAll iw
    return iwv

  boxPackStart v bar PackNatural 0
  boxPackStart v sw PackGrow 0
  set sw [
    containerChild := wv ]

  bar `on` entryActivate $ do
    url :: Text <- entryGetText bar
    webViewLoadUri wv url

  webViewLoadString wv [text|
    <input type="text" id="input" placeholder="string to reverse">
    <div id="result" style="font-size:20px"></div>
  |] (Just "text/html") ""

  wv `on` documentLoadFinished $ \_ -> do
    Just doc <- webViewGetDomDocument wv
    input  <- castToHTMLInputElement <$> select doc "#input"
    result <- select doc "#result"
    input `Ev.on` E.keyDown $ do
      i <- Ev.uiKeyCode
      when (i == 13 || i == 10) $ do
        val <- get input _value
        set result [_text := T.reverse val]
    return ()

  --wv `on` keyReleaseEvent $ tryEvent $ do
  --  [Control] <- eventModifier
  --  "i" <- eventKeyName
  --  webViewInspector


  -- Run GTK.
  widgetShowAll window
  mainGUI

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

select :: (MonadIO m, DocumentClass self) => self -> Text -> m Element
select d s = do
  r <- querySelector d s
  case r of
    Nothing -> error ("select: couldn't find " ++ show s)
    Just e  -> return e

class HasValue e where
  get_value_maybe :: e -> IO (Maybe Text)
  set_value_maybe :: e -> Maybe Text -> IO ()
  _value :: ReadWriteAttr e Text Text
  _value = newAttr
    (\e -> do mbV <- get_value_maybe e
              case mbV of
                Nothing -> error "value: can't get value"
                Just v  -> return v)
    (\e x -> set_value_maybe e (Just x))

instance HasValue HTMLTextAreaElement where
  get_value_maybe = TextAreaElement.getValue
  set_value_maybe = TextAreaElement.setValue
instance HasValue HTMLInputElement where
  get_value_maybe = InputElement.getValue
  set_value_maybe = InputElement.setValue

class HasText e where
  get_text_maybe :: e -> IO (Maybe Text)
  set_text_maybe :: e -> Maybe Text -> IO ()
  _text :: ReadWriteAttr e Text Text
  _text = newAttr
    (\e -> do mbV <- get_text_maybe e
              case mbV of
                Nothing -> error "text: can't get text"
                Just v  -> return v)
    (\e x -> set_text_maybe e (Just x))

instance NodeClass n => HasText n where
  get_text_maybe = Node.getTextContent
  set_text_maybe = Node.setTextContent

get :: MonadIO m => o -> ReadWriteAttr o a b -> m a
get o a = liftIO (GTK.get o a)

set :: MonadIO m => o -> [AttrOp o] -> m ()
set o as = liftIO (GTK.set o as)


{-

main :: IO ()
main = do
  initGUI

  window <- windowNew
  window `on` objectDestroy $ mainQuit
  set window [
    windowTitle := ("Aelve Notes" :: Text),
    windowGravity := GravityCenter,
    windowWindowPosition := WinPosCenter ]
--     windowDefaultWidth := fst _windowSize,
--     windowDefaultHeight := snd _windowSize ]

  -- On Escape, exit.
  window `on` keyPressEvent $ tryEvent $ do
    "Escape" <- eventKeyName
    liftIO mainQuit

  -- On exit, save window size.
  -- window `on` deleteEvent $ liftIO $ do
  --   Rectangle _ _ width height <- widgetGetAllocation window
  --   modifyConfig $ \config -> return (config & windowSize .~ (width, height))
  --   return False

  widgetShowAll window
  mainGUI

-}
