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
import Control.Monad.Trans.Class
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Interpolation
import NeatInterpolation
-- GTK
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebInspector
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass, querySelector)
import Graphics.UI.Gtk.WebKit.Types hiding (Text)
import qualified Graphics.UI.Gtk.WebKit.DOM.EventM as Ev
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as E
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as I
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as N


fromMaybeM :: Monad m => String -> Maybe a -> m a
fromMaybeM st = maybe (fail st) return

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
    doc <- webViewGetDomDocument wv >>= fromMaybeM "no document"
    input  <- castToHTMLInputElement <$> select doc "#input"
    result <- select doc "#result"
    input `Ev.on` E.keyDown $ do
      i <- Ev.uiKeyCode
      when (i == 13 || i == 10) $ do
        val <- value input
        N.setTextContent result (Just (T.reverse val))
    return ()

  --wv `on` keyReleaseEvent $ tryEvent $ do
  --  [Control] <- eventModifier
  --  "i" <- eventKeyName
  --  webViewInspector


  -- Run GTK.
  widgetShowAll window
  mainGUI

select :: (MonadIO m, DocumentClass self) => self -> Text -> m Element
select d s = do
  r <- querySelector d s
  case r of
    Nothing -> error ("select: couldn't find " ++ show s)
    Just e  -> return e

value :: (MonadIO m, HTMLInputElementClass self) => self -> m Text
value e = do
  r <- I.getValue e
  case r of
    Nothing -> error "value: couldn't get value"
    Just v  -> return v

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
