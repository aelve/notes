{-# LANGUAGE ScopedTypeVariables #-}
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
-- Random
import System.Random
-- GTK
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebInspector




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

  --wv `on` keyReleaseEvent $ tryEvent $ do
  --  [Control] <- eventModifier
  --  "i" <- eventKeyName
  --  webViewInspector


  -- Run GTK.
  widgetShowAll window
  mainGUI

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
