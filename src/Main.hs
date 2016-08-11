{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Main
(
  main,
)
where


import BasePrelude hiding (on)
-- Monads
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
-- Text
import qualified Data.Text.All as T
-- Random
import System.Random
-- GTK
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView




main = do

  -- Init GTK.
  initGUI

  -- Create a window which would finish the GTK loop
  -- after being closed.
  window <- windowNew
  window `after` objectDestroy $
    mainQuit

  -- Create a WebView inside.
  webView <- webViewNew
  set window [containerChild := webView]

  -- Make the WebView display a random number on key press.
  webView `on` keyReleaseEvent $ lift $ do
    x <- randomIO :: IO Int
    webViewLoadString webView (show x) Nothing ""
    print x
    return True

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
