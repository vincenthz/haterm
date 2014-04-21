{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad (unless)
import Control.Monad.Trans
import qualified Control.Exception as E
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte
import System.Exit
import System.FilePath
import System.Environment
import Data.IORef

handled f = f >> return True
unHandled = return False

resize term width height = do
    (Rectangle _ _ aw ah) <- widgetGetAllocation term
    window <- maybe (error "no window") id <$> widgetGetWindow term
    b  <- widgetGetRealized term
    st <- drawWindowGetState (castToDrawWindow window)

    --putStrLn ("resized " ++ show width ++ "x" ++ show height)

    unless (b
        || (WindowStateMaximized `elem` st || WindowStateFullscreen `elem` st)
        || width == aw && height == ah
        ) $ do
            cWidth <- terminalGetCharWidth term
            cHeight <- terminalGetCharHeight term

            --style <- get term widgetStyle
            let gWidth = (width - {-(inner.left + inner.right) -} 0) `div` cWidth
                gHeight = (height -  {- (inner.top + inner.bottom) -} 0) `div` cHeight
            terminalSetSize term gWidth gHeight

            return ()

parseConfig = foldl doAcc M.empty . lines
  where doAcc acc s =
            case break (== '=') s of
                (_, "")    -> acc
                (k, '=':v) -> M.insert k v acc
                (_,_)      -> error "break failed"

main :: IO ()
main = do
    home <- getEnv "HOME"
    cfg <- (parseConfig <$> readFile (home </> ".haterm"))
            `E.catch` (\(_ :: E.IOException) -> return M.empty)

    _ <- initGUI
    window <- windowNew
    t <- terminalNew

    fsz  <- newIORef (10 :: Int)
    let fontName = "monospace"
    let getFont = readIORef fsz >>= \sz -> return (fontName ++ " " ++ show sz)

    idproc <- terminalForkCommand t Nothing Nothing Nothing Nothing False False False
    _ <- t `on` childExited $ do
        ces <- terminalGetChildExitStatus t
        putStrLn ("childExited: " ++ show ces)
        exitSuccess
    _ <- t `on` decreaseFontSize $ do
            liftIO $ putStrLn "decreased"
    _ <- t `on` increaseFontSize $ do
            liftIO $ putStrLn "increased"

    let red = Color 0xffff 0 0x0000
        yellow = Color 0xf100 0xbe00 0x2200
        yellowHi = Color 0xffff 0xdc00 0x0000
        blue = Color 0 0 0xffff
        blueHi = Color 0x6666 0x9fff 0xe777 -- 669FE7
        green = Color 0 0xffff 0
        greenHi = Color 0xffff 0xdccc 0x0000 -- FFDC00
        white = Color 0xffff 0xffff 0xffff
        black = Color 0x0 0x0 0x0
        magenta = Color 0x0 0xffff 0xffff
        magentaHi = Color 0xa555 0x1eee 0xc222 -- #A51EC2
        cyan = Color 0x2300 0x9200 0x8600
        cyanHi = Color 0x1bbb 0xdddd 0xd000 -- #1BDDD0
        grey = Color 0x7777 0x7777 0x7777
        darkGrey1 = Color 0x2727 0x2727 0x2727
        darkGrey2 = Color 0x3333 0x3333 0x3333
        foreground = white
        background = black

    terminalSetColors t foreground background
        [darkGrey1,red,green,red,red,red,red,red
        ,darkGrey2,red,greenHi,yellowHi,blueHi,magentaHi,cyanHi,red]
    {-
        [darkGrey1,red,green,yellow,blue,magenta,cyan,grey
        ,darkGrey2,red,greenHi,yellowHi,blueHi,magentaHi,cyanHi,grey]
    -}

    let decreaseFont = liftIO $ do
            modifyIORef fsz (\i -> i - 2)
            getFont >>= terminalSetFontFromString t
        increaseFont = liftIO $ do
            modifyIORef fsz (+ 2)
            getFont >>= terminalSetFontFromString t
        pasteFromClipboard = liftIO $ terminalPasteClipboard t
        copyToClipboard = liftIO $ terminalCopyClipboard t

    _ <- t `on` keyPressEvent $ do
        em   <- eventModifier
        name <- eventKeyName
        if Control `elem` em
            then let hasShift = Shift `elem` em
                  in if hasShift
                        then case name of
                                "plus" -> handled increaseFont
                                "V"    -> handled pasteFromClipboard
                                "C"    -> handled copyToClipboard
                                _      -> unHandled
                        else case name of
                                "minus" -> handled decreaseFont
                                _       -> unHandled
            else unHandled
    _ <- t `on` resizeWindow $ resize t

    terminalSetAllowBold t True
    terminalSetColorHighlight t (Color 0xffff 0xffff 0x0)
    terminalSetScrollbackLines t 16384

    --s <- terminalGetStatusLine t
    --putStrLn $ show s
    --terminalSetFontFromString t "Courier Italic 10"

    terminalFeed t "Welcome to haterm\n\r"

    containerAdd window t
    widgetShowAll window

    mainGUI
