{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
--import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.WindowProperties
import XMonad.Actions.WindowGo
import XMonad.Util.Run
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List

myManageHook = composeAll [
  className =? "stabar" --> doFloat
  , title =? "capture" --> doCenterFloat
  , title =? "editwithemacs" --> doCenterFloat
--  , isFullscreen --> doFullFloat
  ]

main = do
  h <- spawnPipe myBar
  xmonad $ myConfig h

myConfig h = defaultConfig {
  modMask = mod4Mask
  , terminal = "sakura"
  , startupHook = setWMName "LG3D"
  , handleEventHook = fullscreenEventHook
  , logHook = dynamicLogWithPP myPP { ppOutput = hPutStrLn h }
  , layoutHook = smartBorders $ fullscreenSaverOff $ fullscreenFull $ avoidStruts $ layoutHook defaultConfig
  , manageHook = scratchpadManageHook (W.RationalRect 0.1 0.05 0.8 0.5) <+> manageDocks <+> fullscreenManageHook <+> myManageHook <+> (manageHook defaultConfig)
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#0000DD"
  } `additionalKeys` 
           [ ((0, 0x1008FF11), spawn "vol_down")
           , ((0, 0x1008FF13), spawn "vol_up")
           , ((0, 0x1008FF12), spawn "mute_toggle")
           ]
           `additionalKeysP`
       [ ("M-b", runOrRaiseMaster "chromium" (propertyToQuery (Role "browser")))
       , ("M-r", spawn "dmenu_run")
       , ("M-d", runOrRaiseMaster "launchemacs" (className =? "Emacs"))
       , ("M-h", sendMessage Shrink)
       , ("M-i", sendMessage Expand)
       , ("M-n", windows W.focusDown)
       , ("M-e", windows W.focusUp)
       , ("M-S-n", windows W.swapDown)
       , ("M-S-e", windows W.swapUp)
       , ("M-c", spawn "emacsclient -a '' -n -e \"(make-capture-frame)\"")
       , ("<F12>", scratchpadSpawnActionCustom "sakura --name scratchpad")
       , ("<XF86Favorites>", spawn "sudo systemctl suspend")
       , ("M-s", sendMessage ToggleStruts)]
       
activeColor = xmobarColor "#429942"
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = activeColor "" . wrap "<" ">" 
                , ppTitle = activeColor "" . shorten 100
                , ppHidden = \tag -> if tag == "NSP" then "" else tag
                }

data FullscreenSaverOff a = FullscreenSaverOff [a] deriving (Show, Read)
instance LayoutModifier FullscreenSaverOff Window where
  handleMess fso@(FullscreenSaverOff fulls) m
    | Just (DestroyWindowEvent _ _ _ _ _ win) <- fromMessage m = let new_fulls = delete win fulls in do 
       spawn $ if null new_fulls then "saver-on" else "saver-off"
       return . Just $ FullscreenSaverOff new_fulls                     
    | Just ReleaseResources <- fromMessage m = do spawn "saver-on"; return Nothing
    | Just (AddFullscreen win) <- fromMessage m = return . Just $ FullscreenSaverOff $ nub $ win:fulls
    | Just (RemoveFullscreen win) <- fromMessage m = return . Just $ FullscreenSaverOff $ delete win $ fulls
    | Just FullscreenChanged <- fromMessage m = do spawn $ if null fulls then "saver-on" else "saver-off"; return $ Just fso
    | otherwise = return Nothing
fullscreenSaverOff = ModifiedLayout $ FullscreenSaverOff []
