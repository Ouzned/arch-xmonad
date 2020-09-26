{- |
Module      :  xmonad.hs
Description :  Custom xmonad configuration for my arch linux laptop
Copyright   :  @Ouzned
License     :  GPL

Stability   :  unstable
Portability :  portable
-}

import Data.Monoid
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.UpdatePointer
import XMonad.Config.Azerty
import XMonad.Core
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen 
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NoTaskbar
import XMonad.Util.SpawnOnce
import XMonad.Util.Stack

main = xmonad
  . docks
  . fullscreenSupport
  $ customConfig

-- |Name of the compiled xmonad binary
xmonadBin :: String
xmonadBin = "xmonad-x86_64-linux"

-- |Name of the terminal emulator
customTerminal :: String
customTerminal = "alacritty"

-- |Name of the scratchpad workspace
scratchpadWs :: String
scratchpadWs = "NSP"
  
-- |XMonad main configuration structure
customConfig = def
  { terminal = customTerminal
  , borderWidth = 4
  , modMask = mod5Mask
  , layoutHook = customLayout
  , manageHook = customManageHook
  , handleEventHook = customEventHook
  , logHook = customLogHook
  , startupHook = customStartupHook
  , focusedBorderColor = "#2a9d8f"
  , normalBorderColor = "#264653"
  }
  `additionalKeysP` customKeysP
  `additionalKeys` (customWsKeys def)

-- |Custom key bindings & overrides
customKeysP :: [(String, X ())]
customKeysP = 
  [ -- Applications
    ("M-a s", spawn "flameshot gui")
  , ("M-a f", spawn "firefox")
  , ("M-a t", spawn customTerminal)
  , ("M-a e", spawn "emacsclient -c -n -e '(switch-to-buffer nil)'")
  , ("M-a c", spawn "CM_LAUNCHER=rofi clipmenu")

    -- Empty workspaces functions
  , ("M-S-m", tagToEmptyWorkspace)
  , ("M-m", viewEmptyWorkspace)

    -- Scratchpads
  , ("M-s k", namedScratchpadAction scratchpads "keepassxc")
  , ("M-s t", namedScratchpadAction scratchpads "term")

    -- Special keys
  , ("<XF86MonBrightnessUp>", spawn "light -A 2")
  , ("<XF86MonBrightnessDown>", spawn "light -U 2")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- Misc
  , ("M-q", whenX (recompile False) (restart xmonadBin True))
  , ("M-o", spawn "xset s activate")
  , ("M-f", sendMessage $ Toggle FULL) 
  , ("M-d", spawn "rofi -show drun")
  , ("M-g", spawn "rofi -show window")
  , ("M-c c", kill)
  , ("M-t", toggleWS' [scratchpadWs])
  ]

-- |Use the super key for workspace navigation because altgr + top row
-- is already used for special characters
customWsKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
customWsKeys conf = 
    [((mod4Mask, xK_semicolon), sendMessage (IncMasterN (-1)))]
    ++
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) topRow,
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{z,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{z,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    where topRow = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] 
    
-- |List of sequenced actions to execute when xmonad starts
-- ewmhDesktopStartup notify ewmh compliant programs that the
-- protocol is supported by xmonad
customStartupHook :: X ()
customStartupHook = ewmhDesktopsStartup <+> sequence_ 
  [ spawnOnce "picom"
  , spawnOnce "taffybar-x86_64"
  , spawnOnce "$HOME/.fehbg"
  , spawnOnce "libinput-gestures"
  , spawnOnce "clipmenud"
  , spawnOnce "nextcloud"
  , spawnOnce "nm-applet --sm-disable --indicator"
  , spawnOnce "blueman-tray"
  , spawnOnce "xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock"
  ]

-- |List of custom event hooks
-- ewmhDesktopsEventHook handles external events related to the
-- ewmh protocol
customEventHook :: Event -> X All
customEventHook = ewmhDesktopsEventHook

-- |Move the pointer to the center of the newly focused window.
-- Filter the NSP workspace from the list exposed through ewmh
customLogHook :: X ()
customLogHook = updatePointer (0.5, 0.5) (0.5, 0.5)
  <+> ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace

-- |Handle specific window placement. ManageHooks are applied right to left
-- 1: Apply scratchpad hooks
-- 2: Handle Firefox PictureInPicture window
-- 3: Float splash / dialogs at the center of the screen
customManageHook :: ManageHook
customManageHook = composeAll 
    [ floats --> doCenterFloat
    , isPictureInPicture --> doSideFloat SE <+> doCopyToAll
    , namedScratchpadManageHook scratchpads
    ]
  where 
    floats = foldr1 (<||>) 
        [ isDialog
        , isSplash
        , (title =? "win0" <||> title =? "win2") <&&> className =? "jetbrains-phpstorm" 
        ]
    doCopyToAll = do
      w <- ask
      doF (\ws -> foldr (copyWindow w) ws $ currentWorkspaces ws)
      
-- |List of available window layouts with modifiers
customLayout = smartBorders
  . avoidStruts
  . spaced
  . mkToggle (single FULL)
  $ tiled
  ||| Grid
  ||| tabbed shrinkText tabConfig
  where
    tiled = Tall 1 (3/100) (1/2)
    spaced = spacingRaw True (Border 0 0 0 0) True (Border 10 10 10 10) True
  
-- Definition of named scratchpads
scratchpads :: [NamedScratchpad]
scratchpads = [ NS "keepassxc" "keepassxc" (className =? "KeePassXC") floatAndHide
              , NS "term" "alacritty --class scratchterm" (resource =? "scratchterm") floatAndHide
              ]
  where floatAndHide = doRectFloat (W.RationalRect (1/8) (1/8) (3/4) (3/4)) <+> noTaskbar


-- --------------------------------------------
-- Misc themes
-- --------------------------------------------

-- |Configuration for the tabbed layout
tabConfig :: Theme
tabConfig = def { fontName = "xft:Sans"
                , decoHeight = 35
                }

-- -------------------------------------------
-- Helper functions
-- -------------------------------------------

-- |Checks if a window is of type 'splash'
isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isPictureInPicture :: Query Bool
isPictureInPicture = title =? "Picture-in-Picture"

currentWorkspaces :: W.StackSet i l a s sd -> [i]
currentWorkspaces = map W.tag . W.workspaces

