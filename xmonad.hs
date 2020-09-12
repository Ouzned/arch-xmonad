{- |
Module      :  xmonad.hs
Description :  Custom xmonad configuration for my arch linux laptop
Copyright   :  @Ouzned
License     :  GPL

Stability   :  unstable
Portability :  portable
-}

--import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.UpdatePointer
import XMonad.Core
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NoTaskbar
import XMonad.Util.Stack
import XMonad.Util.SpawnOnce
  
main = xmonad . fullscreenSupport . docks . dynamicProjects projects $ customConfig

-- |Name of the compiled xmonad binary
xmonadBin :: String
xmonadBin = "xmonad-x86_64-linux"

-- |Name of the terminal emulator
customTerminal :: String
customTerminal = "alacritty"

customWorkspaces :: [String]
customWorkspaces = ["web", "emacs", "mail"]

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
  , workspaces = customWorkspaces
  } `additionalKeysP` customKeysP

-- |Custom key bindings & overrides
customKeysP :: [(String, X ())]
customKeysP = 
  [ -- Applications
    ("M-a s", spawn "flameshot gui")
  , ("M-a f", spawn "firefox")
  , ("M-a t", spawn customTerminal)
  , ("M-a e", spawn "emacs")
  , ("M-a c", spawn "CM_LAUNCHER=rofi clipmenu")

    -- Go to projects
  , ("M-S-g", shiftToProjectPrompt projectXPConfig)
  , ("M-g", switchProjectPrompt projectXPConfig)

    -- Scratchpads
  , ("M-s k", namedScratchpadAction scratchpads "keepassxc")
  , ("M-s t", namedScratchpadAction scratchpads "term")
  , ("M-s w", namedScratchpadAction scratchpads "whatsapp")

    -- Special keys
  , ("<XF86MonBrightnessUp>", spawn "light -A 2")
  , ("<XF86MonBrightnessDown>", spawn "light -U 2")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- Azerty remapping
  , ("M-;", sendMessage (IncMasterN (-1)))
  , ("M-z", screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ("m-S-z", screenWorkspace 0 >>= flip whenJust (windows . W.shift))

    -- Misc
  , ("M-q", whenX (recompile False) (restart xmonadBin True))
  , ("M-o", spawn "xset s activate")
  , ("M-f", sendMessage $ Toggle FULL) 
  , ("M-d", spawn "rofi -show drun")
  , ("M-w", spawn "rofi -show window")
  , ("M-c c", kill)
  , ("M-t", toggleWS' [scratchpadWs])
  ]
  
-- |List of sequenced actions to execute when xmonad starts
-- ewmhDesktopStartup notify ewmh compliant programs that the
-- protocol is supported by xmonad
customStartupHook :: X ()
customStartupHook = ewmhDesktopsStartup <+> sequence_ 
  [ spawnOnce "picom"
  , spawnOnce "$HOME/.cabal/bin/taffybar-x86_64"
  , spawnOnce "$HOME/fehbg"
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
customEventHook = ewmhDesktopsEventHook

-- |Move the pointer to the center of the newly focused window.
-- Filter the NSP workspace from the list exposed through ewmh
customLogHook :: X ()
customLogHook = updatePointer (0.5, 0.5) (0.5, 0.5) <+> ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace

-- |Handle specific window placement. ManageHooks are applied right to left
-- 1: Apply scratchpad hooks
-- 2: Float dialog, splash and misbehaving windows
-- 3: Place foating window at the center of the screen
customManageHook :: ManageHook
customManageHook = composeAll 
    [ floats --> doCenterFloat
    , namedScratchpadManageHook scratchpads
    ]
  where 
    floats = foldr1 (<||>) 
        [ isDialog
        , isSplash
        , (title =? "win0" <||> title =? "win2") <&&> className =? "jetbrains-phpstorm" 
        ]

-- List of available layouts
customLayout = spaced . avoidStruts . mkToggle (single FULL) $
               tiled
               ||| Mirror tiled 
               ||| Grid
               ||| simpleTabbedAlways
  where
    tiled = Tall 1 (3/100) (1/2)
    spaced  = spacingRaw True (Border 0 0 0 0) True (Border 10 10 10 10) True
    
-- Style for the various prompts (ssh, dynamic projects...)
projectXPConfig :: XPConfig
projectXPConfig = def { font = "xft:Sans"
                      , position = CenteredAt 0.5 0.5
                      , height = 50
                      , bgColor           = "#262e3d"
                      , fgColor           = "#eeeeee"
                      , fgHLight          = "#ffffff"
                      , bgHLight          = "#c50ed2"
                      , borderColor       = "#0D1729"
                      , promptBorderWidth = 4
                      , maxComplRows = Just 3
                      , historySize = 0
                      }

-- |Definition of dynamic projects
projects :: [Project]
projects = 
  [ Project { projectName = "web"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "firefox"
            }
  , Project { projectName = "mail"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "thunderbird"
            }
  , Project { projectName = "emacs"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "emacs"
            }
  , Project { projectName = "php"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "phpstorm"
            }
  ]
  
-- Definition of named scratchpads
scratchpads = [ NS "keepassxc" "keepassxc" (className =? "KeePassXC") floatAndHide
              , NS "term" "alacritty --class scratchterm" (resource =? "scratchterm") floatAndHide
              , NS "whatsapp" whatsAppLauncher (className =? "whatsapp") floatAndHide
              ]
  where whatsAppLauncher = "firefox -P whatsapp --class=whatsapp --kiosk https://web.whatsapp.com"
        floatAndHide = doCenterFloat <+> noTaskbar

-- ----------------
-- Helper functions
-- ----------------

-- |Checks if a window is of type 'splash'
isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
