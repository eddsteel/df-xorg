{-# LANGUAGE DeriveDataTypeable #-}
import XMonad

import Data.List(intersperse)
import Data.Char(isNumber)
import DBus.Client
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.WindowGo (raiseNextMaybe, className, (<||>))
import XMonad.Actions.PhysicalScreens (sendToScreen)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Char (toUpper)

titleCase :: String -> String
titleCase s = (toUpper . head) s : tail s

showMe  :: String -> [String] -> X ()
showMe s as = do
  raiseNextMaybe (safeSpawn s as) (className =? titleCase s <||> className =? s)
  windows W.swapMaster

showMeEmacs :: X ()
showMeEmacs = do
  raiseNextMaybe spawn find
  windows W.swapMaster
  where
    spawn = safeSpawn "emacsclient" ["-c", "-a", "emacs"]
    find = className =? "Emacs"

run  :: String -> X ()
run s = safeSpawn s []

-- Some X
data SShot = All | Sel
screenshot :: SShot -> X ()
screenshot t = unsafeSpawn $ "sleep 1; scrot " ++ s t ++ "'%F-%s.png' -e 'mv $f ~/tmp'"
  where s All = ""
        s Sel = "-s "


data Emacs = Edit String | SudoEdit String | Execute String
emacs :: Emacs -> X ()
emacs o = do
  showMeEmacs
  (spawn . concat) $ "emacsclient " : sfx o
    where sfx (Edit s) = ["-n ", s]
          sfx (SudoEdit s) = ["-n ", "/sudo::", s]
          sfx (Execute s) = ["-e ", s]

data ScreenOp = Enlighten Int | Endarken Int
light :: ScreenOp -> X()
light e = unsafeSpawn $ "light " ++ l e ++ "; ~/bin/brightness | ~/bin/osd"
  where l (Enlighten i) = "-A " ++ show i
        l (Endarken i)  = "-U " ++ show i

data AudioOp = Louder Int | Quieter Int | MuteToggle | Mixer | MicToggle
pa :: AudioOp -> X ()
pa Mixer = safeSpawn "pavucontrol" []
pa MicToggle = safeSpawn "pactl" ["set-source-mute", "2", "toggle"] -- just going to hardcode this mofo
pa op =
  let
    toStrings (Louder n)  = ("set-sink-volume", ('+':(show n)) ++ "%")
    toStrings (Quieter n) = ("set-sink-volume", ('-':(show n)) ++ "%")
    toStrings MuteToggle  = ("set-sink-mute", "toggle")
    (command, effect) = toStrings op
  in do
    mixOut <- runProcessWithInput "pactl" ["list", "short", "sinks"] []
    let mixer = (takeWhile isNumber . last . lines) mixOut
    safeSpawn "pactl" [command, mixer, effect]

layout = id
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT)
         $ avoidStruts (tiled  ||| full ||| Mirror tiled ||| even ||| Mirror even)
  where
    full = noBorders Full
    tiled = noBorders $ Tall 1 (3/100) (4/7) -- emacs at 3/7 width ~= 80 columns
    even = noBorders $ Tall 1 (3/100) (1/2)

extraKeys conf = mkKeymap conf
  [ ("S-M-e", showMeEmacs)
  , ("S-M-w", showMe "chromium" [])                                      -- %! Show chromium
  , ("S-M-g", showMe "google-chrome" ["www.netflix.com"])                -- %! Show chrome (multimedia extensions)
  , ("S-M-p", run "dmenu_run")                                           -- %! Run dmenu_run
  , ("M-c", kill)                                                        -- %! Close current window
  , ("S-M-f", sendMessage (Toggle FULL))                                 -- %! Toggle fullscreen
  , ("M-`", windows W.focusDown)                                         -- %! Move focus to the next window
  , ("S-M-<Up>", sendToScreen 0)
  , ("S-M-<Down>", sendToScreen 1)
  -- FUNCTION KEYS
  -- audio
  , ("<XF86AudioMute>", pa MuteToggle)                                   -- %! Mute/Unmute sound
  , ("C-<XF86AudioMute>", pa Mixer)                                      -- %! Pulse Audio Mixer
  , ("<XF86AudioRaiseVolume>", pa (Louder 10))                           -- %! Increase sound volume
  , ("C-<XF86AudioRaiseVolume>", pa Mixer)                               -- %! Pulse Audio Mixer
  , ("<XF86AudioLowerVolume>", pa (Quieter 10))                          -- %! Decrease sound volume
  , ("C-<XF86AudioLowerVolume>", pa Mixer)                               -- %! Pulse Audio Mixer
  , ("<XF86AudioMicMute>", pa MicToggle)                                 -- %! Mute/Unmute mic
  -- brightness
  , ("<XF86MonBrightnessDown>", light (Endarken 9))                      -- %! Decrease brightness
  , ("<XF86MonBrightnessUp>", light (Enlighten 9))                       -- %! Increase brightness
  , ("C-<XF86MonBrightnessDown>", light (Endarken 1))                    -- %! Decrease brightness by 1
  , ("C-<XF86MonBrightnessUp>", light (Enlighten 1))                     -- %! Increase brightness by 1
  -- display toggle
  , ("C-<XF86Display>", unsafeSpawn "~/bin/tootch.sh toggle")              -- %! Toggle bluetooth
  -- wireless toggle seems to be hardware-level
  -- settings
  , ("<XF86Tools>", emacs (SudoEdit ""))     -- %! Edit OS configuration
  , ("S-<XF86Tools>", emacs (Edit "~/.xmonad/xmonad.hs"))                -- %! Edit WM configuration
  , ("S-C-<XF86Tools>", emacs (Edit "~/.emacs.d/init.el"))               -- %! Edit Editor configuration
  -- search
  , ("<XF86Search>", unsafeSpawn "~/.local/bin/b radio seek; sleep 0.2; ~/.local/bin/b radio np | ~/bin/osd")
  , ("S-<XF86Search>", unsafeSpawn "~/.local/bin/b radio kees; sleep 0.2; ~/.local/bin/b radio np | ~/bin/osd")
  , ("C-<XF86Search>", unsafeSpawn "~/.local/bin/b radio np | ~/bin/osd")
  , ("S-C-<XF86Search>", unsafeSpawn "~/.local/bin/b radio off")

  -- window list
  , ("<XF86LaunchA>", unsafeSpawn "~/bin/batteries | ~/bin/osd")                 -- %! Show battery state

  , ("M-r", shellPrompt promptConfig)                                    -- %! Shell prompt
  , ("C-M-r", prompt "~/.local/bin/b" promptConfig)                      -- %! Brainzo prompt
  , ("S-M-r", xmonadPrompt promptConfig)                                 -- %! Xmonad prompt
    -- TODO google prompt

  -- exposé
  , ("<XF86Explorer>", emacs (Edit "~/txt/gtd/projects.org"))            -- %! Edit projects org file

  , ("<Print>", screenshot All)                                          -- %! Take a screenshot
  , ("S-<Print>", screenshot Sel)                                        -- %! Screenshot window or rectangle

  , ("C-S-<Pause>", unsafeSpawn "systemctl suspend")                       -- %! suspend
  ]

promptConfig = defaultXPConfig
               { fgColor = "#ccc"
               , bgColor = "#222"

               , promptBorderWidth = 0
               , position = Bottom
               , height = 30
               , font = "xft:Droid Sans Mono-12"
               }

fading = composeAll [isUnfocused                   --> transparency 0.1
                    , className =? "google-chrome" --> opaque
                    , className =? "vlc"           --> opaque
                    , className =? "Kodi"          --> opaque

                    , fmap not isUnfocused         --> opaque
                    ]

homeDesktops = composeAll
   [ className =? "Kodi" --> doShift "4"]

main = do
  client <- connectSession
  let pp = defaultPP
  xmonad $
    ewmh $
    defaultConfig
    { modMask = mod4Mask
    , normalBorderColor  = "#777777"
    , focusedBorderColor = "#000000"
    , keys = \x -> extraKeys x `M.union` keys defaultConfig x
    , layoutHook = layout
    , manageHook = homeDesktops <+> manageDocks
    , logHook = fadeWindowsLogHook fading
    , handleEventHook = fadeWindowsEventHook
    }
