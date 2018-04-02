{-# LANGUAGE DeriveDataTypeable #-}
import Data.List(intersperse)
import Data.List.Split(splitOn)
import Data.Char (toUpper)
import qualified Data.Map as M
import XMonad
import XMonad.Actions.PhysicalScreens (sendToScreen)
import XMonad.Actions.WindowGo (raiseNextMaybe, className, (<||>))
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.StackSet(focusDown, swapMaster)
import XMonad.Util.EZConfig
import XMonad.Util.Run

--import qualified Brainzo
--import qualified Brainzo.Commands.Audio as BA
--import qualified Brainzo.Commands.Radio as BR
--import qualified Turtle as T

titleCase :: String -> String
titleCase s = (toUpper . head) s : tail s

showMe  :: String -> [String] -> X ()
showMe s as =
  let
    titleSpaceCase = concat . intersperse " " . fmap titleCase . splitOn "-"
  in do
    raiseNextMaybe (safeSpawn s as) (className =? s <||> className =? titleSpaceCase s)
    windows swapMaster

showMeEmacs :: X ()
showMeEmacs = do
  raiseNextMaybe runEmacs findEmacs
  windows swapMaster
  where
    runEmacs = safeSpawn "emacsclient" ["-c", "-a", "emacs"]
    findEmacs = className =? "Emacs"

run  :: String -> X ()
run s = safeSpawn s []

-- shell :: T.Shell a -> X ()
-- shell  = liftIO . T.sh

-- until we work out the problem with turtle, just run brainzo from a shell
brainzo :: [String] -> X ()
brainzo = unsafeSpawn . unwords . (:) "~/.local/bin/b"


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
  spawn . concat $ "emacsclient " : sfx o
    where sfx (Edit s) = ["-n ", s]
          sfx (SudoEdit s) = ["-n ", "/sudo::", s]
          sfx (Execute s) = ["-e ", s]

data ScreenOp = Enlighten Int | Endarken Int
light :: ScreenOp -> X()
light e = unsafeSpawn $ "light " ++ l e ++ "; ~/bin/brightness | ~/bin/osd"
  where l (Enlighten i) = "-A " ++ show i
        l (Endarken i)  = "-U " ++ show i

layout =
  avoidStruts . mkToggle (single NBFULL)
  $ tallSplit ||| Full ||| evenSplit ||| Mirror tallSplit
  where tallSplit = Tall 1 (3/100) (4/7) -- emacs at 3/7 width ~= 80 columns
        evenSplit = Tall 1 (3/100) (1/2)

toggleFS = do
  sendMessage (Toggle NBFULL)
  sendMessage ToggleStruts

extraKeys = (flip mkKeymap)
  [ ("S-M-e", showMeEmacs)
  , ("S-C-e", run "~/.emacs_anywhere/bin/run")                       -- %! Edit selection in Emacs
  , ("S-M-w", showMe "firefox-developer-edition" [])                 -- %! Show chromium
  , ("S-M-g", showMe "firefox" ["www.netflix.com"])                  -- %! Show chrome (multimedia extensions)
  , ("S-M-k", showMe "kodi" [])
  , ("S-M-p", run "dmenu_run")                                       -- %! Run dmenu_run
  , ("M-w", kill)                                                    -- %! Close current window
  , ("S-M-f", toggleFS)                                              -- %! Toggle fullscreen
  , ("S-M-<Escape>", sendMessage ToggleStruts)                       -- %! Toggle panel display
  , ("M-`", windows focusDown)                                       -- %! Move focus to the next window
  , ("S-C-M-<Left>", sendToScreen 0)                                 -- %! Move focused window to previous display
  , ("S-C-M-<Right>", sendToScreen 1)                                -- %! Move focused window to next display
  -- FUNCTION KEYS
  -- audio
  , ("<XF86AudioMute>",          brainzo ["audio", "mute"])          -- %! Mute/Unmute sound
  , ("C-<XF86AudioMute>",        brainzo ["audio", "mixer"])         -- %! Pulse Audio Mixer
  , ("<XF86AudioRaiseVolume>",   brainzo ["audio", "louder", "10"])  -- %! Increase sound volume
  , ("C-<XF86AudioRaiseVolume>", brainzo ["audio", "mixer"])         -- %! Pulse Audio Mixer
  , ("<XF86AudioLowerVolume>",   brainzo ["audio", "quieter", "10"]) -- %! Decrease sound volume
  , ("C-<XF86AudioLowerVolume>", brainzo ["audio", "mixer"])         -- %! Pulse Audio Mixer
  , ("<XF86AudioMicMute>",       brainzo ["audio", "micmute"])       -- %! Mute/Unmute mic

  -- brightness
  , ("<XF86MonBrightnessDown>", light (Endarken 9))                  -- %! Decrease brightness
  , ("<XF86MonBrightnessUp>", light (Enlighten 9))                   -- %! Increase brightness
  , ("C-<XF86MonBrightnessDown>", light (Endarken 1))                -- %! Decrease brightness by 1
  , ("C-<XF86MonBrightnessUp>", light (Enlighten 1))                 -- %! Increase brightness by 1
  -- display toggle
  , ("C-<XF86Display>", unsafeSpawn "~/bin/tootch.sh")               -- %! Toggle bluetooth
  -- wireless toggle seems to be hardware-level
  -- settings
  , ("<XF86Tools>", emacs (Edit "~/txt/gtd/projects.org"))           -- %! Jump to GTD
  , ("S-<XF86Tools>", emacs (Edit "~/.xmonad/xmonad.hs"))            -- %! Edit WM configuration
  , ("S-C-<XF86Tools>", emacs (Edit "~/.emacs.d/init.el"))           -- %! Edit Editor configuration

  -- search
  , ("<XF86Search>",         brainzo ["radio", "seek"])              -- %! Seek radio
  , ("S-<XF86Search>",       brainzo ["radio", "kees"])              -- %! Reverse seek radio
  , ("C-<XF86Search>",       brainzo ["radio", "np"])                -- %! Display radio now--playing
  , ("S-C-<XF86Search>",      brainzo ["radio", "off"])               -- %! Turn off radio

  -- window list
  , ("<XF86LaunchA>", unsafeSpawn "~/bin/batteries | ~/bin/osd")     -- %! Show battery state
  , ("M-r", shellPrompt promptConfig)                                -- %! Shell prompt
  , ("C-M-r", prompt "~/.local/bin/b" promptConfig)                  -- %! Brainzo prompt
  , ("S-M-r", xmonadPrompt promptConfig)                             -- %! Xmonad prompt
  -- exposé
  , ("<XF86Explorer>", brainzo ["np", "display"])                    -- %! Show what's playing
  , ("<Print>", screenshot All)                                      -- %! Take a screenshot
  , ("S-<Print>", screenshot Sel)                                    -- %! Screenshot window or rectangle
  , ("C-S-<Pause>", safeSpawn "systemctl" ["suspend"])               -- %! suspend
  , ("<XF86Sleep>", safeSpawn "systemctl" ["suspend"])               -- %! suspend
  ]

promptConfig = def
  { fgColor = "#ccc"
  , bgColor = "#222"
  , promptBorderWidth = 0
  , position = Bottom
  , height = 30
  , font = "xft:Droid Sans Mono-12"
  }

fading = composeAll
  [isUnfocused                                   --> transparency 0.1
  , className =? "google-chrome"                 --> opaque
  , className =? "firefox"                       --> opaque
  , className =? "vlc" <||> className =? "cvlc"  --> opaque
  , className =? "mplayer"                       --> opaque
  , className =? "Kodi"                          --> opaque
  , className =? "xtailjournal"                  --> opaque
  , fmap not isUnfocused                         --> opaque
  ]

homeDesktops = composeAll
   [ className =? "Kodi" --> doShift "4"
   , className =? "xtailjournal" --> doShift "9"
   ]


main :: IO ()
main = do
  -- b <- Brainzo.birth
  let mykeys x = extraKeys x `M.union` keys def x -- extraKeys b x `M.union` keys def x
  let conf = def
             { modMask = mod4Mask
             , borderWidth = 2
             , normalBorderColor  = "#333333"
             , focusedBorderColor = "#777777"
             , keys = mykeys
             , layoutHook = layout
             , manageHook = homeDesktops
             , logHook = fadeWindowsLogHook fading
             , handleEventHook = fadeWindowsEventHook
             }
  xmonad . ewmh . docks $ conf
