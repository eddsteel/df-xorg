import System.Taffybar

import Graphics.UI.Gtk (escapeMarkup)

import System.Process(readProcess)
import System.Taffybar.Battery
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.NetMonitor
import System.Taffybar.Weather
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU
import System.Information.Network

import Text.Printf (printf)

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

ff j = j / 255

coral a = (1, ff 96, ff 64, a)
green a = (ff 96, 1, ff 64, a)
blue a  = (ff 128, 1, ff 64, a)
white a = (1, 1, 1, a)

colorSpan c s = "<span fgcolor='" ++ c ++ "'>" ++ s ++ "</span>"

weatherConfig = (defaultWeatherConfig "CYVR") {
    weatherTemplate = colorSpan "#999999" "$weather$ $skyCondition$, $tempC$ °C, $weather$"
  }

clock = textClockNew
        Nothing
        (colorSpan "#FF6040" "%a %b %_d %H:%M")
        1

pager = taffyPagerNew cfg
  where cfg = defaultPagerConfig {
            activeWindow    = colorSpan "#80FF40"
          , activeWorkspace = colorSpan "#40ddff"
          , emptyWorkspace  = colorSpan "#999999"
          , widgetSep       = " ◦ "
          }

cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  where cpuCfg = defaultGraphConfig {
            graphDataColors = [green 1 , green 0.5]
          , graphLabel = Just "cpu "
          }

mem = pollingGraphNew memCfg 1 memCallback
  where  memCfg = defaultGraphConfig {
             graphDataColors = [coral 1]
           , graphLabel = Just "mem "
           }

net = netMonitorNew 5 "wlp3s0"

bt = pollingLabelNew "bt" 1.0 checkBluetooth
  where checkBluetooth = (readProcess "tootch.sh" ["status"] [])

dummyL = pollingLabelNew "yo" 1.0 dummy
  where dummy = return "yo"

batterybar v
  | v > 0.9   = (1, 1, 1)
  | v > 0.1   = (ff 153, ff 153, ff 153)
  | otherwise = (1, ff 96, ff 64)

battery = batteryBarNew config 60.0
  where config = defaultBatteryConfig {
            barBackgroundColor = batterybar
          }

main = let
    note = notifyAreaNew defaultNotificationConfig
    wea = weatherNew weatherConfig 10
    mpris = mprisNew defaultMPRISConfig
    tray = systrayNew
  in do
      defaultTaffybar defaultTaffybarConfig {
        startWidgets = [pager]
        , endWidgets = [dummyL, tray, clock, wea, mem, cpu, battery, mpris, note, net] --, bt]
        }
