~/.fehbg

~/bin/setup-keys &
xcompmgr &
#dropbox &
/home/edd/bin/taffybar &
#blueberry-tray &
caffeine &
/home/edd/.local/bin/brainzo-api &
xsetroot -cursor_name left_ptr


dbus-send --system --type=method_call --print-reply=literal --dest=org.bluez /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true

pasystray &
exec xmonad
