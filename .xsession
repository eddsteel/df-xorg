~/.fehbg
~/bin/setup-keys &

eval `dbus-launch --auto-syntax`
eval `gpg-agent --daemon`

xcompmgr &
dropbox &
redshift &
~/.cabal/bin/taffybar &
pasystray &
blueberry-tray &
redshift-gtk &
caffeine &

xsetroot -cursor_name left_ptr

# bluetooth power on
dbus-send --system --type=method_call --print-reply=literal --dest=org.bluez /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true

exec xmonad
