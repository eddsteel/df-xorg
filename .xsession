export PATH=$PATH:/home/edd/bin:/home/edd/.local/bin
export EDITOR=emacsclient

/home/edd/.fehbg &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

/home/edd/bin/setup-keys &
/usr/bin/xcompmgr &
#dropbox &
/home/edd/bin/taffybar &
#blueberry-tray &
/usr/bin/caffeine &
/home/edd/.local/bin/brainzo-api &
/usr/bin/xsetroot -cursor_name left_ptr

/usr/bin/dbus-send --system --type=method_call --print-reply=literal --dest=org.bluez /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true

/usr/bin/pasystray &

/home/edd/bin/xtailjournal.sh &
/home/edd/go/bin/consul watch -type=key -key=now-playing ~/.local/bin/b np display

exec /usr/bin/xmonad
