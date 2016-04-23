~/.fehbg
setxkbmap -option 'ctrl:nocaps'
xmodmap -e "keycode 66 = Control_L"
xmodmap -e "add Control = Control_L"
xmodmap -e "keycode 999 = Escape"
xmodmap -e "keycode 36 = 0x1234"
xmodmap -e "add Control = 0x1234"
xmodmap -e "keycode any = Return"
xcape -e 'Shift_L=parenleft;Shift_R=parenright;Control_L=Escape;0x1234=Return'

eval `dbus-launch --auto-syntax`
eval `gpg-agent --daemon`

xcompmgr &
dropbox &
redshift &
~/.cabal/bin/taffybar &
pasystray &
blueberry-tray &
redshift &

xsetroot -cursor_name left_ptr

# bluetooth power on
dbus-send --system --type=method_call --print-reply=literal --dest=org.bluez /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true

exec xmonad
