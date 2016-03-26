setxkbmap -option 'ctrl:nocaps'
setxkbmap -option 'compose:menu'
xcape -e 'Shift_L=parenleft;Shift_R=parenright;Control_L=Escape'
eval $(~/bin/gpg-agent)
dropbox &
exec stumpwm
