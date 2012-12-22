xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps
dropboxd &
start-pulseaudio-x11 &
nm-applet &
xset -b &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x4B4B4B --height 18 &
sh ~/.fehbg &
mount-tray &
xmonad
