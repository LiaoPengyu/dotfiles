# Packages to install

- X, DM, WM, Compositor: xorg lightdm lightdm-gtk-greeter xmonad xmonad-contrib picom
- Util: nemo neovim firefox alacritty zsh nitrogen htop xbindkeys bc neofetch xdotool scrot
- Network: networkmanager network-manager-applet
- Font: ttf-freefont ttf-arphic-uming ttf-baekmuk ttf-liberation ttf-droid ttf-bitstream-vera ttf-croscore ttf-dejavu gnu-free-fonts noto-fonts noto-fonts-emoji ttf-linux-libertine ttf-joypixels ttf-font-awesome otf-font-awesome terminus-font
- Fonts Explorer: gucharmap
- Audio: alsa-utils alsa-plugins sof-firmware alsa-ucm-conf pulseaudio pulseaudio-alsa pulseaudio-bluetooth pulseaudio-jack volumeicon
- Bluetooth: bluez bluez-utils blueman
- Video: mesa xf86-video-intel xf86-input-libinput
- Power Management: tlp tlp-rdw powertop
- Hard Drive Health: smartmontools
- Night light: redshift geoclue2
- Notification: notification-daemon dunst gnome-icon-theme adwaita-icon-theme



![Screenshot1](https://raw.github.com/LiaoPengyu/dotfiles/master/screenshots/screenshot1.png)
![Screenshot1](https://raw.github.com/LiaoPengyu/dotfiles/master/screenshots/screenshot2.png)

# Setups

## Login

Use LightDM as Display manager for login, Xmonad as session. To do this:

1. Enable Display Manager: `systemctl enable lightdm`
2. Create a Xmonad session for DM to launch:

        # Copy & Modify the Xmonad session.
        > sudo vim /usr/share/xsessions/xmonad.desktop
        [Desktop Entry]
        Name=Xmonad
        Comment=Xmonad
        Exec=xmonad
        Icon=xmonad
        Type=Xsession

Put autostarts in Xprofile.

## Network Manager: 

Systemd provides a `systemd-networkd` by default, but NetworkManager does more stuff, which is good to avoid some hassles.

	systemctl enalbe --now NetworkManager
	systemctl enable --now NetworkManager-dispatcher

## Status bar

Polybar, provides systray + status bar. Alternative Xmobar is more lightweight.

	# copy default config and edit as needed.
	cp /usr/share/doc/polybar/config $XDG_CONFIG_HOME/polybar/config

*UPDATE: I found that Xmobar could do everything I want do with Polybar. So I switched to Xmobar but copied lots scripts for themes.*

## Night Light

redshift, prob the de facto for "night light" if not running a DE.

	# Download [sample](https://raw.githubusercontent.com/jonls/redshift/master/redshift.conf.sample) and modify as needed
	# Use systemctl to autostart
	systemctl --user enable --now redshift

## Power Manager

There are other alternatives (LMT, ACPI), TLP is my choice. Additionally install `powertop` for power consumption analysis :)

	systemctl enable --now tlp

## Keybdings

Use xbindkeys to capture the key event and spawn a command:

	pacman -S xbindkeys

## Audio

Install alsa, pulseaudio, volumeicon (for tray icon), driver etc. My latop needs kernel 5.10+ to work properly

## Bluetooth

Intall blueman which come with blueman-applet for systray

## Time Sync

`systemctl enable --now systemd-timesyncd`

## DPMS:

Create/Edit `/etc/X11/xorg.conf.d/10-monitor.conf`. Swap is required for Suspend.

	Section "ServerFlags"
	    Option "StandbyTime" "2"
   	    Option "SuspendTime" "4"
   	    Option "OffTime" "8"
   	    Option "BlankTime" "0"
	EndSection

## Touchpad

Create/Edit `/etc/X11/xorg.conf.d/30-touchpad.conf`

	Section "InputClass"
   	     Identifier "libinput touchpad catchall"
	     MatchIsTouchpad "on"
    	     MatchDevicePath "/dev/input/event*"
             Option "Tapping" "True"
	     Option "TappingDrag" "True"
	     Option "ClickMethod" "clickfinger"
	     Option "NaturalScrolling" "True"
       	     Driver "libinput"
	EndSection

## Keyboard Layout

For X11 to swap caps and control: 
`localectl set-x11-keymap us,us pc86 ,dvorak ctrl:swapcaps`

## Misc

Create soft links:

`sudo ln -s `which xdg-open` /usr/bin/open`

`sudo ln -s `which nvim` /usr/bin/vim`
