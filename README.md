# Packages to install

- X, DM, WM, Compositor: xorg lightdm lightdm-gtk-greeter xmonad xmonad-contrib picom
- Session Manager: lxsession
- Util: nemo gvim firefox alacritty zsh nitrogen htop xbindkeys bc neofetch xdotool
- Network: networkmanager network-manager-applet
- Font: ttf-freefont ttf-arphic-uming ttf-baekmuk ttf-liberation ttf-droid ttf-bitstream-vera ttf-croscore ttf-dejavu gnu-free-fonts noto-fonts noto-fonts-emoji ttf-linux-libertine ttf-joypixels ttf-font-awesome otf-font-awesome terminus-font
- Audio: alsa-utils alsa-plugins sof-firmware alsa-ucm-conf pulseaudio pulseaudio-alsa pulseaudio-bluetooth pulseaudio-jack volumeicon
- Bluetooth: bluez bluez-utils blueman
- Vedio: mesa xf86-video-intel xf86-input-libinput
- Power Management: tlp tlp-rdw powertop
- Hard Drive Health: smartmontools
- Night light: redshift geoclue2
- Notification: notification-daemon dunst gnome-icon-theme adwaita-icon-theme

# Setups

## Login

Use LightDM as Display manager for login, LXsession for Session manager which will be used for launching Xmonad (or whatever WM). To do this:

1. Enable Display Manager: `systemctl enable lightdm`
2. Create a LXsession session for DM to launch:

	# Copy & Modify the Xmonad session.
	sudo cp /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/lxsession.desktop
	# Or just create a new one with the content:
	[Desktop Entry]
	Name=Xmonad-LXsession
	Comment=Xsession for launching WM
	Exec=lxsession
	Icon=xmonad
	Type=Xsession

The benefit of this setup is so that having LXsession as a session manager, which manages session separatly. Especially, LXsession support XDG autostart, so one could manage all the autostart tasks in unformed ~/.config/autostart instead of putting into Xinitrc, Xsession, Xprofile etc.

## Network Manager: 

Systemd provides a `systemd-networkd` by default, but NetworkManager does more stuff, which is good to avoid some hassles.

	systemctl enalbe --now NetworkManager
	systemctl enable --now NetworkManager-dispatcher

## System tray and status bar

Polybar, provides systray + status bar, beautiful and highly customizable.

	# copy default config and edit as needed.
	cp /usr/share/doc/polybar/config $XDG_CONFIG_HOME/polybar/config

## Night Light

redshift, prob the de facto for "night light" if not running a DE.

	# Download [sample](https://raw.githubusercontent.com/jonls/redshift/master/redshift.conf.sample) and modify as needed
	# Use systemctl to autostart
	systemctl --user enable --now redshift

## Power Manager

There are other alternatives (LMT, ACPI), TLP is my choice. Additionally install `powertop` for power consumption analysis :)

	systemctl enable --now tlp

## Keybdings

Surprisely I did not find a *native* way to handle XF86 keys. From wiki, the way to do it is capture the key event and spawn a command:

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
