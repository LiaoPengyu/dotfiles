#!/bin/bash

# Example notifier script -- lowers screen brightness, then waits to be killed
# and restores previous brightness on exit.

## CONFIGURATION ##############################################################

# Brightness will be lowered to this value.
min_brightness=5

# If your video driver works with xbacklight, set -time and -steps for fading
# to $min_brightness here. Setting steps to 1 disables fading.
fade_time=200
fade_steps=40

# If you have a driver without RandR backlight property (e.g. radeon), set this
# to use the sy5fs interface and create a .conf file in /etc/tmpfiles.d/
# containing the following line to make the sysfs file writable for group
# "users":
#
#     m /sys/class/backlight/acpi_video0/brightness 0664 root users - -
#
#sysfs_path=/sys/class/backlight/acpi_video0/brightness

###############################################################################

get_brightness() {
    xbacklight -get
}

set_brightness() {
    xbacklight -steps 1 -set $1
}

fade_brightness() {
    xbacklight -time $fade_time -steps $fade_steps -set $1
}

trap 'exit 0' TERM INT
trap "set_brightness $(get_brightness); kill %%" EXIT
fade_brightness $min_brightness
sleep 2147483647 &
wait
