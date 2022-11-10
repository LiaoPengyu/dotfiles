#!/bin/sh
controller="$(bluetoothctl show)"
if ! [[ "$controller" =~ .*"Powered: yes".* ]]; then
    echo "<fc=#616161> </fc>"
elif [[ "$controller" =~ .*"Discovering: yes".* ]]; then
    echo " "
elif [[ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]]; then
    echo " "
else
    echo "<fc=#2193ff> </fc>"
fi

