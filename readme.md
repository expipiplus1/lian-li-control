# Lian-Li Control

A simple program to query and change pump settings for Lian-Li AIO pumps and
fan settings for the SL120 V2 0.5 fan controller.

WARNING: the commands this program sends are not fully understood, while it
hasn't bricked my devices yet, there are no guarantees that it won't
put a pump or fan controller into an unrecoverable state

## Pumps

Tested on the *Galahad II Performance* AIO. Reverse engineered with the help of
WireShark and a Windows VM. The L-Connect software seemed to be very unreliable
in this configuration (or maybe it's always like that).

```
$ lian-li-pump-control --help
Query and modify Lian-Li AIO coolers

Usage: lian-li-pump-control [--get-speed] [--set-speed pwm|INT]
                            [--set-color sync|rrggbb:rrggbb]
                            [--device STRING]... [--list-devices]

Available options:
  -h,--help                Show this help text
  --get-speed              Print the current speed of the pump
  --set-speed pwm|INT      Set the target pump speed to either the PWM input or
                           a specific speed between 2200 and 4200 RPM
  --set-color sync|rrggbb:rrggbb
                           Set the color to either sync to the RGB header or to
                           specific inner and outer colors
  --device STRING          A specific device to use, for example 5-10:1.2
  --list-devices           Print the paths of all recognized pumps
```

## Fans

Tested on the fan controller from the SL120 V2 3-pack. The commands for this
controller were cribbed from the uni-sync project here:
https://github.com/EightB1ts/uni-sync, thank you! 
 
```
$ lian-li-fan-control --help
Modify Lian-Li fan controllers

Usage: Main.hs [--set-speed pwm|INT] [--set-color sync|nosync]
               [--device STRING]... [--list-devices]

Available options:
  -h,--help                Show this help text
  --set-speed pwm|INT      Set the target fan speed to either the PWM input or a
                           specific speed between 0% and 100%
  --set-color sync|nosync  Set the color to either sync to the RGB header or to
                           specific inner and outer colors
  --device STRING          A specific device to use, for example 5-10:1.1
  --list-devices           Print the paths of all recognized pumps
```
