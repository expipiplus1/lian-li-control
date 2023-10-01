# Lian-Li Pump Control

A simple program to query and change pump settings for Lian-Li AIO pumps.

Tested on the *Galahad II Performance* AIO.

```
$ lian-li-pump-control --help
Query and modify Lian-Li AIO coolers

Usage: lian-li-pump-control [--get-speed] [--set-speed pwm|INT]
                            [--set-color sync|rrggbb:rrggbb]
                            [--device STRING]... [--list-devices]

  WARNING: the commands this program sends are not fully understood, while it
  hasn't bricked my pump (yet), there are no guarantees that it won't put a pump
  into an unrecoverable state

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
