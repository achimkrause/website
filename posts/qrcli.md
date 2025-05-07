---
title: QR codes from the command line
date: 2025-05-07
---

It is 2025 and somehow we still don't have a completely satisfactory solution to quickly move small pieces of data between all our various devices. For files, we have various cloud storage options that work quite well, but I frequently find myself in the situation that I have a link on one device that I want to quickly move to another, or a wifi password that I have manually copy, or similar.

What I find surprisingly convenient is the Android feature of sharing wifi between phones through a QR code. You tap "share" in your wifi menu, hold the resulting QR code into the other persons phone camera, and they are connected. Inspired by this, I have sometimes resorted to use one of these online QR code generators on my computer, for example to get a zoom link from an ongoing meeting onto my tablet to connect that. These are full of ads and some of them have annoying UI, so this never quite felt like a good solution, even though it was reasonable effective.

In search for a better solution, I discovered today that there are actually useful command-line tools to generate and scan QR codes! These operate on a variety of output image formats, but what I found super cute is that `qrencode` comes with a variety of ASCII outputs.

![](/images/qrencode.png)

I got the best results with the UTF8 output format which cleverly arranges [block glyphs of various shapes](https://www.compart.com/en/unicode/block/U+2580) into a QR code. There is also a pure ASCII output format which uses `#` symbols for the pixels, but I had trouble getting that to actually scan on my phone.
This can be used in scripts, for example here is a quick script I hacked together to share the currently connected wifi as a QR code:

```bash
#!/bin/bash
SSID=$(iwctl station wlan0 show \
 | awk '/Connected network/ {print $NF}')
PASS=$(sudo cat "/var/lib/iwd/$SSID.psk" \
 | awk -F '=' '/^Passphrase/ {print $2}')
CONTENT="WIFI:T:WPA;S:$SSID;P:$PASS;;"
qrencode -t UTF8i $CONTENT
```

This follows the same format as the aforementioned Android feature. According to the [specification I found](https://github.com/zxing/zxing/wiki/Barcode-Contents#wi-fi-network-config-android-ios-11), there is also some escaping to worry about here if the SSID and passphrase contain special characters, but for now I didn't bother. The code used to read out the currently connected wifi and its passphrase do of course also make some assumptions on the ambient system, so this is probably not super portable in its current state.

In the other direction, we can use `zbar`. This is a collection of programs to scan QR codes from different image sources, with the most useful one of course being the webcam. By default, `zbarcam` opens a window showing the view from the webcam, and continuously outputs scanned QR codes. For use in scripts, it comes with the option `-1` which makes it so it waits until it has scanned a code, and then terminate, and an option `--nodisplay` which skips the window that mirrors the webcam view. I found the latter not so useful, since it is kind of hard to correctly align the QR code with the webcam blindly. We can turn this into a script to quickly share wifi from the phone to the computer.

~~~bash
#!/bin/bash
CODE=$(zbarcam -1)
SSID=$(echo "$CODE" | grep -oP 'S:\K[^;]+')
PASS=$(echo "$CODE" | grep -oP 'P:\K[^;]+')
iwctl --passphrase "$PASS" station wlan0 connect "$SSID"
~~~

Along similar lines, here is a script that turns the clipboard content into a QR code, and can also be used in the reverse direction when called with `-i`:

~~~bash
#/bin/bash!
input=false

while getopts "i" opt; do
  case $opt in
    i)
      input=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;; 
  esac
done

if $input; then
  zbarcam -1 | xclip -i -selection clipboard
else
  xclip -o -selection clipboard | qrencode -t UTF8i
fi
~~~
