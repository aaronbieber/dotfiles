#!/bin/bash
# Waybar custom module: report current light/dark theme (org.gnome.desktop.interface color-scheme)

SCHEME=$(gsettings get org.gnome.desktop.interface color-scheme | tr -d "'")

case "$SCHEME" in
    prefer-dark)
        printf '{"text":"\xf0\x9f\x8c\x99 Dark","class":"dark","tooltip":"Theme: dark\\nClick to switch to light"}\n'
        ;;
    prefer-light)
        printf '{"text":"\xe2\x98\x80 Light","class":"light","tooltip":"Theme: light\\nClick to switch to dark"}\n'
        ;;
    *)
        printf '{"text":"\xf0\x9f\x8c\x93 Auto","class":"auto","tooltip":"Theme: default (%s)\\nClick to switch to dark"}\n' "$SCHEME"
        ;;
esac
