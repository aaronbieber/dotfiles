#!/bin/bash
# Waybar custom module: toggle light/dark theme (org.gnome.desktop.interface color-scheme + gtk-theme)

CURRENT=$(gsettings get org.gnome.desktop.interface color-scheme | tr -d "'")

if [ "$CURRENT" = "prefer-dark" ]; then
    gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
else
    gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
fi

pkill -RTMIN+9 waybar
