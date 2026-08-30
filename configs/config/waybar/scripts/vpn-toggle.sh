#!/bin/bash
# Waybar custom module: toggle Netplex WireGuard tunnel

IFACE="netplex"

if ip link show "$IFACE" &>/dev/null; then
    sudo -n wg-quick down "$IFACE"
else
    sudo -n wg-quick up "$IFACE"
fi

pkill -RTMIN+8 waybar
