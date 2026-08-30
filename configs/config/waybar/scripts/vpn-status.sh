#!/bin/bash
# Waybar custom module: Netplex WireGuard status

IFACE="netplex"

if ip link show "$IFACE" &>/dev/null; then
    TOOLTIP="Netplex VPN: up\nClick to disconnect"
    PEER_INFO=$(sudo -n wg show "$IFACE" latest-handshakes 2>/dev/null)
    if [ -n "$PEER_INFO" ]; then
        HANDSHAKE_TS=$(echo "$PEER_INFO" | awk '{print $2}')
        if [ -n "$HANDSHAKE_TS" ] && [ "$HANDSHAKE_TS" != "0" ]; then
            NOW=$(date +%s)
            AGO=$((NOW - HANDSHAKE_TS))
            TOOLTIP="Netplex VPN: up\nLast handshake: ${AGO}s ago\nClick to disconnect"
        fi
    fi
    printf '{"text":"VPN","class":"connected","tooltip":"%s"}\n' "$TOOLTIP"
else
    printf '{"text":"VPN","class":"disconnected","tooltip":"Netplex VPN: down\\nClick to connect"}\n'
fi
