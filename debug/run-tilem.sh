#!/bin/sh
set -eu

if [ "$#" -lt 2 ]; then
    echo "usage: $0 KEYBINDINGS TILEM [TILEM-ARGS ...]" >&2
    exit 2
fi

keybindings=$1
tilem=$2
shift 2

case $keybindings in
    /*) ;;
    *) keybindings=$PWD/$keybindings ;;
esac

if [ ! -f "$keybindings" ]; then
    echo "$0: keybindings file not found: $keybindings" >&2
    exit 2
fi

zkeme80_tilem_config=$(mktemp -d "${TMPDIR:-/tmp}/zkeme80-tilem.XXXXXX")
trap 'rm -rf -- "$zkeme80_tilem_config"' EXIT HUP INT TERM
mkdir -p "$zkeme80_tilem_config/tilem2"
cp "$keybindings" "$zkeme80_tilem_config/tilem2/keybindings.ini"

echo "zkeme80: TilEm typewriter bindings active (host a-z type A-Z)" >&2
XDG_CONFIG_HOME=$zkeme80_tilem_config "$tilem" "$@"
