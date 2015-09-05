#!/bin/bash

set -e

SDL="SDL2-2.0.3"
TTF="SDL2_ttf-2.0.12"

if [ ! -d "$HOME/$SDL/lib" ]; then
    wget "https://www.libsdl.org/release/$SDL.tar.gz"
    tar -xvzf "$SDL.tar.gz"
    cd "$SDL" && ./configure --prefix="$HOME/$SDL" && make && make install
else
    echo 'Using cached SDL.'
fi

export PKG_CONFIG_PATH="$HOME/$SDL/lib/pkgconfig"
export SDL2_CONFIG="$HOME/$SDL/bin/sdl2-config"

if [ ! -d "$HOME/$TTF/lib" ]; then
    wget "https://www.libsdl.org/projects/SDL_ttf/release/$TTF.tar.gz"
    tar -xvzf "$TTF.tar.gz"
    cd "$TTF" && ./configure --prefix="$HOME/$TTF" && make && make install
else
    echo 'Using cached TTF.'
fi

