#!/bin/bash

# see https://arnesonium.com/2023/07/emacs-29-1-on-ubuntu-22-04-lts
# see https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation

export CC=/usr/bin/gcc-11
export CXX=/usr/bin/gcc-11

sudo apt build-dep emacs
sudo apt install -y \
     gcc-10 \
     gnutls-bin \
     imagemagick \
     libgccjit-11-dev \
     libgccjit0 \
     libgif-dev \
     libgnutls28-dev \
     libharfbuzz-dev \
     libjansson-dev \
     libjansson4 \
     libmagick++-dev \
     libmailutils-dev \
     libsqlite3-dev \
     libtinfo-dev \
     libtree-sitter-dev \
     libwebp-dev \
     libxaw7-dev \
     libxft-dev \
     libxft2 \
     sqlite3 \
     sqlite3-tools \
     webp

./autogen.sh
./configure \
    --with-native-compilation=aot \
    --with-imagemagick \
    --with-json \
    --with-sqlite3 \
    --with-tree-sitter \
    --with-xft \
    --with-pop \
    --with-mailutils \
    --prefix=$HOME/local
make -j `nproc`
make install
