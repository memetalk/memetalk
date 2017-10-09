#!/bin/sh

DEST="/usr/local"
mkdir -p $DEST/lib/memetalk

install meme $DEST/bin
install -m 644 core.img $DEST/lib/memetalk
install -m 644 meme.config.sample /etc/meme.config
cp -r central $DEST/lib/memetalk/central
