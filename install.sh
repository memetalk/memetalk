#!/bin/sh

DEST="/usr/local"
mkdir -p $DEST/lib/memetalk

install meme $DEST/bin
install -m 644 core.img $DEST/lib/memetalk
install -m 644 etc.meme.config /etc/meme.config
rm -rf $DEST/lib/memetalk/central
cp -r central $DEST/lib/memetalk/central
