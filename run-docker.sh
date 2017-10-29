#!/bin/sh

echo "Ensure docker image is built"
docker build $(pwd) -t memetalk

echo "Run make on the Memetalk VM directory"
docker run -p 4200:4200 --privileged -v $(pwd):/build -it memetalk $@
