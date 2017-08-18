#!/bin/sh

echo "Ensure docker image is built"
docker build $(pwd) -t memetalk

echo "Run make on the Memetalk VM directory"
docker run -v $(pwd):/build -it memetalk $@
