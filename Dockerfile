FROM debian

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -qy update && apt-get -qy --fix-missing install \
    build-essential git pkg-config xvfb libgc-dev libatomic-ops-dev \
    libqt4-dev libqtwebkit-dev libqscintilla2-dev libre2-dev \
    libboost-system-dev libboost-filesystem-dev libboost-iostreams-dev \
    python-pip gdb libre2-dev wget

RUN mkdir /build

RUN /bin/bash -c "pip install \
    ejson \
    clime \
    git+git://github.com/thiago-silva/pymeta.git"

WORKDIR /build

# Runs tests under xvfb to allow Qt to connect to a display
CMD make test
