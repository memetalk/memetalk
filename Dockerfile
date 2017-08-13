FROM debian

RUN apt-get -qy update && apt-get -qy install build-essential \
    libqt4-dev libqtwebkit-dev libqscintilla2-dev pkg-config libboost-system-dev \
    libboost-filesystem-dev libboost-iostreams-dev

RUN mkdir /build

COPY . /build

WORKDIR /build

CMD ["make"]
