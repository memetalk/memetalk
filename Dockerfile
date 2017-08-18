FROM debian

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -qy update && apt-get -qy --fix-missing install \
    build-essential git pkg-config \
    libqt4-dev libqtwebkit-dev libqscintilla2-dev \
    libboost-system-dev libboost-filesystem-dev libboost-iostreams-dev \
    python-pip

RUN mkdir /build

RUN /bin/bash -c "pip install \
    ejson \
    clime \
    git+git://github.com/thiago-silva/pymeta.git"

WORKDIR /build/sugarfoot

ENTRYPOINT ["make"]
