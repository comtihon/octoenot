FROM alpine

MAINTAINER Val Tikhonov <valerii.tikhonov@gmail.com>

RUN apk update
RUN apk del --purge python2 \
    && apk add python3 curl git g++ sudo make sqlite sqlite-dev \
    && pip3 install --upgrade pip
RUN apk add erlang  \
erlang-crypto  \
erlang-erts \
erlang-inets  \
erlang-dev  \
erlang-syntax-tools  \
erlang-tools  \
erlang-asn1 \
erlang-compiler \
erlang-kernel \
erlang-os-mon \
erlang-parsetools \
erlang-reltool \
erlang-runtime-tools \
erlang-public-key \
erlang-sasl \
erlang-ssh \
erlang-ssl \
erlang-stdlib


EXPOSE 4567
RUN pip3 install enot
WORKDIR /opt/app
COPY . /opt/app
RUN coon release

ENTRYPOINT ["/opt/app/_rel/octoenot/bin/octoenot", "foreground"]
CMD []