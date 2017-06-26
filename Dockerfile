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
erlang-xmerl \
erlang-asn1 \
erlang-compiler \
erlang-erl-interface \
erlang-kernel \
erlang-orber \
erlang-os-mon \
erlang-otp-mibs \
erlang-parsetools \
erlang-percept \
erlang-public-key \
erlang-reltool \
erlang-runtime-tools \
erlang-sasl \
erlang-snmp \
erlang-ssh \
erlang-ssl \
erlang-stdlib \
erlang-typer


EXPOSE 4567
RUN pip3 install coon
WORKDIR /opt/app
COPY . /opt/app
RUN coon release

ENTRYPOINT ["/opt/app/_rel/octocoon/bin/octocoon", "foreground"]
CMD []