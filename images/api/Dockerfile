FROM ubuntu:16.04

RUN apt-get -y update \
  && apt-get -y install libmysqlclient-dev libssl-dev libgmp10

ADD config.yaml /
ADD dist/slacknowledge-api /usr/bin/
ADD dist/slacknowledge-bot /usr/bin/
ADD static /

# ENTRYPOINT ["slacknowledge-api"]