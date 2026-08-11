# Build with
#   docker build --platform linux/arm64/v8 --tag native-libs-aarch64 .
#   docker build --platform linux/amd64 --tag native-libs-x86_64 .
# Run with
#  docker run -it -v /absolute/path/on/host/to/archives:/archives native-libs
# Racket needs:
#  at-exp-lib
FROM buildpack-deps:buster
RUN printf '%s\n' \
      'deb http://archive.debian.org/debian buster main contrib non-free' \
      'deb http://archive.debian.org/debian-security buster/updates main contrib non-free' \
      > /etc/apt/sources.list \
 && echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf.d/99no-check-valid-until \
 && apt-get update
RUN apt-get update
RUN apt-get install -y gperf ninja-build chrpath

RUN wget https://www.python.org/ftp/python/3.14.7/Python-3.14.7.tgz \
    && tar -xf Python-3.14.7.tgz \
    && cd Python-3.14.7 \
    && ./configure \
    && make \
    && make altinstall
