FROM debian:buster-slim
LABEL maintainer="pmatos@linki.tools"
LABEL description="Debian Stable Slim image with Racket dependencies pre-installed. Mostly used for Racket CI."

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y ca-certificates clang curl gcc git libcairo2-dev libffi-dev libfindbin-libs-perl libfontconfig1-dev libgtk2.0-0 libjpeg-dev libpango1.0-dev libturbojpeg0-dev libxml2-dev make openssl procps python sqlite sudo unzip uuid-dev wget xvfb texlive && \
    curl -sL https://deb.nodesource.com/setup_14.x | bash - && \
    apt-get update && apt-get install -y nodejs && \
    apt-get clean

ENV USER=root
CMD ["bash"]
