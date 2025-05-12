FROM debian:bullseye-slim

# Set environment variables for non-interactive installs
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg \
    xz-utils \
    git \
    libgmp-dev \
    zlib1g-dev \
    make \
    gcc \
    libffi-dev \
    poppler-utils \
    texlive-pictures \
    rubber \
    build-essential \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y locales \
 && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
 && locale-gen

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /site

RUN stack --no-terminal setup --resolver=lts-23.21
RUN stack --no-terminal build pandoc
RUN stack --no-terminal build hakyll

COPY build-site.sh /usr/local/bin/build-site.sh
CMD ["build-site.sh"]
