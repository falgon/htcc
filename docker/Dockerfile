FROM ubuntu:18.04
ARG term=xterm

LABEL maintainer="falgon53@yahoo.co.jp"
SHELL ["/bin/bash", "-c"]
ENV DEBCONF_NOWARNINGS yes
ENV TERM $term

# User setting
ARG user=htcc_user
ARG group=user

# Install required packages
RUN apt-get -qq update && apt-get install -qq -y --no-install-recommends \
    apt-utils \
    binutils \
    gcc \
    gdb \
    git \
    libc6-dev \
    && apt-get clean\
    && rm -rf /var/lib/apt/lists/*
