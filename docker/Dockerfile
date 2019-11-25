FROM ubuntu:latest
ARG term=xterm

MAINTAINER roki <falgon53@yahoo.co.jp>
SHELL ["/bin/bash", "-c"]
ENV DEBCONF_NOWARNINGS yes
ENV TERM $term

# User setting
ARG user=htcc_user
ARG group=user
RUN apt-get update && apt-get install -y --no-install-recommends sudo
RUN adduser --disabled-password --gecos '' ${user}
RUN echo "${user} ALL=(root) NOPASSWD:ALL" >> /etc/sudoers.d/${user}

# Install required packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
    binutils \
    gcc \
    gdb \
    git \
    libc6-dev
