FROM ubuntu:latest
MAINTAINER roki <falgon53@yahoo.co.jp>
SHELL ["/bin/bash", "-c"]
ENV DEBCONF_NOWARNINGS yes

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
 #   ca-certificates \
 #   curl \
    gcc \
    gdb \
    git \
    libc6-dev
 #   make

# Install stack
#USER $user
#WORKDIR /home/${user}
#RUN set -o pipefail && curl -sSL https://get.haskellstack.org/ | sh && stack --version
#ENV PATH /home/${user}/.local/bin:$PATH
#RUN stack update
