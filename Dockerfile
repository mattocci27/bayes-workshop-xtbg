FROM mattocci/cmdstan-verse-zsh:4.5.0

ENV DEBIAN_FRONTEND=noninteractive

# Debug output to confirm TARGETPLATFORM (optional)
ARG TARGETPLATFORM
RUN echo "TARGETPLATFORM: ${TARGETPLATFORM}"

USER root

RUN apt-get update -q -y \
  && apt-get install --no-install-recommends --fix-missing -y \
    libmagick++-dev \
  && apt-get autoremove -y \
  && apt-get clean all

USER rstudio
