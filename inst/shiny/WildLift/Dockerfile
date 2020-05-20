# docker build -t psolymos/caribou .
# docker run -p 4000:3838 psolymos/caribou

FROM rocker/r-base:3.6.3
#FROM rocker/r-ver:3.5.0
MAINTAINER Peter Solymos "psolymos@gmail.com"

# system deps
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
  && rm -rf /var/lib/apt/lists/*

# packages and dependencies
RUN R -e "install.packages('WildLift', repos=c('https://cloud.r-project.org/','https://ABbiodiversity.github.io/drat/'), dependencies=TRUE)"

# set Shiny host and port
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
#RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/local/lib/R/etc/Rprofile.site

# ShinyProxy port
EXPOSE 3838

CMD ["R", "-e", "WildLift::run_app()"]
