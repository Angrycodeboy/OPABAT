FROM rocker/shiny:4.1.3

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libglpk-dev \
    libxml2-dev

RUN install2.r --error --skipinstalled \
    remotes 

RUN R -e "remotes::install_github('daqana/dqshiny')"

RUN install2.r --error --skipinstalled \
    shiny \
    tidyverse \
    ggpubr \
    visNetwork \
    png \
    DT \
    gsubfn \
    shinymanager \
    igraph \
    feather

COPY app/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY app /srv/shiny-server/opabat

# Change permissions
RUN chown -R shiny: /srv/shiny-server/opabat

EXPOSE 3838

# Run as non-root user
USER shiny

CMD ["/usr/bin/shiny-server"]
