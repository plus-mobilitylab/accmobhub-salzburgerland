# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

RUN mkdir /home/notebook

# Install system libraries
RUN apt-get update && apt-get upgrade -y && apt-get clean
RUN apt -y install software-properties-common
RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable && apt -y update
RUN apt -y --no-install-recommends install libgdal-dev libgeos-dev libproj-dev libudunits2-dev
RUN apt -y install libglpk-dev libxml2-dev

# Install R Packages
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('r-spatial/sf')"
RUN R -e "devtools::install_github('r-spatial/lwgeom')"

RUN install2.r --error \
  DT \
  here \
  leaflet \
  plotly \
  tidyverse \
  units \
  sfnetworks

# Copy necessary files
COPY notebook.Rmd /home/notebook/notebook.Rmd
COPY data/data.zip /home/notebook/data/data.zip

# Unzip data
RUN unzip /home/notebook/data/data.zip -d /home/notebook/data

# Expose port
EXPOSE 3838

# Run notebook on container start
CMD ["R", "-e", "rmarkdown::run('/home/notebook/notebook.Rmd', shiny_args = list(host = '0.0.0.0', port = 3838))"]