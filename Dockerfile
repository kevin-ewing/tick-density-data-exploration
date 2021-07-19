# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5
FROM osgeo/gdal

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \ 
    libmysqlclient-dev \
    default-libmysqlclient-dev \ 
    libgdal-dev \
    libproj-dev
    

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
  

# copy necessary files
## app folder
COPY /shiny-app ./app

# install R packages required 
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinythemes", \
              "textdata", \
              "tidyverse", \
              "rgdal" \
              "leaflet", \
              "viridis", \
              "stringi" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'
RUN R -e 'install.packages("rgdal", dependencies=TRUE, \
                            configure.args=c("--with-proj-lib=/opt/local/lib", \
                            "--with-gdal-lib=/opt/local/lib"))'


# run app on container start
CMD ["R", "-e", "shiny::runApp('/app')"]