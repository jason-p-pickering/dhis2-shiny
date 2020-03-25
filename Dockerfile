FROM openanalytics/r-base

MAINTAINER Jason P. Pickering "jason@dhis2.org"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
libcurl4-gnutls-dev \
libssl-dev \
libxml2-dev

# basic shiny functionality
RUN R -e "install.packages(c('glue','dplyr','tibble','httr','jsonlite','tidyr','stringr',\
'rpivotTable','DT','ggplot2','magrittr','shiny',\
'shinyjs','shinyWidgets'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/dhis2-shiny
COPY dhis2-shiny /root/dhis2-shiny
COPY Rprofile.site /usr/lib/R/etc/
  
EXPOSE 3939

CMD ["R", "-e", "shiny::runApp('/root/dhis2-shiny')"]
