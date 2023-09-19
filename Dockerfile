#Example dockerfile from Statworx.com

#Base image
FROM rocker/shiny:latest 

#ALT: Important note: rocker/shiny is built on Debian, so the dockerfile needs to use debian commands, even if the hosting server is running something different (currently Rocky Linux).
# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
	libxml2-dev \
	libcairo2-dev \
	libsqlite3-dev \
	#libmariadb-dev \ #Conflicts with libssl
	libpq-dev \
	libssh2-1-dev \
	unixodbc-dev \
	libcurl4-openssl-dev \
	libssl-dev \
	libudunits2-dev \ #Required for r package units
	libgdal-dev #Installs dependencies needed for mapping

## update system libraries
RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get clean

#copy necessary files
ADD /agquery/Data /Data
#Empty directory for chart output
ADD /agquery/Temp /Temp
ADD /agquery/Update /Update
Add /agquery/Spatial /Spatial

#Renv maintains the packages necessary to run the app - update by running renv::snapshot() from within rstudio if the package list changes.
COPY renv.lock ./renv.lock 
COPY renv ./renv
COPY agquery/app.R ./app.R

ENV PATHS_LIBRARY renv/library
#install renv & restore packages
RUN Rscript -e 'install.packages("openssl")' #Without this, other packages (namely s2, required by sf) can't find the openssl files that they need.
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start 
CMD ["R", "-e", "shiny::runApp('/app.R', host = '0.0.0.0', port=3838)"]
