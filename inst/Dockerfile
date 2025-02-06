FROM bioconductor/bioconductor_docker:3.20-R-4.4.2

# Install R dependencies
RUN Rscript -e "devtools::install_github('adafede/CentroidR')"
# RUN Rscript -e "install.packages('CentroidR', repos = c('https://adafede.r-universe.dev', 'https://bioc.r-universe.dev', 'https://cran.r-universe.dev'))"

# Set default working directory and copy the script
COPY inst/scripts/centroiding.R centroiding.R

# Define default command
# CMD ["Rscript", "centroiding.R"]