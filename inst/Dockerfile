FROM bioconductor/bioconductor_docker:RELEASE_3_21-R-4.5.0

# Install R dependencies
RUN Rscript -e "devtools::install_github('adafede/CentroidR')"
# RUN Rscript -e "install.packages('CentroidR', repos = c('https://adafede.r-universe.dev', 'https://bioc.r-universe.dev', 'https://cran.r-universe.dev'))"

# Copy the script
COPY inst/scripts/centroiding.R centroiding.R

# Define default command
# CMD ["Rscript", "centroiding.R"]