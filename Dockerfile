FROM bioconductor/bioconductor_docker:3.20-R-4.4.2

# Add a non-root user and create the R library directory
RUN useradd -m centroid-user && \
    mkdir -p /home/centroid-user/Library/Frameworks/R.framework/Resources/site-library && \
    chown -R centroid-user:centroid-user /home/centroid-user

# Set the R library path to the new directory
ENV R_LIBS_USER=/home/centroid-user/Library/Frameworks/R.framework/Resources/site-library

# Switch to the non-root user
USER centroid-user
WORKDIR /home/centroid-user

# Install R dependencies
# RUN Rscript -e "devtools::install_github('adafede/CentroidR')"
RUN Rscript -e "install.packages('CentroidR', repos = c('https://adafede.r-universe.dev', 'https://bioc.r-universe.dev', 'https://cran.r-universe.dev'))"

# Set default working directory and copy the script
COPY inst/scripts/centroiding.R /home/centroid-user/centroiding.R

# Disable healthcheck (if you really want to disable it)
HEALTHCHECK NONE

# Define default command
# CMD ["Rscript", "/home/centroid-user/centroiding.R"]