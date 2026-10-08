FROM bioconductor/bioconductor_docker:RELEASE_3_23-R-4.6.0

# Invalidate Docker layer cache when a new commit is pushed
ARG GIT_COMMIT=unknown
ENV GIT_COMMIT=${GIT_COMMIT}

# Install CentroidR and force latest GitHub version
RUN Rscript -e "install.packages('remotes')" && \
    Rscript -e "remotes::install_github('adafede/CentroidR', force = TRUE)"

# Copy script
COPY inst/scripts/centroiding.R /home/bioc-user/centroiding.R

WORKDIR /home/bioc-user

# Default command
CMD ["Rscript", "centroiding.R"]