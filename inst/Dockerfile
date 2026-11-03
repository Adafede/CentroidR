FROM bioconductor/bioconductor_docker:RELEASE_3_23-R-4.6.0

# Pre-install Bioconductor dependencies in a cached layer before source copy
RUN Rscript -e "BiocManager::install(c('BiocParallel', 'MsCoreUtils', 'mzR', 'Spectra'), update = FALSE, ask = FALSE)"

# Invalidate Docker layer cache and embed metadata when a new commit is pushed to main
ARG GIT_COMMIT=unknown
ENV GIT_COMMIT=${GIT_COMMIT}
LABEL org.opencontainers.image.title="CentroidR" \
      org.opencontainers.image.description="Centroid Profile Mass Spectrometry Data (mzML)" \
      org.opencontainers.image.source="https://github.com/adafede/CentroidR" \
      org.opencontainers.image.licenses="AGPL-3.0-or-later" \
      org.opencontainers.image.revision="${GIT_COMMIT}" \
      org.edamontology.topic="topic_3172,topic_3370,topic_0154" \
      org.edamontology.operation="operation_3215" \
      org.edamontology.data="data_0943" \
      org.edamontology.format="format_3244"

# Copy the repository source code into the container and install CentroidR
COPY . /tmp/CentroidR
RUN R CMD INSTALL --no-multiarch /tmp/CentroidR && rm -rf /tmp/CentroidR

# Copy CLI script to the working directory
COPY inst/scripts/centroiding.R /home/bioc-user/centroiding.R

WORKDIR /home/bioc-user

# Default command
CMD ["Rscript", "centroiding.R"]
