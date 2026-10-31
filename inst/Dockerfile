FROM bioconductor/bioconductor_docker:RELEASE_3_23-R-4.6.0

# Install remotes for local package installation
RUN Rscript -e "install.packages('remotes')"

# Invalidate Docker layer cache and embed metadata when a new commit is pushed to main
ARG GIT_COMMIT=unknown
ENV GIT_COMMIT=${GIT_COMMIT}
LABEL org.opencontainers.image.revision="${GIT_COMMIT}"

# Copy the repository source code into the container
COPY . /tmp/CentroidR

# Install CentroidR from the local source tree (reflecting the exact commit)
RUN Rscript -e "remotes::install_local('/tmp/CentroidR', dependencies = TRUE, force = TRUE)"

# Copy script to the working directory
COPY inst/scripts/centroiding.R /home/bioc-user/centroiding.R

WORKDIR /home/bioc-user

# Default command
CMD ["Rscript", "centroiding.R"]
