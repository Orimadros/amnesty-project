# syntax=docker/dockerfile:1.7

FROM rocker/geospatial:4.5.3@sha256:5d50d6d3bd70f5b48e60bf4ecc5cabb2cb854bf072117f975ba78d5d5165bfa3

LABEL org.opencontainers.image.title="amazon-amnesty" \
      org.opencontainers.image.description="Reproducible spatial environment for the amnesty project replication package"

ARG CRAN_SNAPSHOT=https://packagemanager.posit.co/cran/2026-05-03

ENV TZ=Etc/UTC \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PROJ_NETWORK=OFF \
    GDAL_DATA=/usr/share/gdal \
    PROJ_LIB=/usr/share/proj \
    OMP_NUM_THREADS=1 \
    OPENBLAS_NUM_THREADS=1 \
    MKL_NUM_THREADS=1 \
    VECLIB_MAXIMUM_THREADS=1 \
    RSPM=${CRAN_SNAPSHOT}

WORKDIR /amnesty-project

# Use a fixed CRAN snapshot and explicit sf engine behavior.
RUN printf "options(repos = c(CRAN = '%s'))\noptions(sf_use_s2 = TRUE)\n" "${CRAN_SNAPSHOT}" > /usr/local/lib/R/etc/Rprofile.site

# Restore exact package versions from renv.lock (committed in the repo).
COPY renv.lock /amnesty-project/renv.lock

ENV RENV_CONFIG_AUTOLOAD_ENABLED=FALSE
ENV R_PROFILE_USER=/dev/null
RUN R -q -e "install.packages('renv'); renv::restore()"

# Copy the rest of the project so the image is self-contained for replicators.
# During development, bind-mounting the host folder over /amnesty-project will
# override this copy with live edits.
COPY . /amnesty-project

# Drop into an interactive shell by default (development-friendly).
# Replicators run the analysis explicitly, e.g.:
#   docker run --rm myproject:v1 Rscript code/analysis/run_vnp_price_map.R
CMD ["bash"]