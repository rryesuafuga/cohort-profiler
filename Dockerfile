# rocker already points at the Posit Public Package Manager, so R packages
# install as prebuilt Ubuntu binaries. Do not override the repos: from source,
# this build goes from minutes to about an hour.
#
# The tag matches the R version recorded in renv.lock. Bump both together, or
# renv warns on every restore and the pinned versions stop being the ones that
# were actually tested.
FROM rocker/shiny-verse:4.3.3

# The REDCap export is ISO-8859-1 and carries a degree sign in the temperature
# header. Under LANG=C, R's Latin-1 conversion fails silently, the CSV parses
# into garbage, every column key looks ambiguous and the analytic sample comes
# back as zero rows. This line is load-bearing; the failure looks like a schema
# problem rather than an encoding one.
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# libreoffice-writer converts the rendered DOCX to PDF. libreoffice-core alone
# is not enough: without the Writer filters, soffice reports only "source file
# could not be loaded" for every input.
#
# The lib* lines are the runtime libraries the 2026 package binaries link
# against, enumerated with readelf over every compiled package in renv.lock.
# The base image predates some of them — fs needs libuv, V8 links Ubuntu's
# libnode — and a missing one fails the build with "unable to load shared
# object". Most of the rest are already in the image; listing them anyway is
# a no-op today and insurance against a slimmer base tomorrow.
RUN apt-get update && apt-get install -y --no-install-recommends \
        libreoffice-writer \
        libcairo2 libcurl4 libfontconfig1 libfreetype6 libfribidi0 \
        libharfbuzz0b libjpeg-turbo8 libnode72 libpng16-16 libssl3 \
        libtiff5 libuv1 libwebp7 libwebpmux3 libxml2 zlib1g \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Restore dependencies before copying the source so edits do not invalidate the
# package layer. Restoring into the system library keeps the later COPY from
# clobbering a project library.
#
# The image's own CRAN mirror is frozen at the date R 4.3.3 was current
# (2024), but renv.lock records 2026 package versions, so restoring from the
# image's repo fails outright ("failed to retrieve package"). Point renv at a
# Posit Package Manager snapshot dated to match the lockfile instead: it
# serves Ubuntu jammy binaries built for R 4.3, so installs stay fast, and
# the couple of lockfile versions CRAN superseded that same week resolve from
# the snapshot's archive. All 107 locked versions were verified retrievable
# from this snapshot before it was pinned here.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/2026-08-05
COPY renv.lock /app/renv.lock
RUN R -q -e "install.packages('renv', repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')); \
             renv::restore(lockfile = '/app/renv.lock', \
                           library = .libPaths()[1], prompt = FALSE)"

COPY . /app

# Hugging Face Spaces runs the container as uid 1000. LibreOffice also needs a
# writable HOME for its profile, or the PDF conversion fails.
RUN useradd -m -u 1000 appuser && chown -R appuser:appuser /app
USER appuser
ENV HOME=/home/appuser

# Spaces requires the app on 7860, bound to all interfaces. Run Shiny directly;
# there is no shiny-server in this image's role.
EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 7860)"]
