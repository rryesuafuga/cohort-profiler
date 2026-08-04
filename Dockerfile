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
RUN apt-get update && apt-get install -y --no-install-recommends \
        libreoffice-writer \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Restore dependencies before copying the source so edits do not invalidate the
# package layer. Restoring into the system library keeps the later COPY from
# clobbering a project library.
#
# renv would otherwise restore from the repository recorded in the lockfile,
# which is source-only CRAN. Overriding it with the image's own repo keeps the
# binary packages.
COPY renv.lock /app/renv.lock
RUN R -q -e "install.packages('renv'); \
             options(renv.config.repos.override = getOption('repos')); \
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
