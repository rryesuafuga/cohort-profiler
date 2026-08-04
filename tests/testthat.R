library(testthat)

# The repo is loaded from source rather than installed, so prefer load_all()
# and fall back to sourcing R/ directly when pkgload is unavailable.
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
}

test_dir("tests/testthat")
