#!/usr/bin/env Rscript
# Retrieves a particular version of tsl/robin_map:
#  https://github.com/Tessil/robin-map

library(rprojroot)

version   <- "0.6.1"
tag       <- paste0("v", version)
zip_file  <- file.path(tempdir(), sprintf("robin-map-%s.zip", tag))
url       <- sprintf("https://github.com/Tessil/robin-map/archive/%s.zip", tag)

download.file(url, zip_file)
unzip(zip_file, exdir = tempdir())
src_dir <- file.path(tempdir(), sprintf("robin-map-%s", version))

dest_dir  <- rprojroot::find_package_root_file("src/lib/tsl")
unlink(dest_dir, recursive = TRUE)

dir.create(dest_dir, recursive = TRUE)
file.copy(
  file.path(src_dir, "include", "tsl",
    c("robin_hash.h", "robin_map.h", "robin_growth_policy.h")
  ),
  dest_dir,
  overwrite = TRUE
)
