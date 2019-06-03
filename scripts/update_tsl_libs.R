#!/usr/bin/env Rscript
# Retrieves a particular version of tsl/hopscotch_map:
#  https://github.com/Tessil/hopscotch-map

library(rprojroot)

version   <- "2.2.1"
tag       <- paste0("v", version)
zip_file  <- file.path(tempdir(), sprintf("hopscotch-map-%s.zip", tag))
url       <- sprintf("https://github.com/Tessil/hopscotch-map/archive/%s.zip", tag)

download.file(url, zip_file)
unzip(zip_file, exdir = tempdir())
src_dir <- file.path(tempdir(), sprintf("hopscotch-map-%s", version))

dest_dir  <- rprojroot::find_package_root_file("src/lib/tsl")
unlink(dest_dir, recursive = TRUE)

dir.create(dest_dir, recursive = TRUE)
file.copy(
  file.path(src_dir, "include", "tsl",
    c("hopscotch_hash.h", "hopscotch_map.h", "hopscotch_growth_policy.h")
  ),
  dest_dir,
  overwrite = TRUE
)
