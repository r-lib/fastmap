#!/usr/bin/env Rscript
library(rprojroot)

version <- "2.3.0"
tag <- paste0("v", version)

source_url_path <- sprintf("https://raw.githubusercontent.com/Tessil/hopscotch-map/%s/include/tsl/", tag)

dest_dir <- find_package_root_file("src/lib/tsl/")

filenames <- c(
  "hopscotch_growth_policy.h",
  "hopscotch_hash.h",
  "hopscotch_map.h"
)

for (filename in filenames) {
  download.file(paste0(source_url_path, filename), file.path(dest_dir, filename))
}
