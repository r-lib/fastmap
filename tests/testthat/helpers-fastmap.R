# Get the environment from a fastmap/fastqueue/faststack object, so we can
# access internal objects.
env <- function(x) {
  environment(x$as_list)
}
