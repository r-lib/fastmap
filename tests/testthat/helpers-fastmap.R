# Get the environment from a fastmap/fastqueue/faststack object, so we can
# access internal objects.
env <- function(x) {
  environment(x$as_list)
}

# An empty named list is different from an empty unnamed list. This creates
# the former.
empty_named_list <- function() {
  list(a=1)[0]
}
