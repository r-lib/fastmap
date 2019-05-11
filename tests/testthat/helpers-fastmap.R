# Given a fastmap object, return the enclosing environment of its methods.
get_self <- function(m) {
  environment(m$set)
}
