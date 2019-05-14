#' A missing key object
#'
#' A \code{key_missing} object represents a missing key.
#'
#' @param x An object to test.
#'
#' @export
key_missing <- function() {
  # Note: this is more verbose, but much faster than
  # structure(list(), class = "key_missing")
  x <- list()
  class(x) <- "key_missing"
  x
}

#' @rdname key_missing
#' @export
is.key_missing <- function(x) {
  inherits(x, "key_missing")
}

#' @export
print.key_missing <- function(x, ...) {
  cat("<Key Missing>\n")
}
