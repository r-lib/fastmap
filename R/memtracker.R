#' @export
enable_memtracker <- function() {
  .Call(C_enable_memtracker)
}

#' @export
str_to_sexp <- function(address_str) {
  .Call(C_str_to_sexp, address_str)
}

#' @export
sexp_to_str <- function(obj) {
  .Call(C_sexp_to_str, obj)
}
