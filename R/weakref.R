#' @export
make_weakref <- function(obj) {
  .Call(C_make_weakref, obj)
}

#' @export
get_weakref <- function(obj) {
  .Call(C_get_weakref, obj)
}

#' @export
is_weakref <- function(obj) {
  .Call(C_is_weakref, obj)
}
