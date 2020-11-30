#' Create a stack
#'
#' A stack object is backed by a list. The backing list will grow or shrink as
#' the stack changes in size.
#'
#' @param init Initial size of the list that backs the stack. This is also used
#'   as the minimum size of the list; it will not shrink any smaller.
#' @param missing_default The value to return when `pop()` or `peek()` are
#'   called when the stack is empty. Default is `NULL`.
#' @export
stack <- function(init = 20, missing_default = NULL) {
  force(missing_default)

  # A list that represents the stack
  s <- vector("list", init)
  # Current size of the stack
  count <- 0L

  push <- function(..., .list = NULL) {
    if (is.null(.list)) {
      # Fast path for common case
      args <- list(...)
    } else {
      args <- c(list(...), .list)
    }
    new_size <- count + length(args)

    # R 3.4.0 and up will automatically grow vectors in place, if possible, so
    # we don't need to explicitly grow the list here.
    s[count + seq_along(args)] <<- args
    count <<- new_size

    invisible()
  }

  pop <- function(missing = missing_default) {
    if (count == 0L) {
      return(missing)
    }

    value <- s[[count]]
    s[count] <<- list(NULL)
    count <<- count - 1L

    # Shrink list if < 1/4 of the list is used, down to a minimum size of `init`
    len <- length(s)
    if (len > init && count < len/4) {
      new_len <- max(init, ceiling(len/2))
      s <<- s[seq_len(new_len)]
    }

    value
  }

  peek <- function(missing = missing_default) {
    if (count == 0L) {
      return(missing)
    }
    s[[count]]
  }

  reset <- function() {
    s <<- vector("list", init)
    count <<- 0L
    invisible()
  }

  size <- function() {
    count
  }

  # Return the entire stack as a list, where the first item in the list is the
  # oldest item in the stack, and the last item is the most recently added.
  as_list <- function() {
    s[seq_len(count)]
  }


  list(
    push    = push,
    pop     = pop,
    peek    = peek,
    reset   = reset,
    size    = size,
    as_list = as_list
  )
}
