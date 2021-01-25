#' Create a stack
#'
#' A `faststack` is backed by a list. The backing list will grow or shrink as
#' the stack changes in size.
#'
#' `faststack` objects have the following methods:
#'
#' \describe{
#'   \item{\code{push(x)}}{
#'     Push an object onto the stack.
#'   }
#'   \item{\code{mpush(..., .list = NULL)}}{
#'     Push objects onto the stack. `.list` can be a list of objects to add.
#'   }
#'   \item{\code{pop(missing = missing_default)}}{
#'     Remove and return the top object on the stack. If the stack is empty,
#'     it will return `missing`, which defaults to the value of
#'     `missing_default` that `stack()` was created with (typically, `NULL`).
#'   }
#'   \item{\code{mpop(n, missing = missing_default)}}{
#'     Remove and return the top `n` objects on the stack, in a list. The first
#'     element of the list is the top object in the stack. If `n` is greater
#'     than the number of objects in the stack, any requested items beyond
#'     those in the stack will be replaced with `missing` (typically, `NULL`).
#'   }
#'   \item{\code{peek(missing = missing_default)}}{
#'     Return the top object on the stack, but do not remove it from the stack.
#'     If the stack is empty, this will return `missing`.
#'   }
#'   \item{\code{reset()}}{
#'     Reset the stack, clearing all items.
#'   }
#'   \item{\code{size()}}{
#'     Returns the number of items in the stack.
#'   }
#'   \item{\code{as_list()}}{
#'     Return a list containing the objects in the stack, where the first
#'     element in the list is the object at the bottom of the stack, and the
#'     last element in the list is the object at the top of the stack.
#'   }
#' }
#'
#'
#' @param init Initial size of the list that backs the stack. This is also used
#'   as the minimum size of the list; it will not shrink any smaller.
#' @param missing_default The value to return when `pop()` or `peek()` are
#'   called when the stack is empty. Default is `NULL`.
#' @export
faststack <- function(init = 20, missing_default = NULL) {
  force(missing_default)

  # A list that represents the stack
  s <- vector("list", init)
  # Current size of the stack
  count <- 0L

  push <- function(x) {
    new_size <- count + 1L

    # R 3.4.0 and up will automatically grow vectors in place, if possible, so
    # we don't need to explicitly grow the list here.
    if (is.null(x)) {
      # Special case for NULL (in the normal case, we'll avoid creating a new
      # list() and then unwrapping it.)
      s[new_size] <<- list(NULL)
    } else {
      s[[new_size]] <<- x
    }
    count <<- new_size

    invisible()
  }

  mpush <- function(..., .list = NULL) {
    if (is.null(.list)) {
      # Fast path for common case
      args <- list(...)
    } else {
      args <- c(list(...), .list)
    }

    if (length(args) == 0) {
      stop("`mpush`: No items provided to push on stack.")
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

    # Shrink list if < 1/2 of the list is used, down to a minimum size of `init`
    len <- length(s)
    if (len > init && count < len/2) {
      new_len <- max(init, count)
      s[seq.int(new_len + 1L, len)] <<- list(NULL)
    }

    value
  }

  mpop <- function(n, missing = missing_default) {
    n <- as.integer(n)

    if (n < 1) {
      stop("`n` must be at least 1.")
    }

    if (n > count) {
      n_pop <- count
      n_extra <- n - count
    } else {
      n_pop <- n
      n_extra <- 0L
    }

    idx <- seq.int(count, count - n_pop + 1L)
    if (n_extra != 0) {
      values <- vector("list", n)
      values[seq_len(n_pop)] <- s[idx]
      if (!is.null(missing)) {
        values[seq.int(n_pop + 1, n)] <- missing
      }

    } else {
      values <- s[idx]
    }

    s[idx] <<- list(NULL)
    count <<- count - n_pop

    # Shrink list if < 1/2 of the list is used, down to a minimum size of `init`
    len <- length(s)
    if (len > init && count < len/2) {
      new_len <- max(init, count)
      # Assign in place; avoids making copies
      s[seq.int(new_len + 1L, len)] <<- NULL
    }

    values
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
    mpush   = mpush,
    pop     = pop,
    mpop    = mpop,
    peek    = peek,
    reset   = reset,
    size    = size,
    as_list = as_list
  )
}
