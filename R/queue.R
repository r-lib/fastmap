#' Create a queue
#'
#' A queue object is backed by a list, which is used in a circular manner. The
#' backing list will grow or shrink as the queue changes in size.
#'
#' @param init Initial size of the list that backs the queue. This is also used
#'   as the minimum size of the list; it will not shrink any smaller.
#' @param missing_default The value to return when `remove()` or `peek()` are
#'   called when the stack is empty. Default is `NULL`.
#' @export
queue <- function(init = 20, missing_default = NULL) {
  force(missing_default)

  q    <- vector("list", init)
  head <- 0L  # Index of most recently added item
  tail <- 0L  # Index of oldest item (next to be removed)
  n    <- 0L  # Number of items in queue

  add <- function(..., .list = NULL) {
    if (is.null(.list)) {
      # Fast path for common case
      args <- list(...)
    } else {
      args <- c(list(...), .list)
    }

    n_args <- length(args)

    if (n_args == 0L) {
      return(invisible())
    }

    capacity <- length(q)

    # We're adding more items than can fit in `q`, so we need to grow it. This
    # will also rearrange items so the tail is at 1 and the head is at n.
    if (n + n_args > capacity) {
      # Resize in powers of 2
      doublings <- ceiling(log2((n + n_args) / capacity))
      new_capacity <- capacity * 2 ^ doublings
      .resize(new_capacity)
      capacity <- new_capacity
    }

    # When we get here, we know we have enough capacity.

    n_until_wrap <- capacity - head
    if (n_until_wrap >= n_args) {
      # Case 1: We don't need to wrap
      q[head + seq_along(args)] <<- args
      head <<- head + n_args

    } else {
      # Case 2: need to wrap around

      # Fill in from head until end of `q`
      if (n_until_wrap > 0) {
        q[seq.int(head + 1, capacity)] <<- args[seq_len(n_until_wrap)]
      }

      # Now fill in beginning of q.
      n_after_wrap <- n_args - n_until_wrap
      q[seq_len(n_after_wrap)] <<- args[seq.int(n_until_wrap + 1, n_args)]

      head <<- head + n_args - capacity
    }

    # If tail was at zero, we had an empty queue, and need to set tail to 1
    if (tail == 0L) {
      tail <<- 1L
    }

    n <<- n + n_args

    invisible()
  }

  remove <- function(missing = missing_default) {
    if (n == 0L)
      return(missing)

    capacity <- length(q)
    value <- q[[tail]]
    q[tail] <<- list(NULL)
    if (tail == head) {
      # We've emptied the queue
      tail <<- head <<- 0L
    } else {
      tail <<- tail + 1L

      # Wrapped around
      if (tail > capacity)
        tail <<- tail - capacity
    }

    n <<- n - 1L

    # Shrink list if < 1/4 of the list is used, down to a minimum size of `init`
    if (capacity > init && size() < capacity/4) {
      .resize(max(init, ceiling(capacity/4)))
    }

    value
  }

  peek <- function(missing = missing_default) {
    if (n == 0L) {
      return(missing)
    }

    q[[tail]]
  }

  empty <- function() {
    head == 0L
  }

  reset <- function() {
    q    <<- vector("list", init)
    head <<- 0L
    tail <<- 0L
    n    <<- 0L
    invisible()
  }

  size <- function() {
    n
  }

  # Return the entire queue as a list, where the first item is the next to be
  # removed (and oldest in the queue).
  # `.size` is the desired size of the output list. This is only for internal use.
  as_list <- function(.size = NULL) {
    if (n == 0L)
      return(list())

    if (is.null(.size)) {
      .size <- size()
    } else if (.size < size()) {
      stop("Can't return list smaller than number of items.")
    }

    capacity <- length(q)

    # low_tail can be negative values up to zero, and is always less than head.
    low_tail <- tail
    if (head < tail)
      low_tail <- tail - capacity

    # Get indices and transfer over old
    new_q <- vector("list", .size)
    old_idx <- (seq(low_tail, head) - 1L) %% capacity + 1L
    new_q[seq_len(length(old_idx))] <- q[old_idx]

    new_q
  }

  .resize <- function(new_size) {
    if (new_size < n)
      stop("Can't shrink smaller than number of items (", size(), ").")
    if (new_size <= 0)
      stop("Can't shrink smaller than one.")

    if (n == 0L) {
      q <<- vector("list", new_size)
      return(invisible())
    }

    q <<- as_list(new_size)
    tail <<- 1L
    head <<- n
    invisible()
  }


  list(
    add     = add,
    remove  = remove,
    peek    = peek,
    empty   = empty,
    reset   = reset,
    size    = size,
    as_list = as_list
  )
}
