#' Create a queue
#'
#' A `fastqueue` is backed by a list, which is used in a circular manner. The
#' backing list will grow or shrink as the queue changes in size.
#'
#' `fastqueue` objects have the following methods:
#'
#' \describe{
#'   \item{\code{add(x)}}{
#'     Add an object to the queue.
#'   }
#'   \item{\code{madd(..., .list = NULL)}}{
#'     Add objects to the queue. `.list` can be a list of objects to add.
#'   }
#'   \item{\code{remove(missing = missing_default)}}{
#'     Remove and return the next object in the queue, but do not remove it from
#'     the queue. If the queue is empty, this will return `missing`, which
#'     defaults to the value of `missing_default` that `queue()` was created
#'     with (typically, `NULL`).
#'   }
#'   \item{\code{remove(n, missing = missing_default)}}{
#'     Remove and return the next `n` objects on the queue, in a list. The first
#'     element of the list is the oldest object in the queue (in other words,
#'     the next item that would be returned by `remove()`). If `n` is greater
#'     than the number of objects in the queue, any requested items beyond
#'     those in the queue will be replaced with `missing` (typically, `NULL`).
#'   }
#'   \item{\code{peek(missing = missing_default)}}{
#'     Return the next object in the queue but do not remove it from the queue.
#'     If the queue is empty, this will return `missing`.
#'   }
#'   \item{\code{reset()}}{
#'     Reset the queue, clearing all items.
#'   }
#'   \item{\code{size()}}{
#'     Returns the number of items in the queue.
#'   }
#'   \item{\code{as_list()}}{
#'     Return a list containing the objects in the queue, where the first
#'     element in the list is oldest object in the queue (in other words, it is
#'     the next item that would be returned by `remove()`), and the last element
#'     in the list is the most recently added object.
#'   }
#' }
#'
#'
#' @param init Initial size of the list that backs the queue. This is also used
#'   as the minimum size of the list; it will not shrink any smaller.
#' @param missing_default The value to return when `remove()` or `peek()` are
#'   called when the stack is empty. Default is `NULL`.
#' @export
fastqueue <- function(init = 20, missing_default = NULL) {
  force(missing_default)

  q     <- vector("list", init)
  head  <- 0L  # Index of most recently added item
  tail  <- 0L  # Index of oldest item (next to be removed)
  count <- 0L  # Number of items in queue

  add <- function(x) {
    force(x)

    capacity <- length(q)
    if (count + 1L > capacity) {
      capacity <- .resize_at_least(count + 1L)
    }

    if (capacity - head >= 1L) {
      # Case 1: We don't need to wrap
      head <<- head + 1L
    } else {
      # Case 2: need to wrap around
      head <<- 1L
    }

    if (is.null(x)) {
      q[head] <<- list(NULL)
    } else {
      q[[head]] <<- x
    }

    # If tail was at zero, we had an empty queue, and need to set tail to 1
    if (tail == 0L) {
      tail <<- 1L
    }

    count <<- count + 1L

    invisible()
  }

  madd <- function(..., .list = NULL) {
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
    if (count + n_args > capacity) {
      capacity <- .resize_at_least(count + n_args)
    }

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

    count <<- count + n_args

    invisible()
  }

  remove <- function(missing = missing_default) {
    if (count == 0L)
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

    count <<- count - 1L

    # Shrink list if <= 1/4 of the list is used, down to a minimum size of
    # `init`. When we resize, make sure there's room to add items without having
    # to resize again (that's why the +1 is there).
    if (capacity > init && count <= capacity/4) {
      .resize_at_least(count + 1L)
    }

    value
  }

  mremove <- function(n, missing = missing_default) {
    n <- as.integer(n)

    if (n < 1) {
      stop("`n` must be at least 1.")
    }

    capacity <- length(q)
    values <- vector("list", n)

    # When removing multiple, there are two variables to deal with:
    #   (1) no wrap vs. (2) wrap
    #   (A) n < count vs. (B) n == count vs. (C) n > count

    # =====================================================================
    # First run: Fill from tail until we hit n items, head, or end of list.
    # =====================================================================
    run_length <- min(n, capacity-tail+1L)
    if (head >= tail) {
      # In the case when the queue does NOT wrap around...
      run_length <- min(run_length, head-tail+1L)
    }
    run_idxs <- seq.int(tail, tail + run_length - 1)
    values[seq_len(run_length)] <- q[run_idxs]
    q[run_idxs] <<- list(NULL)

    # After first run, do some bookkeeping.
    total_filled <-  run_length
    remaining_n  <-  n     - run_length
    count        <<- count - run_length
    tail         <<- tail  + run_length

    if (count == 0L) {
      # We've emptied the queue
      head <<- 0L
      tail <<- 0L

    } else if (tail > capacity) {
      # We've wrapped around
      stopifnot(tail == capacity + 1L) # Should alwoys land on one after capacity (debugging)
      tail <<- 1L
    }

    # ==========================================================
    # Second run: Continue filling until we hit n items or head.
    # ==========================================================
    if (remaining_n > 0 && count != 0 && tail <= head) {
      stopifnot(tail == 1L) # Make sure we've actually wrapped
      run_length <- min(remaining_n, head)
      run_idxs <- seq_len(run_length)
      values[seq.int(total_filled+1, total_filled+run_length)] <- q[run_idxs]
      q[run_idxs] <<- list(NULL)

      # Do more bookkeeping. TODO: functionize this part
      total_filled <-  total_filled + run_length
      remaining_n  <-  remaining_n  - run_length
      count        <<- count        - run_length
      tail         <<- tail         + run_length

      if (count == 0L) {
        # We've emptied the queue
        stopifnot(tail == head + 1L) # Should land on one after head (debugging)
        head <<- 0L
        tail <<- 0L
      }
    }

    # ===============================================================
    # Third run: We've emptied the queue but still need to fill more.
    # ===============================================================
    if (remaining_n > 0) {
      stopifnot(count == 0)
      values[seq(total_filled+1, n)] <- list(missing)
    }

    # Shrink list if <= 1/4 of the list is used, down to a minimum size of
    # `init`. When we resize, make sure there's room to add items without having
    # to resize again (that's why the +1 is there).
    if (capacity > init && count <= capacity/4) {
      .resize_at_least(count + 1L)
    }

    values
  }

  peek <- function(missing = missing_default) {
    if (count == 0L) {
      return(missing)
    }

    q[[tail]]
  }

  reset <- function() {
    q     <<- vector("list", init)
    head  <<- 0L
    tail  <<- 0L
    count <<- 0L
    invisible()
  }

  size <- function() {
    count
  }

  # Return the entire queue as a list, where the first item is the next to be
  # removed (and oldest in the queue).
  as_list <- function() {
    if (count == 0L)
      return(list())

    .as_list()
  }

  # Internal version of as_list()
  # `.size` is the desired size of the output list.
  .as_list <- function(.size = count) {
    if (.size < count) {
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

  # Resize to a specific size. This will also rearrange items so the tail is at
  # 1 and the head is at count.
  .resize <- function(n) {
    if (n < count) {
      stop("Can't shrink smaller than number of items (", count, ").")
    }
    if (n <= 0) {
      stop("Can't shrink smaller than one.")
    }

    # If q is already the right size, don't need to do anything.
    if (length(q) == n) {
      return(n)
    }

    if (count == 0L) {
      q <<- vector("list", n)
      return(n)
    }

    q    <<- .as_list(n)
    tail <<- 1L
    head <<- count
    n
  }

  # Resize the backing list to a size that's `init` times a power of 2, so that
  # it's at least as large as `n`.
  .resize_at_least <- function(n) {
    doublings <- ceiling(log2(n / init))
    doublings <- max(0, doublings)
    new_capacity <- init * 2 ^ doublings
    .resize(new_capacity)
  }


  list(
    add     = add,
    madd    = madd,
    remove  = remove,
    mremove = mremove,
    peek    = peek,
    reset   = reset,
    size    = size,
    as_list = as_list
  )
}
