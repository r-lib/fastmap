#' Create a queue
#'
#' A queue object is backed by a list, which is used in a circular manner. The
#' backing list will grow or shrink as the queue changes in size.
#'
#' @param init Initial size of the list that backs the queue. This is also used
#'   as the minimum size of the list; it will not shrink any smaller.
#' @export
queue <- function(init = 20) {
  q <- vector("list", init)
  head <- 0L  # Index of most recently added item
  tail <- 0L  # Index of oldest item (next to be removed)


  # TODO: implement with ... and .list arguments
  add <- function(value) {
    # If value will throw an error, do it early.
    force(value)

    len <- length(q)
    new_head <- head + 1L
    if (new_head > len)
      new_head <- new_head - len

    if (new_head <= tail) {
      # low_tail can be negative values up to zero, and is always less than new_head.
      low_tail <- tail - len
    } else {
      # In this case, low_tail can be from 1 up to and including head
      low_tail <- tail
    }

    # If head meets tail, need to grow.
    if (new_head - low_tail >= len) {
      .resize(ceiling(len*2))

      # Set head and tail and add the item
      tail <<- 1L
      head <<- new_head - low_tail + 1L

      # Need special workaround for NULL;
      if (is.null(value)) q[head] <<- list(NULL)
      else                q[[head]] <<- value
      return(invisible())
    }

    # Didn't need to grow, but we wrapped around the list
    if (new_head > len)
      new_head <- new_head - len

    # If tail was at zero, we had an empty queue, and need to set tail to 1
    if (tail == 0L)
      tail <<- 1L

    head <<- new_head
    q[head] <<- list(value)
    invisible()
  }

  remove <- function() {
    if (tail == 0L)
      return(NULL)

    len <- length(q)
    value <- q[[tail]]
    q[tail] <<- list(NULL)
    if (tail == head) {
      # We've emptied the queue
      tail <<- head <<- 0L
    } else {
      tail <<- tail + 1L

      # Wrapped around
      if (tail > len)
        tail <<- tail - len
    }

    # Shrink list if < 1/4 of the list is used, down to a minimum size of `init`
    if (len > init && size() < len/4) {
      .resize(max(init, ceiling(len/4)))
    }

    value
  }

  peek <- function() {
    if (tail == 0L)
      return(NULL)

    q[[tail]]
  }

  empty <- function() {
    head == 0L
  }

  reset <- function() {
    q <<- vector("list", init)
    head <<- 0L
    tail <<- 0L
    invisible()
  }

  size <- function() {
    if (head == 0L)
      return(0L)

    n <- head - tail + 1L
    if (n < 1L)
      n <- n + length(q)

    n
  }

  # Return the entire queue as a list, where the first item is the next to be
  # removed (and oldest in the queue).
  # `.size` is the desired size of the output list. This is only for internal use.
  as_list <- function(.size = NULL) {
    if (head == 0L)
      return(list())

    if (is.null(.size)) {
      .size <- size()
    } else if (.size < size()) {
      stop("Can't return list smaller than number of items.")
    }

    len <- length(q)

    # low_tail can be negative values up to zero, and is always less than head.
    low_tail <- tail
    if (head < tail)
      low_tail <- tail - len

    # Get indices and transfer over old
    new_q <- vector("list", .size)
    old_idx <- (seq(low_tail, head) - 1L) %% len + 1L
    new_q[seq_len(length(old_idx))] <- q[old_idx]

    new_q
  }

  .resize <- function(new_size) {
    n <- size()
    if (new_size < n)
      stop("Can't shrink smaller than number of items (", size(), ").")
    if (new_size <= 0)
      stop("Can't shrink smaller than one.")

    if (head == 0L) {
      q <<- vector("list", new_size)
      return(invisible())
    }

    len <- length(q)

    q <<- as_list(new_size)

    # Set head and tail and add the item
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
