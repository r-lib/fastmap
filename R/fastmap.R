#' @useDynLib fastmap, .registration = TRUE
NULL

#' @export
fastmap <- function() {
  # Number of items currently stored in the fastmap object.
  n <- 0L
  # Mapping from key (a string) to index into the list that stores the values
  # (which can be any R object).
  key_idx_map <- .Call(C_map_create)
  # The backing store for the R objects.
  values <- list()
  # Indices in the list which are less than n and not currently occupied. These
  # occur when objects are removed from the map.
  holes <- integer()
  n_holes <- 0L
  self <- environment()

  set <- function(key, value) {
    idx <- .Call(C_map_get, key_idx_map, key)

    if (idx == -1L) {
      # This is a new key. If we have any holes in our values list, store it
      # there. Otherwise append to the end of the values list.
      if (n_holes != 0L) {
        idx <- holes[n_holes]
        holes[n_holes] <<- NA_integer_   # Mark as NA, for safety
        n_holes <<- n_holes - 1L
      } else {
        n <<- n + 1L
        idx <- n
      }
      .Call(C_map_set, key_idx_map, key, idx)
    }

    if (is.null(value)) {
      # Need to handle NULLs differently. Wrap them in a list so that this
      # doesn't result in deletion.
      values[idx] <<- list(NULL)
    } else {
      values[[idx]] <<- value
    }

    invisible(self)
  }

  mset <- function(...) {
    objs <- list(...)
    keys <- names(objs)
    if (is.null(keys) || any(keys == "")) {
      stop("mset: all values must be named.")
    }
    for (i in seq_along(objs)) {
      set(keys[i], objs[[i]])
    }

    invisible(self)
  }

  get <- function(key) {
    idx <- .Call(C_map_get, key_idx_map, key)
    if (idx == -1L) {
      return(NULL)
    }

    values[[idx]]
  }

  mget <- function(keys) {
    lapply(keys, get)
  }

  exists <- function(key) {
    idx <- .Call(C_map_get, key_idx_map, key)
    return(idx != -1L)
  }

  remove <- function(key) {
    idx <- .Call(C_map_remove, key_idx_map, key)
    if (idx == -1L) {
      return(invisible(self))
    }

    values[idx] <<- list(NULL)
    n <<- n - 1L

    n_holes <<- n_holes + 1L
    holes[n_holes] <<- idx

    # Shrink the values list if there are more than 20 items and the values list
    # is more than half holes.
    if (n > 20 && n_holes * 2 > n) {
      compact()
    }

    invisible(self)
  }

  size <- function() {
    .Call(C_map_size, key_idx_map)
  }

  keys <- function() {
    .Call(C_map_keys, key_idx_map)
  }

  as_list <- function() {
    keys_idxs <- .Call(C_map_keys_idxs, key_idx_map)
    setNames(values[keys_idxs], names(keys_idxs))
  }

  compact <- function() {
    if (n_holes == 0L)
      return(invisible(self))

    keys_idxs <- .Call(C_map_keys_idxs, key_idx_map)

    # Suppose values is a length-7 list, n==3, holes==c(4,1,3,NA), n_holes==3
    # Drop any extra values stored in the holes vector.
    holes <<- holes[seq_len(n_holes)]

    remap_inv <- seq_len(length(values))
    remap_inv <- remap_inv[-holes]
    # remap_inv now is c(2, 5, 6, 7). It will be sorted.

    remap <- integer(length(values))
    remap[remap_inv] <- seq_along(remap_inv)
    # remap is now c(0,1,0,0,2,3,4). The non-zero values will be sorted.

    if (length(keys_idxs) != length(remap_inv)) {
      stop("length mismatch of keys_idxs and remap_inv")
    }
    keys <- names(keys_idxs)
    for (i in seq_along(keys)) {
      idx <- keys_idxs[[i]]
      .Call(C_map_set, key_idx_map, keys[i], remap[idx])
    }

    values <<- values[-holes]
    holes <<- integer()
    n_holes <<- 0L
    n <<- length(values)

    invisible(self)
  }

  list(
    set = set,
    mset = mset,
    get = get,
    mget = mget,
    exists = exists,
    remove = remove,
    keys = keys,
    size = size,
    as_list = as_list,
    compact = compact
  )
}
