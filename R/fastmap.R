#' @useDynLib fastmap, .registration = TRUE
NULL


#' Create a fastmap object
#'
#' A fastmap object provides a key-value store where the keys are strings and
#' the values are any R objects.
#'
#' In R, it is common to use environments as key-value stores, but they can leak
#' memory: every time a new key is used, R registers it in its global symbol
#' table, which only grows and is never garbage collected. If many different
#' keys are used, this can cause a non-trivial amount of memory leakage.
#'
#' Fastmap objects do not use the symbol table and do not leak memory.
#'
#' One important difference between environments and fastmaps is that
#' environments can be serialized and restored in another R session, while
#' fastmaps cannot (though this may be added in the future).
#'
#' Fastmap objects have the following methods:
#'
#' \describe{
#'   \item{\code{set(key, value)}}{
#'     Set a key-value pair. \code{key} must be a string. Returns \code{value}.
#'   }
#'   \item{\code{mset(..., .list = NULL)}}{
#'     Set multiple key-value pairs. The key-value pairs are named arguments,
#'     and/or a list passed in as \code{.list}. Returns a named list where the
#'     names are the keys, and the values are the values.
#'   }
#'   \item{\code{get(key, missing = missing_default)}}{
#'     Get a value corresponding to \code{key}. If the key is not in the map,
#'     return \code{missing}.
#'   }
#'   \item{\code{mget(keys, missing = missing_default)}}{
#'     Get values corresponding to \code{keys}, which is a character vector. The
#'     values will be returned in a named list where the names are the same as
#'     the \code{keys} passed in, in the same order. For keys not in the map,
#'     they will have \code{missing} for their value.
#'   }
#'   \item{\code{exists(keys)}}{
#'     Given a vector of keys, returns a logical vector reporting whether each
#'     key is contained in the map.
#'   }
#'   \item{\code{remove(keys)}}{
#'     Given a vector of keys, remove the key-value pairs from the map. Returns
#'     a logical vector reporting whether each item existed in (and was removed
#'     from) the map.
#'   }
#'   \item{\code{keys()}}{
#'     Returns a character vector of all the keys, in unspecified order.
#'   }
#'   \item{\code{size()}}{
#'     Returns the number of items in the map.
#'   }
#'   \item{\code{as_list()}}{
#'     Return a named list where the names are the keys from the map, and the
#'     values are the values, in unspecified order.
#'   }
#'   \item{\code{reset()}}{
#'     Reset the fastmap object, clearing all items.
#'   }
#' }
#'
#' @param missing_default The value to return when \code{get()} is called with a
#'   key that is not in the map. The default is \code{NULL}, but in some cases
#'   it can be useful to return a sentinel value, such as a
#'   \code{\link{key_missing}} object.
#'
#' @examples
#' # Create the fastmap object
#' m <- fastmap()
#'
#' @export
fastmap <- function(missing_default = NULL) {
  force(missing_default)

  # Number of items currently stored in the fastmap object.
  n <- NULL
  # Mapping from key (a string) to index into the list that stores the values
  # (which can be any R object).
  key_idx_map <- NULL
  # The backing store for the R objects.
  values <- NULL
  # Indices in the list which are less than n and not currently occupied. These
  # occur when objects are removed from the map.
  holes <- NULL
  n_holes <- NULL
  self <- environment()

  reset <- function() {
    n <<- 0L
    key_idx_map <<- .Call(C_map_create)
    values <<- list()
    holes <<- integer()
    n_holes <<- 0L
    invisible(NULL)
  }
  reset()

  set <- function(key, value) {
    # Force evaluation of value here, so that, if it throws an error, the error
    # will not happen in the middle of this function and leave things in an
    # inconsistent state.
    force(value)

    idx <- .Call(C_map_get, key_idx_map, key)

    if (idx == -1L) {
      # This is a new key.
      n <<- n + 1L

      # If we have any holes in our values list, store it there. Otherwise
      # append to the end of the values list.
      if (n_holes != 0L) {
        idx <- holes[n_holes]
        holes[n_holes] <<- NA_integer_   # Mark as NA, for safety
        n_holes <<- n_holes - 1L
      } else {
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

    invisible(value)
  }

  mset <- function(..., .list = NULL) {
    objs <- c(list(...), .list)
    keys <- names(objs)
    if (is.null(keys) || any(keys == "")) {
      stop("mset: all values must be named.")
    }
    for (i in seq_along(objs)) {
      set(keys[i], objs[[i]])
    }

    invisible(objs)
  }

  get <- function(key, missing = missing_default) {
    idx <- .Call(C_map_get, key_idx_map, key)
    if (idx == -1L) {
      return(missing)
    }

    values[[idx]]
  }

  mget <- function(keys, missing = missing_default) {
    if (!(is.character(keys) || is.null(keys))) {
      stop("mget: `keys` must be a character vector or NULL")
    }
    names(keys) <- keys
    lapply(keys, get, missing)
  }

  # Internal function
  exists_one <- function(key) {
    idx <- .Call(C_map_get, key_idx_map, key)
    return(idx != -1L)
  }

  exists <- function(keys) {
    if (!(is.character(keys) || is.null(keys))) {
      stop("mget: `keys` must be a character vector or NULL")
    }
    vapply(keys, exists_one, FUN.VALUE = TRUE, USE.NAMES = FALSE)
  }

  # Internal function
  remove_one <- function(key) {
    idx <- .Call(C_map_remove, key_idx_map, key)
    if (idx == -1L) {
      return(FALSE)
    }

    values[idx] <<- list(NULL)
    n <<- n - 1L

    n_holes <<- n_holes + 1L
    holes[n_holes] <<- idx

    # Shrink the values list if its length is larger than 40 and it is half or
    # more empty.
    values_length <- length(values)
    if (values_length > 40L  &&  values_length >= n * 2L) {
      compact()
    }

    TRUE
  }

  remove <- function(keys) {
    if (!(is.character(keys) || is.null(keys))) {
      stop("mget: `keys` must be a character vector or NULL")
    }
    if (any(keys == "") || any(is.na(keys))) {
      stop('mget: `keys` must not be "" or NA')
    }
    vapply(keys, remove_one, FUN.VALUE = TRUE, USE.NAMES = FALSE)
  }

  size <- function() {
    n
  }

  keys <- function() {
    .Call(C_map_keys, key_idx_map)
  }

  as_list <- function() {
    keys_idxs <- .Call(C_map_keys_idxs, key_idx_map)
    result <- values[keys_idxs]
    names(result) <- names(keys_idxs)
    result
  }

  # Internal function
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
    reset = reset,
    set = set,
    mset = mset,
    get = get,
    mget = mget,
    exists = exists,
    remove = remove,
    keys = keys,
    size = size,
    as_list = as_list
  )
}


#' A Key Missing object
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
