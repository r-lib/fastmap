#' @useDynLib fastmap, .registration = TRUE
NULL

#' @export
fastmap <- function() {
  n <- 0L  # Total number of items
  values <- list()
  self <- environment()

  key_idx_map <- map_create()

  set <- function(key, value) {
    idx <- .Call(C_map_get, key_idx_map, key)

    if (idx == -1L) {
      # This is a new key
      n <<- n + 1L
      .Call(C_map_set, key_idx_map, key, n)
      idx <- n
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

  get <- function(key) {
    idx <- .Call(C_map_get, key_idx_map, key)
    if (idx == -1L) {
      return(NULL)
    }

    values[[idx]]
  }

  exists <- function(key) {
    idx <- .Call(C_map_get, key_idx_map, key)
    return(idx != -1L)
  }

  remove <- function(key) {
    idx <- .Call(C_map_remove, key_idx_map, key)
    if (idx != -1L) {
      values[idx] <<- list(NULL)
    }

    idx
  }

  keys <- function() {
    .Call(C_map_keys, key_idx_map)
  }

  as_list <- function() {
    keys_idxs <- .Call(C_map_keys_idxs, key_idx_map)
    setNames(values[keys_idxs], names(keys_idxs))
  }

  compact <- function() {


  }

  list(
    set = set,
    get = get,
    exists = exists,
    remove = remove,
    keys = keys,
    as_list = as_list,
    self = self
  )
}





#' @export
map_create <- function() {
  .Call(C_map_create)
}


#' @export
map_set <- function(map, key, idx) {
  .Call(C_map_set, map, key, idx)
}

#' @export
map_get <- function(map, key) {
  .Call(C_map_get, map, key)
}


#' @export
map_remove <- function(map, key) {
  .Call(C_map_remove, map, key)
}

