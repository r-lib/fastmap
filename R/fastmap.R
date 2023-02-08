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
#' Unlike with environments, the keys in a fastmap are always encoded as UTF-8,
#' so if you call `$set()` with two different strings that have the same Unicode
#' values but have different encodings, the second call will overwrite the first
#' value. If you call `$keys()`, it will return UTF-8 encoded strings, and
#' similarly, `$as_list()` will return a list with names that have UTF-8
#' encoding.
#'
#' Note that if you call `$mset()` with a named argument, where the name is
#' non-ASCII, R will convert the name to the native encoding before fastmap has
#' the chance to convert them to UTF-8, and the keys may get mangled in the
#' process. However, if you use `$mset(.list = x)`, then R will not convert the
#' keys to the native encoding, and the keys will be correctly converted to
#' UTF-8. With `$mget()`, the keys will be converted to UTF-8 before they are
#' fetched.
#'
#'
#' `fastmap` objects have the following methods:
#'
#' \describe{
#'   \item{`set(key, value)`}{
#'     Set a key-value pair. `key` must be a string. Returns `value`.
#'   }
#'   \item{`mset(..., .list = NULL)`}{
#'     Set multiple key-value pairs. The key-value pairs are named arguments,
#'     and/or a named list passed in as `.list`. Returns a named list where
#'     the names are the keys, and the values are the values.
#'   }
#'   \item{`get(key, missing = missing_default)`}{
#'     Get a value corresponding to `key`. If the key is not in the fastmap,
#'     return `missing`.
#'   }
#'   \item{`mget(keys, missing = missing_default)`}{
#'     Get values corresponding to `keys`, which is a character vector. The
#'     values will be returned in a named list where the names are the same as
#'     the `keys` passed in, in the same order. For keys not in the fastmap,
#'     they will have `missing` for their value.
#'   }
#'   \item{`has(keys)`}{
#'     Given a vector of keys, returns a logical vector reporting whether each
#'     key is contained in the fastmap
#'   }
#'   \item{`remove(keys)`}{
#'     Given a vector of keys, remove the key-value pairs from the fastmap Returns
#'     a logical vector reporting whether each item existed in (and was removed
#'     from) the fastmap.
#'   }
#'   \item{`keys(sort = FALSE)`}{
#'     Returns a character vector of all the keys. By default, the keys will be
#'     in arbitrary order. Note that the order can vary across platforms and is
#'     not guaranteed to be consistent. With `sort=TRUE`, the keys will be
#'     sorted according to their Unicode code point values.
#'   }
#'   \item{`size()`}{
#'     Returns the number of items in the map.
#'   }
#'   \item{`as_list(sort = FALSE)`}{
#'     Return a named list where the names are the keys from the map, and the
#'     values are the values. By default, the keys will be in arbitrary order.
#'     Note that the order can vary across platforms and is not guaranteed to
#'     be consistent. With `sort=TRUE`, the keys will be sorted according
#'     to their Unicode code point values.
#'   }
#'   \item{`reset()`}{
#'     Reset the fastmap object, clearing all items.
#'   }
#'   \item{`clone()`}{
#'     Returns a copy of the fastmap object. This is a shallow clone; objects in
#'     the fastmap will not be copied.
#'   }
#'   \item{`map(fn)`}{
#'     Apply a function with signature `function(v)` to each element, collect
#'     the return values, and return a new fastmap object with those values.
#'   }
#'   \item{`map_with_key(fn)`}{
#'     Apply a function with signature `function(k, v)` to each element, collect
#'     the return values, and return a new fastmap object with those values.
#'   }
#'   \item{`modify(fn)`}{
#'     Apply a function with signature `function(v)` to each element and update
#'     the key with the returned value. Returns `NULL`.
#'   }
#'   \item{`modify_with_key(fn)`}{
#'     Apply a function with signature `function(k, v)` to each element and
#'     update the key with the returned value. Returns `NULL`.
#'   }
#'   \item{`walk(fn)`}{
#'     Apply a function with signature `function(v)` to each element, and do not
#'     collect the return value. (Because it does not collect the return values,
#'     this can be faster than `map()` and `modify()`.) Returns `NULL`.
#'   }
#'   \item{`walk_with_key(fn)`}{
#'     Apply a function with signature `function(k, v)` to each element, and do
#'     not collect the return value. Returns `NULL`.
#'   }
#'   \item{`filter(fn)`}{
#'     Create a copy of the fastmap, keeping only elements for which `fn(v)`
#'     returns a truthy value. Returns a new fastmap object.
#'   }
#'   \item{`filter_with_key(fn)`}{
#'     Create a copy of the fastmap, keeping only elements for which `fn(k, v)`
#'     returns a truthy value. Returns a new fastmap object.
#'   }
#'   \item{`filter_key(fn)`}{
#'     Create a copy of the fastmap, keeping only elements for which `fn(k)`
#'     returns a truthy value. (This can be faster than `filter_with_key()`
#'     because it does not need to fetch the value of each element.) Returns a
#'     new fastmap object.
#'   }
#' }
#'
#' @param missing_default The value to return when `get()` is called with a key
#'   that is not in the map. The default is `NULL`, but in some cases it can be
#'   useful to return a sentinel value, such as a [`key_missing`] object.
#'
#' @examples
#' # Create the fastmap object
#' m <- fastmap()
#'
#' # Set some key-value pairs
#' m$set("x", 100)
#' m$set("letters", c("a", "b", "c"))
#' m$mset(numbers = c(10, 20, 30), nothing = NULL)
#'
#' # Get values using keys
#' m$get("x")
#' m$get("numbers")
#' m$mget(c("letters", "numbers"))
#'
#' # Missing keys return NULL by default, but this can be customized
#' m$get("xyz")
#'
#' # Check for existence of keys
#' m$has("x")
#' m$has("nothing")
#' m$has("xyz")
#'
#' # Remove one or more items
#' m$remove(c("letters", "x"))
#'
#' # Return number of items
#' m$size()
#'
#' # Get all keys
#' m$keys()
#'
#' # Return named list that represents all key-value pairs
#' str(m$as_list())
#'
#' # Clear the map
#' m$reset()
#'
#'
#' # Specify missing value when get() is called
#' m <- fastmap()
#' m$get("x", missing = key_missing())
#' #> <Key Missing>
#'
#' # Specify the default missing value
#' m <- fastmap(missing_default = key_missing())
#' m$get("x")
#' #> <Key Missing>
#'
#' @export
fastmap <- function(missing_default = NULL) {
  force(missing_default)

  # ===================================
  # Constants
  # ===================================
  INITIAL_SIZE <- 32L
  GROWTH_FACTOR <- 1.2
  SHRINK_FACTOR <- 2

  # ===================================
  # Internal state
  # ===================================

  # Number of items currently stored in the fastmap object.
  n <- NULL
  # External pointer to the C++ object that maps from key (a string) to index
  # into the list that stores the values (which can be any R object).
  key_idx_map <- NULL
  # A vector containing keys, where the keys are in the corresponding position
  # to the values in the values list. This is only used to repopulate the map
  # after the fastmap has been serialized and deserialized. It contains the same
  # information as key_idx_map, but, since it is a normal R object, it can be
  # saved and restored without any extra effort.
  keys_ <- NULL
  # Backing store for the R objects.
  values <- NULL
  # Indices in the list which are less than n and not currently occupied. These
  # occur when objects are removed from the map. When a hole is filled, the
  # entry is replaced with NA, and n_holes is updated to reflect it; this is
  # instead of simply shrinking the holes vector, because that involves copying
  # the entire object.
  # Has a structure like this, with numbers at the beginning and NA's at the
  # end:
  #   1 2 3 4 5 NA NA NA
  holes <- NULL
  # A number. For the example above, n_holes would be 5.
  n_holes <- NULL

  # ===================================
  # Methods
  # ===================================

  reset <- function() {
    n           <<- 0L
    key_idx_map <<- .Call(C_map_create)
    keys_       <<- rep(NA_character_, INITIAL_SIZE)
    values      <<- vector(mode = "list", INITIAL_SIZE)
    holes       <<- seq_len(INITIAL_SIZE)
    n_holes     <<- INITIAL_SIZE
    invisible(NULL)
  }
  reset()

  set <- function(key, value) {
    # Force evaluation of value here, so that, if it throws an error, the error
    # will not happen in the middle of this function and leave things in an
    # inconsistent state.
    force(value)

    ensure_restore_map()

    idx <- .Call(C_map_get, key_idx_map, key)

    if (idx == -1L) {
      # This is a new key.
      n <<- n + 1L

      # If we have any holes in our values list, store it there. Otherwise
      # append to the end of the values list.
      if (n_holes == 0L) {
        idx <- n
        # If we got here, we need to grow. This grows values, and holes is
        # updated to track it.
        grow()
      }

      idx <- holes[n_holes]
      holes[n_holes] <<- NA_integer_   # Mark as NA, for safety
      n_holes <<- n_holes - 1L

      .Call(C_map_set, key_idx_map, key, idx)
    }

    if (is.null(value)) {
      # Need to handle NULLs differently. Wrap them in a list so that this
      # doesn't result in deletion.
      values[idx] <<- list(NULL)
    } else {
      values[[idx]] <<- value
    }
    # Store the key, as UTF-8
    keys_[idx] <<- .Call(C_char_vec_to_utf8, key)

    invisible(value)
  }

  mset <- function(..., .list = NULL) {
    objs <- c(list(...), .list)
    keys <- names(objs)
    if (is.null(keys) || any(is.na(keys)) || any(keys == "")) {
      stop("mset: all values must be named.")
    }
    for (i in seq_along(objs)) {
      set(keys[i], objs[[i]])
    }

    invisible(objs)
  }

  get <- function(key, missing = missing_default) {
    ensure_restore_map()
    idx <- .Call(C_map_get, key_idx_map, key)
    if (idx == -1L) {
      return(missing)
    }

    values[[idx]]
  }

  mget <- function(keys, missing = missing_default) {
    if (is.null(keys)) {
      return(list(a=1)[0]) # Special case: return empty named list
    }
    if (!is.character(keys)) {
      stop("mget: `keys` must be a character vector or NULL")
    }

    # Make sure keys are encoded in UTF-8. Need this C function because iconv
    # doesn't work right for vectors with mixed encodings.
    keys <- .Call(C_char_vec_to_utf8, keys)
    res <- lapply(keys, get, missing)
    names(res) <- keys
    res
  }

  # Internal function
  has_one <- function(key) {
    ensure_restore_map()
    .Call(C_map_has, key_idx_map, key)
  }

  has <- function(keys) {
    if (!(is.character(keys) || is.null(keys))) {
      stop("mget: `keys` must be a character vector or NULL")
    }
    if (length(keys) == 1) {
      # In the common case of only one key, it's faster to avoid vapply.
      has_one(keys)
    } else {
      vapply(keys, has_one, FUN.VALUE = TRUE, USE.NAMES = FALSE)
    }
  }

  # Internal function
  remove_one <- function(key) {
    ensure_restore_map()
    idx <- .Call(C_map_remove, key_idx_map, key)
    if (idx == -1L) {
      return(FALSE)
    }

    values[idx] <<- list(NULL)
    keys_[idx] <<- NA_character_
    n <<- n - 1L

    n_holes <<- n_holes + 1L
    holes[n_holes] <<- idx

    # Shrink the values list if its length is larger than 32 and it is half or
    # more empty.
    values_length <- length(values)
    if (values_length > INITIAL_SIZE  &&  values_length >= n * SHRINK_FACTOR) {
      shrink()
    }

    TRUE
  }

  remove <- function(keys) {
    if (!(is.character(keys) || is.null(keys))) {
      stop("remove: `keys` must be a character vector or NULL")
    }
    if (any(keys == "") || any(is.na(keys))) {
      stop('remove: `keys` must not be "" or NA')
    }
    if (length(keys) == 1) {
      # In the common case of only one key, it's faster to avoid vapply.
      invisible(remove_one(keys))
    } else {
      invisible(vapply(keys, remove_one, FUN.VALUE = TRUE, USE.NAMES = FALSE))
    }
  }

  size <- function() {
    n
  }

  keys <- function(sort = FALSE) {
    # The implementation below is equivalent to the following, but about 100x
    # faster:
    #   ensure_restore_map()
    #   .Call(C_map_keys, key_idx_map, sort)

    k <- keys_[non_hole_idxs_()]
    if (sort) {
      k <- sort(k, method = "radix")
    }
    k
  }

  clone <- function() {
    m <- fastmap()
    # Use this env to access internal objects of the new fastmap object.
    e <- environment(m$get)
    e$ensure_restore_map()

    e$n           <- n
    e$key_idx_map <- .Call(C_map_copy, key_idx_map)
    e$keys_       <- keys_
    e$values      <- values
    e$holes       <- holes
    e$n_holes     <- n_holes

    m
  }

  # This implementation of map_values() uses clone(), which is very fast, and
  # then calls fn() on each value in the backing list, without looking up each
  # key-to-index mapping via key_idx_map. This should be faster than if we had
  # to look up the key-to-index mapping for each item.
  map <- function(fn) {
    m <- clone()
    m$modify(fn)
    m
  }

  map_with_key <- function(fn) {
    m <- clone()
    m$modify_with_key(fn)
    m
  }


  # Like map(), but modifies the values in place.
  # Returns NULL.
  modify <- function(fn) {
    idxs <- non_hole_idxs_()
    for (i in idxs) {
      values[[i]] <<- fn(values[[i]])
    }
    invisible()
  }

  # Like map_with_key(), but modifies the values in place.
  # Returns NULL.
  modify_with_key <- function(fn) {
    idxs <- non_hole_idxs_()
    for (i in idxs) {
      values[[i]] <<- fn(keys_[[i]], values[[i]])
    }
    invisible()
  }


  # Invoke a function on each value, but don't collect the return values.
  # Returns NULL.
  walk <- function(fn) {
    idxs <- non_hole_idxs_()
    for (i in idxs) {
      fn(values[[i]])
    }
    invisible()
  }

  # Like walk(), but fn takes args (key, value)
  walk_with_key <- function(fn) {
    idxs <- non_hole_idxs_()
    for (i in idxs) {
      fn(keys_[[i]], values[[i]])
    }
    invisible()
  }



  # Filter values. This implementation first clones the original map (which is
  # very fast) and then removes items that fail the user-provided filter
  # function. It is fastest when there are no items to remove. It would be
  # possible to provide an option to build up the new fastmap by adding each
  # item that passes the filter function; this would be faster when most items
  # fail the filter.
  filter <- function(fn) {
    m <- clone()
    for (key in m$keys()) {
      if (!fn(m$get(key))) {
        m$remove(key)
      }
    }
    m
  }

  filter_with_key <- function(fn) {
    m <- clone()
    for (key in m$keys()) {
      if (!fn(key, m$get(key))) {
        m$remove(key)
      }
    }
    m
  }

  filter_key <- function(fn) {
    m <- clone()
    for (key in m$keys()) {
      if (!fn(key)) {
        m$remove(key)
      }
    }
    m
  }

  # Returns a named list
  as_list <- function(sort = FALSE) {
    idxs <- non_hole_idxs_()
    result <- values[idxs]
    names(result) <- keys_[idxs]
    if (sort) {
      result <- result[order(names(result))]
    }
    result
  }


  # Internal function
  #
  # Returns a vector containing the indices of the non-holes in the keys_ and
  # values lists.
  non_hole_idxs_ <- function() {
    # Need to special case when 0 holes, because x[-integer(0)] (as done in
    # the other code path) returns an empty vector, instead of the original x.
    if (n_holes == 0) {
      seq_along(keys_)
    } else {
      h <- holes[seq_len(n_holes)]
      seq_along(keys_)[-h]
    }
  }

  # Internal function
  grow <- function() {
    old_values_length <- length(values)
    new_values_length <- as.integer(ceiling(old_values_length * GROWTH_FACTOR))

    # Increase size of values list by assigning NULL past the end. On R 3.4 and
    # up, this will grow it in place.
    values[new_values_length] <<- list(NULL)
    keys_[new_values_length] <<- NA_character_

    # When grow() is called, `holes` is all NAs, but it's not as long as values.
    # Grow it (possibly in place, depending on R version) to new_values_length,
    # and have it point to all the new empty spaces in `values`. Strictly
    # speaking, it doesn't have to be as large as new_values_length -- it only
    # needs to be of size (new_values_length - new_values_length /
    # SHRINK_FACTOR), but it's possible that there will be a rounding error and
    # I'm playing it safe here.
    holes[new_values_length] <<- NA_integer_
    n_holes <<- new_values_length - old_values_length
    holes[seq_len(n_holes)] <<- seq.int(from = old_values_length + 1, to = new_values_length)
  }

  # Internal function
  shrink <- function() {
    if (n_holes == 0L)
      return(invisible())

    keys_idxs <- .Call(C_map_keys_idxs, key_idx_map, FALSE)

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
    keys_ <<- keys_[-holes]
    holes <<- integer()
    n_holes <<- 0L
    n <<- length(values)
  }

  # Internal function. This is useful after a fastmap is deserialized. When that
  # happens, the key_idx_map xptr will be NULL, and it needs to be repopulated.
  # Every external-facing method that makes use of key_idx_map should call this
  # before doing any operations on it.
  ensure_restore_map <- function() {
    # If the key_idx_map pointer is not NULL, just return.
    if (!.Call(C_xptr_is_null, key_idx_map)) {
      return(invisible())
    }

    # Repopulate key_idx_map.
    key_idx_map <<- .Call(C_map_create)
    idxs <- non_hole_idxs_()
    for (idx in idxs) {
      .Call(C_map_set, key_idx_map, keys_[idx], idx)
    }
  }

  list(
    reset = reset,
    set = set,
    mset = mset,
    get = get,
    mget = mget,
    has = has,
    remove = remove,
    keys = keys,
    size = size,
    clone = clone,
    as_list = as_list,

    map = map,
    map_with_key = map_with_key,

    modify = modify,
    modify_with_key = modify_with_key,

    walk = walk,
    walk_with_key = walk_with_key,

    filter = filter,
    filter_with_key = filter_with_key,
    filter_key = filter_key
  )
}
