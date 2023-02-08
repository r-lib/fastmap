fastmap
=======

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/fastmap)](https://cran.r-project.org/package=fastmap)
[![R build status](https://github.com/r-lib/fastmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/fastmap/actions)
<!-- badges: end -->

**fastmap** implements the following data structures for R:

* `fastmap`: maps (key-value store)
* `faststack`: stacks
* `fastqueue`: queues

The usual way of implementing maps in R is to use environments. However, this method is problematic when using a large set of keys or randomly-generated keys, because each time you use a key or even check for the existence of a key using `exists()`, that key is interned as a symbol and stored in the R symbol table, which is never garbage-collected. This means that every time you use a new key -- whether it is to store an object or just check whether the key exists in the environment, R leaks a little memory. If you have a relatively small, fixed set of keys, or if your R process is a short-running process, this may not be a problem. But if, for example, you have a long-running R process that uses random keys, then the memory leakage can cause a noticeable increase in memory usage. Also, when R's symbol table is large, garbage collection events, which occur regularly, take more time, reducing R's performance in general. (See the _Memory leak examples_ section of this document for more information.)

**fastmap** solves this problem by storing the keys as C++ `std::string` objects, and so it does not use the R symbol table at all. The values are stored in a list so that R knows not to garbage-collect them. In C++, fastmap uses a [`tsl::hopscotch_map`](https://github.com/Tessil/hopscotch-map/) (which is similar to `std::unordered_map`) to map from the keys to indices in the list of values.

## Installation

```R
install.packages("fastmap")
```


## API

`fastmap` objects are unordered maps. The keys are UTF-8 strings, and the values are any R object.

To create a fastmap object:

```
m <- fastmap()
```

`fastmap` objects have the following methods:

* `set(key, value)`: Set a key-value pair. `key` must be a string. Returns `value`.
* `mset(..., .list = NULL)`: Set multiple key-value pairs.  The key-value pairs are named arguments, and/or a named list passed in as `.list`.
* `get(key, missing = missing_default)`: Get the value for `key`. If the key is not in the fastmap, return `missing`.
* `mget(keys, missing = missing_default)`: Get values corresponding to `keys`, which is a character vector. The values will be returned in a named list where the names are the same as the `keys` passed in, in the same order. For keys not in the fastmap, they will have `missing` for their value.
* `has(keys)`: Given a vector of keys, returns a logical vector reporting whether each key is contained in the fastmap.
* `remove(keys)`: Given a vector of keys, remove the key-value pairs from the fastmap. Returns a logical vector reporting whether each item existed in (and was removed from) the fastmap.
* `keys(sort = FALSE)`: Returns a character vector of all the keys. By default, the keys will be in an arbitrary order. Note that the order can vary across platforms and is not guaranteed to be consistent. With `sort=TRUE`, the keys will be sorted according to their Unicode code point values.
* `size()`: Return the number of items in the fastmap.
* `as_list(sort = FALSE)`: Return a named list with the items in the fastmap. By default, the keys will be in an arbitrary order. Note that the order can vary across platforms and is not guaranteed to be consistent. With `sort=TRUE`, the keys will be sorted according to their Unicode code point values.
* `reset()`: Reset the fastmap object, clearing all items.
* `clone()`: Create a (shallow) copy of this fastmap.

* Functional programming methods. Note that these methods apply functions in an unspecified order. The order is not guaranteed to be consistent.
  * `map(fn)`: Apply a function with signature `function(v)` to each element, collect the return values, and return a new fastmap object with those values.
  * `map_with_key(fn)`: Apply a function with signature `function(k, v)` to each element, collect the return values, and return a new fastmap object with those values.

  * `modify(fn)`: Apply a function with signature `function(v)` to each element and update the key with the returned value. Returns `NULL`.
  * `modify_with_key(fn)` Apply a function with signature `function(k, v)` to each element and update the key with the returned value. Returns `NULL`.
  * `walk(fn)`: Apply a function with signature `function(v)` to each element, and do not collect the return value. (Because it does not collect the return values, this can be faster than `map()` and `modify()`.) Returns `NULL`.
  * `walk_with_key(fn)`: Apply a function with signature `function(k, v)` to each element, and do not collect the return value. Returns `NULL`.
  * `filter(fn)`: Create a copy of the fastmap, keeping only elements for which `fn(v)` returns a truthy value. Returns a new fastmap object.
  * `filter_with_key(fn)`: Create a copy of the fastmap, keeping only elements for which `fn(k, v)` returns a truthy value. Returns a new fastmap object.
  * `filter_key(fn)`: Create a copy of the fastmap, keeping only elements for which `fn(k)` returns a truthy value. (This can be faster than `filter_with_key()` because it does not need to fetch the value of each element.) Returns a new fastmap object.


## Example usage

### `fastmap()`

```R
library(fastmap)

# Create a map
m <- fastmap()

# Set some key-value pairs
m$set("x", 100)
m$set("letters", c("a", "b", "c"))
m$mset(numbers = c(10, 20, 30), nothing = NULL)

# Get values using keys
m$get("x")
#> [1] 100
m$get("numbers")
#> [1] 10 20 30
m$mget(c("letters", "numbers"))
#> $letters
#> [1] "a" "b" "c"
#>
#> $numbers
#> [1] 10 20 30

# Missing keys return NULL by default, but this can be customized
m$get("xyz")
#> NULL

# Check for existence of keys
m$has("x")
#> [1] TRUE
m$has("nothing")
#> [1] TRUE
m$has("xyz")
#> [1] FALSE

# Remove one or more items
m$remove(c("letters", "x"))

# Return number of items
m$size()
#> [1] 2

# Get all keys
m$keys()
#> [1] "nothing" "numbers"

# Return named list that represents all key-value pairs
str(m$as_list())
#> List of 3
#>  $ nothing: NULL
#>  $ numbers: num [1:3] 10 20 30

# Clear the map
m$reset()

# Create a copy of the fastmap
m1 <- m$clone()
```

By default, `get()` returns `NULL` for keys that aren't present. You can instead specify a sentinel value to return for missing keys, either when the fastmap is created, or when `get()` is called. For example, you can return a `key_missing()` object to represent missing values:

```R
# Specify missing value when get() is called
m <- fastmap()
m$get("x", missing = key_missing())
#> <Key Missing>

# Specify the default missing value
m <- fastmap(missing_default = key_missing())
m$get("x")
#> <Key Missing>
```

### `faststack()`

```R
s <- faststack()
s$push(10)
s$mpush(11, 12, 13)
s$mpush(.list = list(14, 15))

s$pop()
#> [1] 15

str(s$mpop(3))
#> List of 3
#>  $ : num 14
#>  $ : num 13
#>  $ : num 12

s$peek()
#> [1] 11

s$size()
#> [1] 2

# Get the stack in list form. Note that the order is the opposite of $mpop()
str(s$as_list())
#> List of 2
#>  $ : num 10
#>  $ : num 11

s$reset()
```

By default, popping from an empty stack returns `NULL`, but you can specify other values.

```R
s$pop()
#> NULL

# Can specify the default missing value at creation.
s <- faststack(missing_default = key_missing())
s$pop()
#> <Key Missing>

# Can specify a missing value when $pop is called
s$pop(missing = "nope")
#> [1] "nope"
```

### `fastqueue()`

```R
q <- fastqueue()
q$add(10)
q$madd(11, 12, 13)
q$madd(.list = list(14, 15))

q$remove()
#> [1] 10

str(q$mremove(3))
#> List of 3
#>  $ : num 11
#>  $ : num 12
#>  $ : num 13

q$peek()
#> [1] 14

q$size()
#> [1] 2

# Get the queue in list form.
str(q$as_list())
#> List of 2
#>  $ : num 14
#>  $ : num 15

q$reset()
```

By default, removing from an empty queue returns `NULL`, but you can specify other values.

```R
q$remove()
#> NULL

# Can specify the default missing value at creation.
q <- fastqueue(missing_default = key_missing())
q$remove()
#> <Key Missing>

# Can specify a missing value when $pop is called
q$remove(missing = "nope")
#> [1] "nope"
```


## Notes on `fastmap` objects

### Key ordering

When you call `m$keys()` or `m$as_list()`, the items are returned in an arbitrary order. Keep in mind that there is no guarantee that the order will be the same across platforms, or across different builds of fastmap.

If you want to guarantee a particular order, you can call `m$keys(sort=TRUE)` or `m$as_list(sort=TRUE)`. The result will be a locale-independent sorting of the keys by their Unicode code point values. For example, `é` (Unicode code point 233) comes after `z` (122). If you want the keys to be sorted a different way, you will need to sort them yourself.

### Serialization

A `fastmap` object can be serialized (or saved) in one R session and deserialized (or loaded) in another. For performance, the data structure that tracks the mapping between keys and values is implemented in C++, and this data structure will not be serialized, but fastmap also keeps a copy of the same information in an ordinary R vector, which will be serialized. After a fastmap object is deserialized, the C++ data structure will not exist, but the first time any method on the fastmap is called, the C++ data structure will be rebuilt using information from the R vector.

The vector is much slower for lookups, and so it is used only for restoring the C++ data structure after a fastmap object is deserialized or loaded.

### Key encoding

Unlike with environments, the keys in a fastmap are always encoded as UTF-8, so if you call `m$set()` with two different strings that have the same Unicode values but have different encodings, the second call will overwrite the first value. If you call `m$keys()`, it will return UTF-8 encoded strings, and similarly, `m$mget()` and `m$as_list()` will return lists with names that have UTF-8 encoding.

### Testing for equality

The base R functions `identical()` and `all.equal()` are commonly used to test two objects for equality, but they will not work correctly for fastmap objects. `identical()` will always report `FALSE` for two distinct fastmap objects, even if they have the same contents, while `all.equal()` will always report `TRUE` for two fastmap objects.

To test whether two fastmap objects have the same contents, compare the results of `$as_list(sort=TRUE)` for both of the objects. For example:

```
identical(a$as_list(sort = TRUE), b$as_list(sort = TRUE))
# or
all.equal(a$as_list(sort = TRUE), b$as_list(sort = TRUE))
```

These comparisons are subject to the technical details of how `identical()` and `all.equal()` treat named lists.


## Memory leak examples

This example shows how using a regular R environment leaks memory, even when simply checking for the existence of a key.

```R
library(pryr)
gc()
start_mem <- mem_used()
start_time <- as.numeric(Sys.time())
for (i in 1:8) {
  cat(i, ": ", sep = "")
  print(mem_used())
  e <- new.env(parent = emptyenv())
  for (j in 1:10000) {
    # Generate random key
    x <- as.character(runif(1))
    exists(x, envir = e, inherits = FALSE)
  }
  rm(e, x)
}
end_time <- as.numeric(Sys.time())
gc()
end_mem <- mem_used()
cat("Elapsed time:", round(end_time - start_time, 1), "seconds\n")
cat("Memory leaked:", end_mem - start_mem, "bytes\n")
```

The output looks something like this:

```
1: 57.9 MB
2: 59.9 MB
3: 61.9 MB
4: 64.4 MB
5: 66.4 MB
6: 68.4 MB
7: 70.4 MB
8: 72.4 MB
Elapsed time: 1.1 seconds
Memory leaked: 16243656 bytes
```

The elapsed time gets progressively slower as the R symbol table gets larger and larger. After running the above code repeatedly, the elapsed time for the fifth run is 3.1 seconds. If you profile the code with [profvis](https://rstudio.github.io/profvis/), you can see that most of the slowdown is not with environment operations themselves, but with garbage collection events. This slowdown appears to affect all GC events, even when no environment-related operations are performed between one GC and the next.


For comparison, this example with fastmap does the same thing.

```R
library(fastmap)
library(pryr)
gc()
start_mem <- mem_used()
start_time <- as.numeric(Sys.time())
for (i in 1:8) {
  cat(i, ": ", sep = "")
  print(mem_used())
  m <- fastmap()
  for (j in 1:10000) {
    x <- as.character(runif(1))
    m$has(x)
  }
  rm(m, x)
}
end_time <- as.numeric(Sys.time())
gc()
end_mem <- mem_used()
cat("Elapsed time:", round(end_time - start_time, 1), "seconds\n")
cat("Memory leaked:", end_mem - start_mem, "bytes\n")
```

The output in a new R session looks something like this (note that this is from the second run of the code above -- for the first run, there is an increase in memory used, but it is probably related to code being run for the first time in the R session):

```
1: 42.3 MB
2: 42.3 MB
3: 42.3 MB
4: 42.3 MB
5: 42.3 MB
6: 42.3 MB
7: 42.3 MB
8: 42.3 MB
Elapsed time: 0.9 seconds
Memory leaked: 0 bytes
```

It does not leak memory, and it does not slow down if you run it repeatedly. After running it ten times, it still takes 0.9 seconds, and leaks no memory.


The simple tests above simply check for the existence of keys, but with setting values, the results are similar.

Note that the environment operations are themselves slightly faster than the fastmap operations, but the penalty is in slower garbage collection when many keys have been used. Also keep in mind that these tests are very artificial and use tens of thousands of random keys; if your application does not do this, then fastmap may have no practical benefit. In general, these operations are so fast that performance bottlenecks almost always lie elsewhere.


## Testing your code for symbol leakage

If you want to test your code directly for symbol leakage, you can use the code below. (Note: This only works on Mac.)

The `get_symbols()` function returns all symbols that are registered in R's symbol table.

`new_symbols()` returns all symbols that have been added since the last time `new_symbols()` was run. If you want to test whether your code causes the symbol table to grow, run `new_symbols()`, then run your code, then run `new_symbols()` again.


```R
# Note: this will only compile on a Mac. `R_SymbolTable` is not an exported
# symbol from Defn.h, but the a Mac, the linker exports all C symbols.

get_symbols <- inline::cfunction(
  includes = "
    #define HSIZE   49157 /* The size of the hash table for symbols, from Defn.h */
    extern SEXP* R_SymbolTable;
  ",
  body = "
    int symbol_count = 0;
    SEXP s;
    int j;
    for (j = 0; j < HSIZE; j++) {
      for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
        if (CAR(s) != R_NilValue) {
          symbol_count++;
        }
      }
    }


    SEXP result = PROTECT(Rf_allocVector(STRSXP, symbol_count));
    symbol_count = 0;
    for (j = 0; j < HSIZE; j++) {
      for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
        if (CAR(s) != R_NilValue) {
          SET_STRING_ELT(result, symbol_count, PRINTNAME(CAR(s)));
          symbol_count++;
        }
      }
    }

    UNPROTECT(1);
    return result;
  "
)

# Test it out
get_symbols()


# new_symbols() returns a character vector of symbols that have been added since
# the last time it was run.
last_symbols <- get_symbols()
new_symbols <- function() {
  cur_symbols <- get_symbols()
  res <- setdiff(cur_symbols, last_symbols)
  last_symbols <<- cur_symbols
  res
}

# Example

# The first couple times it's run, R might do something that adds symbols, like
# load the compiler package. Run it a bunch of times until it returns
# character(0).
new_symbols()
new_symbols()
new_symbols()
# character(0)


# After R stops loading things, run our code and see which new symbols have
# been added.
abcdefg <- 1
exists("xyz")
new_symbols()
#> [1] "abcdefg" "xyz"
```
