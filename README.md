fastmap
=======

**fastmap** is a package that implements _maps_ -- that is, key-value stores -- in R.

The usual way of doing this in R is to use environments.. However, this method is problematic when using a large set of keys or randomly-generated keys, because each time you use a key or even check for the existence of a key using `exists()`, that key is interned as a symbol and stored in the R symbol table, which is never garbage-collected. This means that every time you use a new key -- whether it is to store an object or just check whether the key exists in the environment, R leaks a little memory. If you have a relatively small, fixed set of keys, or if your R process is a short-running process, this may not be a problem. But if you have a long-running R process that uses random keys, then the memory leakage may be problematic.

**fastmap** solves this problem by storing the keys as C++ `std::string` objects, and so it does not use the R symbol table at all. The values are stored in a list so that R knows not to garbage-collect them. In C++, fastmap uses a `std::map` to map from the keys to indices in the list of values.

## Installation

```R
devtools::install_github("wch/fastmap")
```


## Usage

```R
library(fastmap)

# Create a map
m <- fastmap()

# Set some key-value pairs
m$set("key_1", 100)
m$set("abc", c("a", "b", "c"))
m$set("something", c(100, 200, 300))

# Get values using keys
m$get("key_1")
#> [1] 100
m$get("abc")
#> [1] "a" "b" "c"

# Missing keys currently return NULL (may change in the future)
m$get("xyz")
#> NULL

# Check for existence of keys
m$exists("abc")
#> [1] TRUE
m$exists("xyz")
#> [1] FALSE

# Remove items
m$remove("abc")

# Return number of items
m$size()
#> [1] 2

# Get all keys
m$keys()
#> [1] "key_1"     "something"

# Return named list that represents all key-value pairs
str(m$as_list())
#> $key_1
#> [1] 100
#> 
#> $something
#> [1] 100 200 300
```


## Notes

One important difference between using a `fastmap` object vs. an environment-backed map is that a fastmap can't be serialized since it includes an external pointer. That means that it can't be saved in one R session and restored in another. (It may be possible in the future to make this work.)

In a package, if a fastmap object is used, it can't be created and stored at build time. Instead of creating a fastmap object at build time, it should be created each time the package is loaded, in the package's `.onLoad()` function.



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
cat("Elapsed time:", round(end_time - start_time, 1), "seconds")
cat("Memory leaked:", end_mem - start_mem, "bytes")
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

The elapsed time gets progressively slower as the R symbol table gets larger and larger. After running the above code repeatedly, the elapsed time for the fifth run is 3.1 seconds.


For comparison, this example with fastmap is similar but does a little bit more. In each iteration, it checks for the existence of an object, sets a key-value pair.

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
    m$exists(x)
    m$set(x, x)
  }
  rm(m, x)
}
end_time <- as.numeric(Sys.time())
gc()
end_mem <- mem_used()
cat("Elapsed time:", round(end_time - start_time, 1), "seconds")
cat("Memory leaked:", end_mem - start_mem, "bytes")
```

The output looks something like this (note that this is from the second run of the code above -- for the first run, there is an increase in memory used, but I think it's probably related to code being run for the first time in the R session):

```
1: 42.3 MB
2: 42.3 MB
3: 42.3 MB
4: 42.3 MB
5: 42.3 MB
6: 42.3 MB
7: 42.3 MB
8: 42.3 MB
Elapsed time: 1.5 seconds
Memory leaked: 0 bytes
```

It does not leak memory, and it does not slow down if you run it repeatedly. After running it ten times, it still takes 1.5 seconds, and leaks no memory.
