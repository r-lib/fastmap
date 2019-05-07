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
