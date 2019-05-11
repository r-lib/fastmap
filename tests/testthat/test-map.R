# Given a fastmap object, return the enclosing environment of its methods.
get_self <- function(m) {
  environment(m$set)
}


test_that("General correctness", {
  m <- fastmap()
  m$set("asdf", c(1, 2, 3))
  m$set("foo", "blah")

  expect_equal(m$get("asdf"), c(1, 2, 3))
  expect_mapequal(
    m$as_list(),
    list("asdf" = c(1, 2, 3), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)

  # Removal
  m$remove("asdf")
  expect_equal(m$get("asdf"), NULL)
  expect_mapequal(
    m$as_list(),
    list("foo"= "blah")
  )
  expect_false(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 1L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)
  # Removing non-existent key has no effect
  m$remove("asdf")
  expect_equal(m$get("asdf"), NULL)

  # Adding back
  m$set("asdf", list("a", "b"))
  expect_equal(m$get("asdf"), list("a", "b"))
  expect_mapequal(
    m$as_list(),
    list("asdf" = list("a", "b"), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)


  # Replacing existing object
  m$set("asdf", list("x", "y"))
  expect_equal(m$get("asdf"), list("x", "y"))
  expect_mapequal(
    m$as_list(),
    list("asdf" = list("x", "y"), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)

  # NULL handling
  m$set("asdf", NULL)
  expect_equal(m$get("asdf"), NULL)
  expect_true(m$exists("asdf"))
  expect_mapequal(
    m$as_list(),
    list("asdf" = NULL, "foo"= "blah")
  )
})


test_that("reset", {
  m <- fastmap()
  m$set("a", 1)
  m$set("b", 2)
  m$reset()
  expect_equal(m$as_list(), list(a=1)[0])
  expect_equal(m$size(), 0)
})


test_that("mset and mget", {
  m <- fastmap()
  m$set("c", 3)
  m$mset(b = -2, a = 1)
  m$mset(b = 2, .list = list(e = 5))

  expect_mapequal(
    m$as_list(),
    list(a=1, b=2, c=3, e=5)
  )

  expect_equal(
    m$mget(c("e", "c", "a")),
    list(e=5, c=3, a=1)
  )
})


test_that("Missing keys", {
  m <- fastmap()
  expect_identical(m$get("a"), NULL)
  expect_identical(m$get("a", missing = key_missing()), key_missing())
  expect_identical(m$mget(c("a", "b")), list(a = NULL, b = NULL))
  expect_identical(
    m$mget(c("a", "b"), missing = key_missing()),
    list(a = key_missing(), b = key_missing())
  )

  # With a different default for missing
  m <- fastmap(missing_default = key_missing())
  expect_identical(m$get("a"), key_missing())
  expect_true(is.key_missing(m$get("a")))

  expect_identical(m$get("a", missing = NULL), NULL)
  expect_identical(m$get("a"), key_missing())
  expect_identical(m$mget(c("a", "b")), list(a = key_missing(), b = key_missing()))
  expect_identical(
    m$mget(c("a", "b"), missing = NULL),
    list(a = NULL, b = NULL)
  )
})


test_that("Malformed keys", {
  m <- fastmap()
  expect_error(m$set(1, 123))
  expect_error(m$set(TRUE, 123))
  expect_error(m$set(NA_character_, 123))
  expect_error(m$set(NA_integer_, 123))
  expect_error(m$set("", 123))
  expect_error(m$set(character(0), 123))
  expect_error(m$set(numeric(0), 123))
  expect_error(m$set(NULL, 123))

  expect_error(m$get(1))
  expect_error(m$get(TRUE))
  expect_error(m$get(NA_character_))
  expect_error(m$get(NA_integer_))
  expect_error(m$get(""))
  expect_error(m$get(character(0)))
  expect_error(m$get(numeric(0)))
  expect_error(m$get(NULL))

  expect_mapequal(m$mget(NULL), list()) # Fix this test. Should we allow NULL?
  expect_error(m$mget(c(1, 2)))
  expect_error(m$mget(c("A", "")))
  expect_error(m$mget(c("A", NA)))

  expect_error(m$remove(NA_character_))
  expect_error(m$remove(NA_integer_))
  expect_error(m$remove(""))
  expect_error(m$remove(character(0)))
  expect_error(m$remove(numeric(0)))
  expect_error(m$remove(NULL))

  # When removing multiple, one bad key shouldn't stop others from being removed
  m$mset(a=1, b=2, c=3)

  expect_error(m$remove(c("a", NA, "c")))
  expect_mapequal(m$as_list(), list(b=2))

  # Key or value unspecified
  m$set("a")
  m$set(value = 123)
})
