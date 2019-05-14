
test_that("General correctness", {
  m <- fastmap()
  expect_identical(m$set("asdf", c(1, 2, 3)), c(1, 2, 3))
  expect_identical(m$set("foo", "blah"), "blah")

  expect_equal(m$get("asdf"), c(1, 2, 3))
  expect_mapequal(
    m$as_list(),
    list("asdf" = c(1, 2, 3), "foo"= "blah")
  )
  expect_true(m$has("asdf"))
  expect_true(m$has("foo"))
  expect_false(m$has("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)

  # Removal
  expect_true(m$remove("asdf"))
  expect_equal(m$get("asdf"), NULL)
  expect_mapequal(
    m$as_list(),
    list("foo"= "blah")
  )
  expect_false(m$has("asdf"))
  expect_true(m$has("foo"))
  expect_false(m$has("bar"))
  expect_identical(m$size(), 1L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)
  # Removing non-existent key has no effect
  expect_false(m$remove("asdf"))
  expect_equal(m$get("asdf"), NULL)

  # Adding back
  m$set("asdf", list("a", "b"))
  expect_equal(m$get("asdf"), list("a", "b"))
  expect_mapequal(
    m$as_list(),
    list("asdf" = list("a", "b"), "foo"= "blah")
  )
  expect_true(m$has("asdf"))
  expect_true(m$has("foo"))
  expect_false(m$has("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)


  # Replacing existing object
  m$set("asdf", list("x", "y"))
  expect_equal(m$get("asdf"), list("x", "y"))
  expect_mapequal(
    m$as_list(),
    list("asdf" = list("x", "y"), "foo"= "blah")
  )
  expect_true(m$has("asdf"))
  expect_true(m$has("foo"))
  expect_false(m$has("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(get_self(m)$values) - get_self(m)$n_holes)

  # NULL handling
  m$set("asdf", NULL)
  expect_equal(m$get("asdf"), NULL)
  expect_true(m$has("asdf"))
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


test_that("Vectorized operations", {
  m <- fastmap()
  expect_identical(m$set("c", 3), 3)
  expect_identical(m$mset(b = -2, a = 1), list(b = -2, a = 1))
  expect_identical(m$mset(b = 2, .list = list(e = 5)), list(b = 2, e = 5))

  # Order does not matter for as_list()
  expect_mapequal(
    m$as_list(),
    list(a=1, b=2, c=3, e=5)
  )

  # Order matters for mget(), and keys can be duplicated
  expect_identical(
    m$mget(c("e", "c", "a", "e")),
    list(e=5, c=3, a=1, e=5)
  )

  expect_identical(
    m$has(c("e", "a", "x", "a", "y")),
    c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )

  # Note that when removing a duplicated key, like "a" here, it reports TRUE for
  # the first instance, and FALSE for the second, because it was removed when
  # the algorithm iterated through the vector and encountered it the first time.
  # I'm not sure if that's the way it should be, or if it would be better to
  # report TRUE both times, but that is how it currently works.
  expect_identical(
    m$remove(c("e", "a", "x", "a", "y")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_mapequal(
    m$as_list(),
    list(b=2, c=3)
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

  args <- list(1,2,3)
  names(args) <- c("a", NA_character_, "c")
  expect_error(m$mset(.list = args))
  # Make sure no values got set
  expect_true(length(m$as_list()) == 0)
  expect_identical(m$keys(), character(0))
  expect_identical(m$size(), 0L)
  expect_identical(m$get("a"), NULL)

  expect_error(m$get(1))
  expect_error(m$get(TRUE))
  expect_error(m$get(NA_character_))
  expect_error(m$get(NA_integer_))
  expect_error(m$get(""))
  expect_error(m$get(character(0)))
  expect_error(m$get(numeric(0)))
  expect_error(m$get(NULL))

  expect_identical(m$mget(NULL), list(a=1)[0]) # Empty named list
  expect_error(m$mget(c(1, 2)))
  expect_error(m$mget(c("A", "")))
  expect_error(m$mget(c("A", NA)))

  expect_error(m$has(1))
  expect_error(m$has(TRUE))
  expect_error(m$has(NA_character_))
  expect_error(m$has(NA_integer_))
  expect_error(m$has(""))
  # has() is a bit more lenient than get() because it accepts a vector.
  expect_silent(m$has(character(0)))
  expect_silent(m$has(NULL))
  expect_error(m$has(numeric(0)))

  expect_error(m$remove(NA_character_))
  expect_error(m$remove(NA_integer_))
  expect_error(m$remove(""))
  # remove() is a bit more lenient than get() because it accepts a vector.
  m$remove(character(0))
  m$remove(NULL)
  expect_error(m$remove(numeric(0)))

  # Key or value unspecified
  expect_error(m$set("a"))
  expect_error(m$set(value = 123))
})


test_that("Vectorized operations are all-or-nothing", {
  # An error in set() won't leave map in an inconsistent state.
  m <- fastmap(missing_default = key_missing())
  expect_error(m$set("a", stop("oops")))
  expect_identical(m$size(), 0L)
  expect_true(length(m$as_list()) == 0)
  expect_identical(m$get("a"), key_missing())

  # Same for mset()
  expect_error(m$mset(a=1, b=stop("oops")))
  expect_identical(m$size(), 0L)
  expect_true(length(m$as_list()) == 0)
  expect_identical(m$get("a"), key_missing())

  # mset(): one bad key stops the entire thing from happening.
  expect_error(m$mset(a=1, 2, c=3))
  expect_identical(m$size(), 0L)
  expect_true(length(m$as_list()) == 0)
  expect_identical(m$get("a"), key_missing())


  # get(): one bad key stops all from being removed.
  m$mset(a=1, b=2, c=3)
  expect_error(m$mget(c("a", NA, "c")))

  # remove(): one bad key stops all from being removed.
  expect_error(m$remove(c("a", NA, "c")))
  expect_identical(m$size(), 3L)
  expect_mapequal(m$as_list(), list(a=1, b=2, c=3))
  expect_identical(m$get("a"), 1)

  # has(): one bad key stops all from being removed.
  expect_error(m$has(c("a", "", "c")))
  expect_identical(m$size(), 3L)
  expect_mapequal(m$as_list(), list(a=1, b=2, c=3))
  expect_identical(m$get("a"), 1)
})
