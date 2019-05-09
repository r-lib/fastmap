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
  expect_equal(m$get("asdf"), NULL) # TODO: return a sentinel value?
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
  expect_equal(m$get("asdf"), NULL) # TODO: return a sentinel value?

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
