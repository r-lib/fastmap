context("correctness")

source(test_path("helpers-expect.R"))

test_that("", {
  m <- fastmap()
  m$set("asdf", c(1, 2, 3))
  m$set("foo", "blah")

  expect_equal(m$get("asdf"), c(1, 2, 3))
  expect_contents_identical(
    m$as_list(),
    list("asdf" = c(1, 2, 3), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(m$self$values) - m$self$n_holes)

  # Removal
  m$remove("asdf")
  expect_equal(m$get("asdf"), NULL) # TODO: return a sentinel value?
  expect_contents_identical(
    m$as_list(),
    list("foo"= "blah")
  )
  expect_false(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 1L)
  expect_identical(m$size(), length(m$self$values) - m$self$n_holes)

  # Adding back
  m$set("asdf", list("a", "b"))
  expect_equal(m$get("asdf"), list("a", "b"))
  expect_contents_identical(
    m$as_list(),
    list("asdf" = list("a", "b"), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(m$self$values) - m$self$n_holes)


  # Replacing existing object
  m$set("asdf", list("x", "y"))
  expect_equal(m$get("asdf"), list("x", "y"))
  expect_contents_identical(
    m$as_list(),
    list("asdf" = list("x", "y"), "foo"= "blah")
  )
  expect_true(m$exists("asdf"))
  expect_true(m$exists("foo"))
  expect_false(m$exists("bar"))
  expect_identical(m$size(), 2L)
  expect_identical(m$size(), length(m$self$values) - m$self$n_holes)

  # NULL handling
  m$set("asdf", NULL)
  expect_equal(m$get("asdf"), NULL)
  expect_true(m$exists("asdf"))
  expect_contents_identical(
    m$as_list(),
    list("asdf" = NULL, "foo"= "blah")
  )
})

# =============================================================================
# Correctness tests for contents_identical
# =============================================================================
test_that("contents_identical works", {
  # The contents_identical function is used by expect_contents_identical. This
  # checks that it works correctly.
  expect_false(contents_identical(list(a=NULL, b=NULL), list(a=1, b=2)))
  expect_false(contents_identical(list(a=1, b=2), list(a=NULL, b=NULL)))
})
