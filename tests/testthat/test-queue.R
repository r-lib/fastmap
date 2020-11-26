# Get the environment from the queue object, so we can access internal objects.
env <- function(x) {
  environment(x$as_list)
}

test_that("Basic operations", {
  q <- queue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$as_list(), list(1,2,3))
  expect_identical(q$remove(), 1)
  expect_identical(q$as_list(), list(2,3))
  q$add(4)
  expect_identical(q$as_list(), list(2,3,4))
  q$add(5)
  q$add(list(6,7))
  expect_identical(q$as_list(), list(2,3,4,5,list(6,7)))
  # Grow again
  q$add(8)
  q$add(9)
  expect_identical(q$remove(), 2)
})


test_that("Removing from empty queue", {
  q <- queue()
  expect_null(q$remove())
  expect_null(q$remove())
  expect_true(q$empty())
  expect_identical(q$as_list(), list())

  q$add(5)
  q$add(6)
  expect_identical(q$as_list(), list(5, 6))
})


test_that("Resizing", {
  # Starting index 1, grow
  q <- queue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  env(q)$.resize(5)
  expect_identical(env(q)$q, list(1,2,3,NULL,NULL))
  expect_identical(q$as_list(), list(1,2,3))

  # Starting index 2, grow
  q <- queue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$remove(), 1)
  env(q)$.resize(4)
  expect_identical(env(q)$q, list(2,3,NULL,NULL))
  expect_identical(q$as_list(), list(2,3))

  # Starting index 3, wrap around, grow
  q <- queue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$remove(), 1)
  expect_identical(q$remove(), 2)
  q$add(4)
  q$add(5)
  env(q)$.resize(5)
  expect_identical(env(q)$q, list(3,4,5,NULL,NULL))
  expect_identical(q$as_list(), list(3,4,5))

  # Starting index 1, shrink
  q <- queue(4)
  q$add(1)
  q$add(2)
  env(q)$.resize(2)
  expect_identical(env(q)$q, list(1,2))
  expect_identical(q$as_list(), list(1,2))

  # Starting index 2, shrink
  q <- queue(4)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$remove(), 1)
  env(q)$.resize(2)
  expect_identical(env(q)$q, list(2,3))
  expect_identical(q$as_list(), list(2,3))

  # Starting index 3, wrap around, shrink
  q <- queue(4)
  q$add(1)
  q$add(2)
  q$add(3)
  q$add(4)
  expect_identical(q$remove(), 1)
  expect_identical(q$remove(), 2)
  q$add(5)
  env(q)$.resize(3)
  expect_identical(env(q)$q, list(3,4,5))
  expect_identical(q$as_list(), list(3,4,5))

  # Can't shrink smaller than number of items
  q <- queue(4)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_error(env(q)$.resize(2))
  expect_identical(env(q)$q, list(1,2,3,NULL))
  expect_identical(q$as_list(), list(1,2,3))
})


test_that("Error expressions don't result in inconsistent state", {
  q <- queue(4)
  q$add(1)
  expect_error(q$add(stop("2")))
  q$add(3)
  expect_error(q$add(stop("4")))
  expect_identical(q$size(), 2L)
  expect_identical(q$peek(), 1)
  expect_identical(env(q)$q, list(1,3,NULL,NULL))
  expect_identical(q$as_list(), list(1,3))
})
