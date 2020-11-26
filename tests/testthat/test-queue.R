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


test_that("Random walk test", {
  q <- queue()

  set.seed(1312)
  ops <- integer(2e5)
  csum <- 0

  # Set up a random walk where we add and remove items (1=add, -1=remove), but
  # never try to remove when the queue is empty.

  # This is a "pool" of possible operations that's larger than the actual number
  # of operations we'll end up with. The loop below reads from this object so
  # that it doesn't have to call sample(, 1) over and over, because that takes a
  # long time.
  ops_pool <- sample(c(-1, 1), length(ops) * 2, replace = TRUE, prob = c(0.4, 0.6))

  for (i in seq_along(ops)) {
    if (csum <= 0) {
      # Ensure we never do a -1 when already empty.
      ops[i] <- 1
    } else {
      # Each position has 1 or -1, but we bias the probability toward increasing
      # in size.
      ops[i] <- ops_pool[i]
    }
    csum <- csum + ops[i]
  }

  # Set up commands to remove items until empty.
  for (i in (length(ops) + seq_len(csum))) {
    ops[i] <- -1
    # csum <- csum + ops[i]
  }
  # At the end, we should have same number of add and remove commands.
  expect_identical(sum(ops), 0)

  # The values to add are  1, 2, 3, etc., and we expect them to be removed in
  # the same order.
  next_to_add    <- 1
  next_to_remove <- 1
  for (i in seq_along(ops)) {
    if (ops[i] < 0) {
      v <- q$remove()
      if (v != next_to_remove) {
        # Don't use expect_identical() here because it's too slow in a loop.
        stop("Mismatch between actual and expected value.")
      }
      next_to_remove <- next_to_remove + 1

    } else {
      q$add(next_to_add)
      next_to_add <- next_to_add + 1
    }
  }

  expect_identical(q$size(), 0L)
  expect_identical(q$as_list(), list())
  # Should be back to original internal state: a list of 20 NULLs
  expect_identical(env(q)$q, lapply(1:20, function(x) NULL))
})
