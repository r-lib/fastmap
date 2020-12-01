test_that("Basic operations", {
  q <- fastqueue(3)
  q$add(1)
  q$madd(2)
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
  q <- fastqueue()
  expect_null(q$remove())
  expect_null(q$remove())
  expect_identical(q$size(), 0L)
  expect_identical(q$as_list(), list())

  q$add(5)
  q$add(6)
  expect_identical(q$as_list(), list(5, 6))
})


test_that("Adding NULL to a queue", {
  q <- fastqueue()
  q$add(NULL)
  expect_identical(q$as_list(), list(NULL))

  # Do some other weird combinations of adding NULL
  q$madd(NULL, NULL)
  q$madd(.list = list(NULL))
  q$madd(NULL, .list = list(NULL, NULL))

  expect_identical(q$as_list(), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL))
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), NULL)
  expect_identical(q$remove(missing = "foo"), "foo")
})


test_that("Different values when removing from an empty queue", {
  q <- fastqueue()
  expect_identical(q$remove(missing = "foo"), "foo")
  expect_identical(q$peek(missing = "foo"), "foo")


  q <- fastqueue(missing_default = key_missing())
  expect_identical(q$remove(), key_missing())

  q$add(5)
  q$add(6)
  expect_identical(q$remove(), 5)
  expect_identical(q$remove(), 6)
  expect_identical(q$remove(), key_missing())
  expect_identical(q$peek(), key_missing())

  expect_identical(q$remove(missing = "foo"), "foo")
  expect_identical(q$peek(missing = "foo"), "foo")
})


test_that("Adding multiple items", {
  q <- fastqueue(3)
  q$madd(1, .list = list(3), 2)
  expect_identical(env(q)$q, list(1,2,3))
  expect_identical(q$as_list(), list(1,2,3))

  # Should cause two doublings, to capacity of 12
  q$madd(4,5,6,7)
  expect_identical(env(q)$q, list(1,2,3,4,5,6,7,NULL,NULL,NULL,NULL,NULL))
  expect_identical(q$as_list(), list(1,2,3,4,5,6,7))

  q$madd(8,9)
  for (i in 1:5) q$remove()
  # Wrapping around
  q$madd(10,11,12,13,14,15,16)
  expect_identical(q$as_list(), as.list(as.numeric(6:16)))
  expect_identical(env(q)$q, list(13,14,15,16,NULL,6,7,8,9,10,11,12))


  q <- fastqueue(3)
  # Should double to size 6
  q$madd(1,2,3,4,5,6)
  expect_identical(q$as_list(), list(1,2,3,4,5,6))
  expect_identical(q$remove(), 1)
  q$madd(7,8,9,10,11,12,13,14)
  expect_equal(length(env(q)$q), 24)
  expect_identical(q$as_list(), as.list(as.numeric(2:14)))
  expect_identical(env(q)$q[1:13], as.list(as.numeric(2:14)))
  for (i in 1:6) q$remove()
  expect_equal(length(env(q)$q), 24)
  expect_identical(q$as_list(), as.list(as.numeric(8:14)))

  # This should cause a shrink to size 12 because we hit the 1/4 threshold.
  expect_identical(q$remove(), 8)
  expect_identical(env(q)$q, list(9,10,11,12,13,14,NULL,NULL,NULL,NULL,NULL,NULL))
})


test_that("Resizing", {
  # Starting index 1, grow
  q <- fastqueue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  env(q)$.resize(5)
  expect_identical(env(q)$q, list(1,2,3,NULL,NULL))
  expect_identical(q$as_list(), list(1,2,3))

  # Starting index 2, grow
  q <- fastqueue(3)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$remove(), 1)
  env(q)$.resize(4)
  expect_identical(env(q)$q, list(2,3,NULL,NULL))
  expect_identical(q$as_list(), list(2,3))

  # Starting index 3, wrap around, grow
  q <- fastqueue(3)
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
  q <- fastqueue(4)
  q$add(1)
  q$add(2)
  env(q)$.resize(2)
  expect_identical(env(q)$q, list(1,2))
  expect_identical(q$as_list(), list(1,2))

  # Starting index 2, shrink
  q <- fastqueue(4)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_identical(q$remove(), 1)
  env(q)$.resize(2)
  expect_identical(env(q)$q, list(2,3))
  expect_identical(q$as_list(), list(2,3))

  # Starting index 3, wrap around, shrink
  q <- fastqueue(4)
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
  q <- fastqueue(4)
  q$add(1)
  q$add(2)
  q$add(3)
  expect_error(env(q)$.resize(2))
  expect_identical(env(q)$q, list(1,2,3,NULL))
  expect_identical(q$as_list(), list(1,2,3))
})


test_that("Error expressions don't result in inconsistent state", {
  q <- fastqueue(4)
  q$add(1)
  expect_error(q$add(stop("2")))
  q$add(3)
  expect_error(q$madd(stop("4")))
  expect_identical(q$size(), 2L)
  expect_identical(q$peek(), 1)
  expect_identical(env(q)$q, list(1,3,NULL,NULL))
  expect_identical(q$as_list(), list(1,3))
})


test_that("Random walk test", {
  q <- fastqueue()

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


test_that(".resize_at_least() works", {
  q <- fastqueue(3)
  n <- 1:25
  names(n) <- n

  # When calling .ensure_capacity for each vakye of n, the resulting size should
  # be enough to fit it, and go up by doubling.
  capacities <- vapply(n, function(x) {
    env(q)$.resize_at_least(x)
    length(env(q)$q)
  }, numeric(1))

  expected <- c(
     `1` = 3,   `2` =  3,  `3` =  3,
     `4` =  6,  `5` =  6,  `6` =  6,
     `7` = 12,  `8` = 12,  `9` = 12, `10` = 12, `11` = 12, `12` = 12,
    `13` = 24, `14` = 24, `15` = 24, `16` = 24, `17` = 24, `18` = 24,
    `19` = 24, `20` = 24, `21` = 24, `22` = 24, `23` = 24, `24` = 24,
    `25` = 48
  )

  expect_identical(capacities, expected)
})


test_that("mremove()", {
  # Case 1A and 1B
  q <- fastqueue(5)
  q$madd(1,2,3,4,5)
  expect_identical(q$mremove(1), list(1))
  expect_identical(q$mremove(4), list(2,3,4,5))
  expect_identical(env(q)$q, vector("list", 5))
  expect_identical(q$size(), 0L)

  # Case 1A and 1B, but removing exactly one item in last mremove
  q <- fastqueue(5)
  q$madd(1,2,3,4,5)
  expect_identical(q$mremove(4), list(1,2,3,4))
  expect_identical(q$mremove(1), list(5))
  expect_identical(env(q)$q, vector("list", 5))
  expect_identical(q$size(), 0L)

  # Wrap around
  q <- fastqueue(5)
  q$madd(1,2,3,4,5)
  expect_identical(q$mremove(1), list(1))
  q$madd(6)
  expect_identical(q$as_list(), list(2,3,4,5,6))
  expect_identical(env(q)$q, list(6,2,3,4,5))

  expect_identical(q$mremove(4), list(2,3,4,5))
  expect_identical(env(q)$q, list(6,NULL,NULL,NULL,NULL))
  expect_identical(q$size(), 1L)
  expect_identical(q$mremove(3, missing = -1), list(6, -1, -1))

  q <- fastqueue(5)
  q$madd(1,2,3,4,5)
  q$remove()
  q$add(6)
  expect_identical(q$mremove(5), list(2,3,4,5,6))

  q <- fastqueue(5, missing_default = -1)
  q$madd(1,2,3,4,5)
  q$remove()
  q$add(6)
  expect_identical(q$mremove(6), list(2,3,4,5,6,-1))

  q <- fastqueue(5, missing_default = -1)
  q$madd(1,2,3,4,5)
  q$mremove(2)
  q$madd(6,7)
  expect_identical(q$mremove(6), list(3,4,5,6,7,-1))
})
