test_that("Basic operations", {
  s <- faststack()
  expect_identical(s$size(), 0L)

  s$push(5)
  s$push(6)
  s$push(NULL)
  s$push(list(a=1, b=2))
  s$mpush(.list=list(NULL))
  s$mpush(.list=list(NULL,7))
  s$mpush(8, .list=list(10), 9)

  # as_list() returns in the order that they were inserted
  expect_identical(
    s$as_list(),
    list(
      5,
      6,
      NULL,
      list(a=1, b=2),
      NULL,
      NULL,
      7,
      8,
      9,
      10
    )
  )

  expect_identical(s$pop(), 10)
  expect_identical(s$pop(), 9)
  expect_identical(s$pop(), 8)
  expect_identical(s$pop(), 7)
  expect_identical(s$pop(), NULL)
  expect_identical(s$pop(), NULL)
  expect_identical(s$pop(), list(a=1, b=2))
  expect_identical(s$peek(), NULL)
  expect_identical(s$pop(), NULL)
  expect_identical(s$size(), 2L)

  expect_identical(s$as_list(), list(5, 6))

  s$reset()
  expect_identical(s$size(), 0L)
  expect_identical(s$as_list(), list())
})


test_that("Pushing multiple", {
  s <- faststack()
  s$mpush(1,2,3)
  s$mpush(4,5, .list = list(6, list(7,8)))
  s$mpush(9,10)
  expect_identical(s$as_list(), list(1,2,3,4,5,6,list(7,8),9,10))
  expect_identical(s$pop(), 10)
  expect_identical(s$pop(), 9)
  expect_identical(s$pop(), list(7,8))
})


test_that("Popping from empty stack", {
  s <- faststack()
  expect_null(s$pop())
  expect_null(s$pop())
  expect_null(s$peek())
  expect_identical(s$size(), 0L)

  s$push(5)
  s$push(6)
  expect_identical(s$as_list(), list(5, 6))
})


test_that("Different values when popping from an empty stack", {
  s <- faststack()
  expect_identical(s$pop(missing = "foo"), "foo")
  expect_identical(s$peek(missing = "foo"), "foo")


  s <- faststack(missing_default = key_missing())
  expect_identical(s$pop(), key_missing())
  expect_identical(s$pop(), key_missing())
  expect_identical(s$peek(), key_missing())
  expect_identical(s$size(), 0L)

  s$push(5)
  s$push(6)
  expect_identical(s$pop(), 6)
  expect_identical(s$pop(), 5)
  expect_identical(s$pop(missing = "foo"), "foo")
  expect_identical(s$pop(), key_missing())

})


test_that("Error expressions prevent any from being added", {
  s <- faststack()
  expect_error(s$push(1, stop("2"), 3))
  expect_identical(s$size(), 0L)
  expect_null(s$peek())
  expect_identical(s$as_list(), list())

  expect_error(s$push(1, .list = list(2, stop("3")), 4))
  expect_identical(s$size(), 0L)
  expect_null(s$peek())
  expect_identical(s$as_list(), list())
})


test_that("mpop()", {
  s <- faststack(2L)
  s$mpush(1,2,3,4,5,6,7,8,9,10,11,12,13)
  expect_identical(s$mpop(6), list(13,12,11,10,9,8))
  expect_identical(s$as_list(), list(1,2,3,4,5,6,7))
  expect_identical(s$size(), 7L)
  # Check that we did NOT resize the underlying list since we haven't gone under
  # the 1/2 threshold.
  expect_identical(env(s)$s, c(list(1,2,3,4,5,6,7), rep(list(NULL), 6)))

  expect_identical(s$mpop(1), list(7))
  expect_identical(s$as_list(), list(1,2,3,4,5,6))
  expect_identical(s$size(), 6L)
  # Now we should have resized.
  expect_identical(env(s)$s, list(1,2,3,4,5,6))

  expect_identical(s$mpop(9), list(6,5,4,3,2,1,NULL,NULL,NULL))
  expect_identical(env(s)$s, list(NULL,NULL))
  expect_identical(s$size(), 0L)

  # Different `missing`
  s <- faststack(2, missing_default = NA)
  s$mpush(1,2,3,4,5,6,7,8,9,10,11,12,13)
  expect_identical(s$mpop(6), list(13,12,11,10,9,8))
  expect_identical(s$mpop(9), list(7,6,5,4,3,2,1,NA,NA))
  expect_identical(s$mpop(3, missing = "x"), list("x","x","x"))
})
