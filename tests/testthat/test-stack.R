test_that("Basic operations", {
  s <- stack()
  expect_true(s$empty())
  expect_identical(s$size(), 0L)

  s$push(5)
  s$push(6)
  s$push(NULL)
  s$push(list(a=1, b=2))
  s$push(.list=list(NULL))
  s$push(.list=list(NULL,7))
  s$push(8, .list=list(10), 9)

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
  s <- stack()
  s$push(1,2,3)
  s$push(4,5, .list = list(6, list(7,8)))
  s$push(9,10)
  expect_identical(s$as_list(), list(1,2,3,4,5,6,list(7,8),9,10))
  expect_identical(s$pop(), 10)
  expect_identical(s$pop(), 9)
  expect_identical(s$pop(), list(7,8))
})


test_that("Popping from empty stack", {
  s <- stack()
  expect_null(s$pop())
  expect_null(s$pop())
  expect_null(s$peek())
  expect_true(s$empty())

  s$push(5)
  s$push(6)
  expect_identical(s$as_list(), list(5, 6))
})


test_that("Error expressions prevent any from being added", {
  s <- stack()
  expect_error(s$push(1, stop("2"), 3))
  expect_identical(s$size(), 0L)
  expect_null(s$peek())
  expect_identical(s$as_list(), list())

  expect_error(s$push(1, .list = list(2, stop("3")), 4))
  expect_identical(s$size(), 0L)
  expect_null(s$peek())
  expect_identical(s$as_list(), list())
})
