context("encoding")

test_that("Non-ASCII keys are represented as UTF-8", {
  m <- fastmap()

  k1 <- "abc"
  # "åbc" in UTF-8
  k2 <- "\u00e5bc"
  # "åbc" in latin1
  k3 <- iconv(k2, from = "UTF-8", to = "latin1")
  # "中 A" in UTF-8
  k4 <- "\u4e2d A"

  expect_identical(Encoding(k2), "UTF-8")
  expect_identical(Encoding(k3), "latin1")
  expect_identical(Encoding(k4), "UTF-8")

  m$set(k1, 1)
  m$set(k2, 2)
  # Should overwrite k2 since the keys are the same strings in different
  # encodings, and fastmap converts keys to UTF-8.
  m$set(k3, 3)
  m$set(k4, 4)
  expect_identical(m$get(k1), 1)
  expect_identical(m$get(k2), 3)
  expect_identical(m$get(k3), 3)
  expect_identical(m$get(k4), 4)

  # keys() should be in UTF-8
  keys <- m$keys()
  # Note: expect_setequal (and expect_identical, for that matter) compares
  # strings but converts them to the same encoding before comparison, so we need
  # to separately check encoding.
  expect_setequal(keys, c(k1, k2, k4))
  expect_true(Encoding(keys[keys == k1]) == "unknown")
  expect_true(Encoding(keys[keys == k2]) == "UTF-8")
  expect_true(Encoding(keys[keys == k3]) == "UTF-8")
  expect_true(Encoding(keys[keys == k4]) == "UTF-8")

  # names for as_list() should be in UTF-8
  m_list <- m$as_list()
  expect_mapequal(
    m_list,
    setNames(list(1, 3, 4), c(k1, k2, k4))
  )
  keys <- names(m_list)
  expect_setequal(keys, c(k1, k2, k4))
  expect_true(Encoding(keys[keys == k1]) == "unknown")
  expect_true(Encoding(keys[keys == k2]) == "UTF-8")
  expect_true(Encoding(keys[keys == k3]) == "UTF-8")
  expect_true(Encoding(keys[keys == k4]) == "UTF-8")
})


test_that("Non-ASCII keys with mset and mget", {
  m <- fastmap()

  k1 <- "abc"
  # "åbc" in UTF-8
  k2 <- "\u00e5bc"
  # "åbc" in latin1
  k3 <- iconv(k2, from = "UTF-8", to = "latin1")
  # "中 A" in UTF-8
  k4 <- "\u4e2d A"

  args <- setNames(list(1, 2, 3, 4), c(k1, k2, k3, k4))
  expect_identical(
    Encoding(names(args)),
    c("unknown", "UTF-8", "latin1", "UTF-8")
  )

  # These are just here for comparison purposes. R will convert the argument
  # names to native encoding before fastmap can convert the names (keys) to
  # UTF-8. In a UTF-8 locale, the tests below would pass; in some non-UTF-8
  # locales, the tests would fail. They're commented out because we can't expect
  # them to pass on all platforms.
  # do.call(m$mset, args)
  # expect_identical(m$get(k1), 1)
  # expect_identical(m$get(k2), 3)
  # expect_identical(m$get(k3), 3)
  # expect_identical(m$get(k4), 4)

  # Same as above, but using .list. This should succeed in all locales.
  m <- fastmap()
  m$mset(.list = args)
  expect_identical(m$get(k1), 1)
  expect_identical(m$get(k2), 3)
  expect_identical(m$get(k3), 3)
  expect_identical(m$get(k4), 4)

  # names for as_list() should be in UTF-8
  m_list <- m$as_list()
  expect_mapequal(
    m_list,
    setNames(list(1, 3, 4), c(k1, k2, k4))
  )
  keys <- names(m_list)
  expect_setequal(keys, c(k1, k2, k4))
  expect_true(Encoding(keys[keys == k1]) == "unknown")
  expect_true(Encoding(keys[keys == k2]) == "UTF-8")
  expect_true(Encoding(keys[keys == k3]) == "UTF-8")
  expect_true(Encoding(keys[keys == k4]) == "UTF-8")

  # mget will convert the latin1 key to UTF-8
  res <- m$mget(c(k1, k2, k3, k4))
  expect_identical(
    Encoding(names(res)),
    c("unknown", "UTF-8", "UTF-8", "UTF-8")
  )
  expect_identical(names(res), c(k1, k2, k2, k4))
  expect_identical(unname(res), list(1, 3, 3, 4))
})
