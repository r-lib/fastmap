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
    m$as_list(),
    setNames(list(1, 3, 4), c(k1, k2, k4))
  )
  keys <- names(m_list)
  expect_setequal(keys, c(k1, k2, k4))
  expect_true(Encoding(keys[keys == k1]) == "unknown")
  expect_true(Encoding(keys[keys == k2]) == "UTF-8")
  expect_true(Encoding(keys[keys == k3]) == "UTF-8")
  expect_true(Encoding(keys[keys == k4]) == "UTF-8")
})
