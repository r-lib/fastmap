test_that("Serializing and unserializing a map", {
  # Test with difficult encodings (code borrowed from encoding tests)
  m <- fastmap()

  k1 <- "abc"
  # "åbc" in UTF-8
  k2 <- "\u00e5bc"
  # "åbc" in latin1
  k3 <- iconv(k2, from = "UTF-8", to = "latin1")
  # "中 A" in UTF-8
  k4 <- "\u4e2d A"
  k5 <- "xyz"

  m$set(k1, 1)
  m$set(k2, 2)
  m$set(k3, 3)
  m$set(k4, 4)
  m$set(k5, 5)
  m$remove(k1)  # Make a hole

  m1 <- unserialize(serialize(m, NULL))
  expect_mapequal(m$as_list(), m1$as_list())
  expect_identical(m$size(), m1$size())
  expect_setequal(m$keys(), m1$keys())
  expect_true(all(Encoding(m1$keys()) %in% c("unknown", "UTF-8")))

  # Make sure that m1 behaves correctly when modified
  m$set(k1, 10)
  m$set(k3, 30)
  m1$set(k1, 10)
  m1$set(k3, 30)
  expect_mapequal(m$as_list(), m1$as_list())
  expect_identical(m$size(), m1$size())
  expect_true(all(Encoding(m1$keys()) %in% c("unknown", "UTF-8")))
})

test_that("Serializing and unserializing stress test", {
  set.seed(3524)

  n <- 1e4
  # Generate keys and values. Note that all the keys/values have duplicates.
  values <- rnorm(n)
  keys <- as.character(values)

  m <- fastmap()
  # First add them in random order (all are added twice, but that shouldn't hurt)
  add_order <- sample.int(n)
  for (i in add_order) {
    m$set(keys[i], values[i])
  }

  # Then remove 1/3 them in random order
  remove_order <- sample.int(n, size = round(1/3 * n))
  for (i in remove_order) {
    m$remove(keys[i])
  }

  m1 <- unserialize(serialize(m, NULL))
  m1$restore()
  expect_mapequal(m$as_list(), m1$as_list())
  expect_identical(m$size(), m1$size())
  expect_setequal(m$keys(), m1$keys())


  # Add some random subset of values to m and m1, and make sure the result is
  # the same.
  add_order <- sample.int(n, size = round(1/3 * n))
  for (i in add_order) {
    m$set(keys[i], values[i])
    m1$set(keys[i], values[i])
  }
  expect_mapequal(m$as_list(), m1$as_list())
  expect_identical(m$size(), m1$size())
  expect_setequal(m$keys(), m1$keys())

  # Remove a subset of values from m and m1, and make sure the result is the
  # same.
  remove_order <- sample.int(n, size = round(1/3 * n))
  for (i in remove_order) {
    m$remove(keys[i])
    m1$remove(keys[i])
  }
  expect_mapequal(m$as_list(), m1$as_list())
  expect_identical(m$size(), m1$size())
  expect_setequal(m$keys(), m1$keys())
})
