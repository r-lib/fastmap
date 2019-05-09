context("encoding")

test_that("Non-ASCII keys", {
  m <- fastmap()
  m$set("åbc", 1)
  m$set("abc", 2)
  m$set("a∫ç", 3)
  expect_identical(m$get("åbc"), 1)
  expect_identical(m$get("abc"), 2)
  expect_identical(m$get("a∫ç"), 3)
  expect_mapequal(
    m$as_list(),
    list("åbc" = 1, "abc" = 2, "a∫ç" = 3)
  )
})
