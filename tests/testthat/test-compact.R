
test_that("Compacting a map", {
  m <- fastmap()
  m$set("d", 4)
  m$set("g", 7)
  m$set("b", 2)
  m$set("a", 1)
  m$set("c", 3)
  m$set("e", 5)
  m$set("f", 6)

  m$compact()
  expect_mapequal(
    m$as_list(),
    list(a = 1, b = 2, c = 3, d = 4, e = 5, f= 6, g = 7)
  )

  m$remove("d")
  m$remove("a")
  m$remove("c")
  m$remove("e")
  m$set("e", 5)

  m$compact()
  expect_mapequal(
    m$as_list(),
    list(b = 2, e = 5, f= 6, g = 7)
  )

  # Second compacting does not change anything
  m$compact()
  expect_mapequal(
    m$as_list(),
    list(b = 2, e = 5, f= 6, g = 7)
  )
})

test_that("Compacting empty map", {
  m <- fastmap()
  m$compact()
  expect_true(length(m$as_list()) == 0)
})
