context("test utils")

test_that("split trip ids", {

  OD <- data.table::data.table(v1 = c("a - b", "c - d"), v2 = c(1, 2))
  OD2 <- split_trip_id(OD = OD, or = "v1")
<<<<<<< HEAD
  expect_identical(OD2$or_id, c("a", "c"))
  expect_identical(OD2$dest_id, c("b", "d"))
=======
  expect_identical(OD2$or, c("a", "c"))
  expect_identical(OD2$dest, c("b", "d"))
>>>>>>> bf28da29a3b74552256d0c6bda523c9b06bbeee7
  expect_equal(nrow(OD), nrow(OD2))
  expect_identical(OD$v2, OD2$v2)

  OD <- data.table::data.table(v1 = c("a-b", "c-d"), v2 = c(1, 2))
  OD2 <- split_trip_id(OD = OD, or = "v1", pattern = "-")
<<<<<<< HEAD
  expect_identical(OD2$or_id, c("a", "c"))
  expect_identical(OD2$dest_id, c("b", "d"))
=======
  expect_identical(OD2$or, c("a", "c"))
  expect_identical(OD2$dest, c("b", "d"))
>>>>>>> bf28da29a3b74552256d0c6bda523c9b06bbeee7
  expect_equal(nrow(OD), nrow(OD2))
  expect_identical(OD$v2, OD2$v2)

})
