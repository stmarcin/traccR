context("test arguments of functions")

test_that("test if proximity function has all arguments", {

  OD <- "a"
  msg <- paste0("'", OD, "' is not a valid object. '", OD, "' has to be a data.frame object")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2"), msg)

  OD <- data.table::data.table(v1 = c("a", "b"), v2 = c(1, 2))

  or <- "a"
  msg <- paste0("column '", or, "' does not exist in the provided origin-destination matrix")
  expect_error (g <- proximity (OD, or = "a", ttime = "v2"), msg)

  ttime <- "a"
  msg <- paste0("column '", ttime, "' does not exist in the provided origin-destination matrix")
  expect_error (g <- proximity (OD, or = "v1", ttime = "a"), msg)

  dest <- "a"
  msg <- paste0("Provided column '", dest, "' does not exist in the provided origin-destination matrix")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2", dest = "a"), msg)

  pattern <- " - "
  or <- "v1"
  msg <- paste0("Please check the pattern. The selected one - ", pattern, "' - does not split '", or, "' column")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2"), msg)

  OD <- data.table::data.table(v1 = c("a - b", "b - a"), v2 = c(1, 2))
  ttime <- "v1"
  msg <- paste0("The type of '", ttime, "' column has to 'double'")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v1"), msg)

  OD <- data.table::data.table(v1 = c("a - b", "b"), v2 = c(1, 2))
  msg <- paste0("Not all records contain selected pattern - '", pattern, "' - in '", or, "' column")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2"), msg)

  OD <- data.table::data.table(v1 = c("a - b", "b - a"), v2 = c(1, NA))
  ttime <- "v2"
  msg <- glue::glue("'{ttime}' column contains 'NA' values.
                    Please verify your data or remove NA by 'na.omit'")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2"), msg)

  destinations <- "a"
  msg <- glue::glue("Provided 'destinations' is not a valid object.
                    'destinations' has to be a data.frame object")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2", destinations = destinations), msg)

  OD <- data.table::data.table(v1 = c("a - b", "b - a"), v2 = c(1, 2))
  des_test <- data.table::data.table(des = c("a", "c"))
  msg <- glue::glue("If 'destinations' is specified, 'destinations_id' is required.
                    Please provide 'destinations_id' as id column name")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2", destinations = des_test), msg)

  OD <- data.table::data.table(or = c("e", "f", "g"), dest = c("a", "b", "c"), v2 = c(1, 2, 3))
  msg <- glue::glue("'or_dest' has to be 'TRUE' for trips with the same id for origin and destination to be excluded
    or 'FALSE' otherwise.")
  expect_error (g <- proximity (OD, or = "or", dest = "dest", ttime = "v2", or_dest = "a"), msg)
  
  msg <- "'zero' has to be 'TRUE' for trips of to be excluded or 'FALSE' otherwise."
  expect_error (g <- proximity (OD, or = "or", dest = "dest", ttime = "v2", zero = "a"), msg)

})

test_that("test arguments of split_trip_id function", {

  OD <- "a"
  msg <- paste0("'", OD, "' is not a valid object. '", OD, "' has to be a data.frame object")
  expect_error (g <- split_trip_id (OD, or = "v1"), msg)

  OD <- data.table::data.table(v1 = c("a", "b"), v2 = c(1, 2))
  or <- "a"
  msg <- paste0("column '", or, "' does not exist in the provided origin-destination matrix")
  expect_error (g <- split_trip_id (OD, or = "a"), msg)

  pattern <- " - "
  or <- "v1"
  msg <- paste0("Please check the pattern. The selected one - ", pattern, "' - does not split '", or, "' column")
  expect_error (g <- split_trip_id (OD, or = "v1"), msg)

  OD <- data.table::data.table(v1 = c("a - b", "b"), v2 = c(1, 2))
  msg <- paste0("Not all records contain selected pattern - '", pattern, "' - in '", or, "' column")
  expect_error (g <- split_trip_id (OD, or = "v1"), msg)

})

test_that("no empty OD files", {

  OD <- data.table::data.table(v1 = c("a - b", "b - a"), v2 = c(1, 2))[0]
  msg <- paste0("provided origin-destination matrix has no data")
  expect_error (g <- proximity (OD, or = "v1", ttime = "v2"), msg)
  expect_error (g <- split_trip_id (OD, or = "v1"), msg)

})

test_that("no empty join with destinations", {

  OD <- data.table::data.table(or = c("e", "f", "g"), dest = c("a", "b", "c"), v2 = c(1, 2, 3))
  des_test <- data.table::data.table(des = c("x", "y"), foo = 1)
  msg <- glue::glue("There is no matching records between 'OD' and 'destinations'.
                      Check if 'destinations_id' is defined correctly")
  expect_error (g <- proximity(OD = OD, or = "or", ttime = "v2", dest = "dest",
    destinations = des_test, destinations_id = "des"), msg)

  des_test <- data.table::data.table(des = c("a", "b"), foo = 1)
  od_copy <- proximity(OD = OD, or = "or", ttime = "v2", dest = "dest", 
    destinations = des_test, destinations_id = "des")
  expect_equal(nrow(od_copy), 2)

})


# test_that("test matching of destinations dataset", {
#
#
#
#
# })
