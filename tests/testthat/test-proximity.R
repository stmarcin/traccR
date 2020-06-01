context("test proximity function")

test_that("test selection of destinations", {

      set.seed(123)
      OD <- data.table::data.table(
            or = paste(rep(letters[seq(1, 5)], 5), letters[rep(1:5, each = 5)], sep = " - "),
            ttime = round(runif(25, 0, 30), 1))[
                  substr(or, 1, 1) == substr(or, nchar(or), nchar(or)) | (or == "a - b"), ttime := 0]
      des_test <- data.table::data.table(id = letters[seq(1, 4)])

      # test number of results - always 5 as it is defined by number of origins
      expect_equal(nrow(g <- proximity(OD = OD, or = "or", ttime = "ttime",
                                       destinations = des_test, destinations_id = "id")), 5)
      expect_equal(nrow(g <- proximity(OD = OD, or = "or", ttime = "ttime",
                                       destinations = des_test, destinations_id = "id", or_dest = FALSE)), 5)
      expect_equal(nrow(g <- proximity(OD = OD, or = "or", ttime = "ttime",
                                       destinations = des_test, destinations_id = "id", zero = FALSE)), 5)

      results <- merge(proximity(OD = OD, or = "or", ttime = "ttime"),
                       proximity(OD = OD, or = "or", ttime = "ttime",
                                 destinations = des_test, destinations_id = "id"),
                       by = "or_id")

      for(i in 1:5)  {
         expect_lte(results$proximity.x[i], results$proximity.y[i])
      }

      expect_lt(sum(results$proximity.x), sum(results$proximity.y))




})

test_that("test scenarios (zeros and or=dest", {

   set.seed(123)
   OD <- data.table::data.table(
      or = rep(letters[seq(1, 5)], 5),
      dest = letters[rep(1:5, each=5)],
      ttime = round(runif(25, 0, 30), 1))[
         or == dest | (or == dest | (or == "a" & dest == "b")), ttime := 0]
   expect_equal(min(g <- proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest", zero = FALSE)$proximity), 0)
   expect_false(min(g <- proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest")$proximity) == 0)

   for(i in 1:5)  {
      expect_equal(proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest",
                             or_dest = FALSE, zero = FALSE)$proximity[i], 0)
   }

})

test_that("test proximity results [sets or & dest overlap]", {

   set.seed(123)
   OD <- data.table::data.table(
      or = rep(letters[seq(1, 5)], 5),
      dest = letters[rep(1:5, each=5)],
      ttime = round(runif(25, 0, 30), 1))[
         or == dest | (or == dest | (or == "a" & dest == "b")), ttime := 0]


   for(i in letters[seq(1, 5)])  {
      expect_equal( min(OD[or == i & ttime != 0][["ttime"]]),
                    proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest")[or_id == i][["proximity"]])
   }

   for(i in letters[seq(1, 5)])  {
      expect_equal( min(OD[or == i & or != dest][["ttime"]]),
                    proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest", zero = FALSE)[or_id == i][["proximity"]])
   }

})

test_that("test proximity results [sets or & dest do not overlap]", {

   set.seed(123)
   OD <- data.table::data.table(
      or = rep(letters[seq(1, 5)], 5),
      dest = letters[rep(6:10, each = 5)],
      ttime = round(runif(25, 0, 30), 1))[
         or == dest | (or == dest | (or == "a" & dest == "f")), ttime := 0]


   for(i in letters[seq(1, 5)])  {
      expect_equal( min(OD[or == i & ttime != 0][["ttime"]]),
                    proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest")[or_id == i][["proximity"]])
   }

   for(i in letters[seq(1, 5)])  {
      expect_equal( min(OD[or == i & or != dest][["ttime"]]),
                    proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest",
                              zero = FALSE)[or_id == i][["proximity"]])
   }

   for(i in letters[seq(1, 5)])  {
      expect_equal( min(OD[or == i & ttime != 0][["ttime"]]),
                    proximity(OD = OD, or = "or", ttime = "ttime", dest = "dest",
                              or_dest = FALSE)[or_id == i][["proximity"]])
   }

})
