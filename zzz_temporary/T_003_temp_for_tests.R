library(testthat)
library(data.table)
set.seed(123)
OD <- data.table::data.table(
  or = rep(letters[seq(1, 5)], 5),
  dest = letters[rep(1:5, each=5)],
  # or = paste(rep(letters[seq(1, 5)], 5), letters[rep(1:5, each=5)], sep = " - "),
  ttime = round(runif(25, 0, 30), 1))[
    or == dest | (or == dest | (or == "a" & dest == "b")), ttime := 0
    # substr(or, 1, 1) == substr(or, nchar(or), nchar(or)), ttime := 0
    ]
des_test <- data.table(id = letters[seq(1,4)])


OD = OD
or = "or"
ttime = "ttime"
# dest = ""
dest = "dest"
pattern = " - "
destinations = des_test
destinations_id = "id"
or_dest = TRUE
zero = TRUE

source("R/utils.R")
od_copy <- copy(OD)

# OD[, c("or_id", "dest_id") := data.table::tstrsplit(c(or), pattern, fixed = TRUE)]

abc <- od_copy[or_id != dest_id & c(ttime) != 0,
                      .(proximity = min(c(ttime))), by = or_id]

abc <- od_copy[or_id != dest_id & c(ttime) != 0]

