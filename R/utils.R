#' Split trip id to origin and destination id
#'
#' Splits trip ids in origin-destination object into origins and destinations columns.
#' Returns modified data.table
#'
#' @param OD data.table with origin-destination matrix
#' @param or column in `OD` that contains ids of all the trip (origin and destination ids)
#'
#' @return modified data.table
#'
#' @importFrom data.table :=
#'
#' @noRd
split_trip_id <- function(OD, or, pattern = " - ")
{
  # makes a copy in order to avoid forced modification of an original file!
  if(!is.data.frame(OD))
    stop("'", OD, "' is not a valid object. '", OD, "' has to be a data.frame object")

  if(nrow(OD) == 0)
    stop("provided origin-destination matrix has no data")

  if(!or %in% names(OD))
    stop("column '", or, "' does not exist in the provided origin-destination matrix")

  if(sum(grepl(pattern = pattern, OD[[or]])) == 0 )
    stop("Please check the pattern. The selected one - ", pattern, "' - does not split '", or, "' column")

  if(!sum(grepl(pattern = pattern, OD[[or]])) == nrow(OD) )
    stop("Not all records contain selected pattern - '", pattern, "' - in '", or, "' column")

  OD[, c("or_id", "dest_id") := data.table::tstrsplit(get(or), pattern, fixed = TRUE)]

  if(or != "or_id")  OD[, c(or) := NULL]

  return(OD)

}