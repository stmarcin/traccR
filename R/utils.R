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


#' Performs tests of arguments (universal ones)
checks_base_args <- function(OD, or, ttime, dest, pattern, or_dest, zero)
{
  if(!is.data.frame(OD))
    stop("'", OD, "' is not a valid object. '", OD, "' has to be a data.frame object")

  if(nrow(OD) == 0)
    stop("provided origin-destination matrix has no data")

  if(!or %in% names(OD))
    stop("column '", or, "' does not exist in the provided origin-destination matrix")

  if(!ttime %in% names(OD))
    stop("column '", ttime, "' does not exist in the provided origin-destination matrix")

  if(dest != "" & !dest %in% names(OD))
    stop("Provided column '", dest, "' does not exist in the provided origin-destination matrix")

  if(dest == "" & sum(grepl(pattern = pattern, OD[[or]])) == 0 )
    stop("Please check the pattern. The selected one - ", pattern, "' - does not split '", or, "' column")

  if(!dest == "" & sum(grepl(pattern = pattern, OD[[or]])) == nrow(OD) )
    stop("Not all records contain selected pattern - '", pattern, "' - in '", or, "' column")
  
  if(!typeof(or_dest) == "logical")
    stop(glue::glue("'or_dest' has to be 'TRUE' for trips with the same id for origin and destination to be excluded
      or 'FALSE' otherwise."))
  
  if(!typeof(zero) == "logical")
    stop("'zero' has to be 'TRUE' for trips of to be excluded or 'FALSE' otherwise.")
  
}


checks_ttime_args <- function(OD, ttime)
{
  if(typeof(OD[[ttime]]) != "double")
    stop("The type of '", ttime, "' column has to 'double'")

  if(sum(is.na(OD[[ttime]])) > 0)
      stop(glue::glue("'{ttime}' column contains 'NA' values.
        Please verify your data or remove NA by 'na.omit'"))
}  

checks_dest_args <- function(destinations = destinations, destinations_id = destinations_id)
{
  if (!is.data.frame(destinations))
    stop(glue::glue("Provided 'destinations' is not a valid object.
      'destinations' has to be a data.frame object") )
  
  if (destinations_id == "")
    stop(glue::glue("If 'destinations' is specified, 'destinations_id' is required.
      Please provide 'destinations_id' as id column name") )
}


