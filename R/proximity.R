#' @title Proximity accessibility indicator
#'
#' @description \code{proximity} uses origin-destination matrix
#' to calculate proximity indicator
#'
#' @param OD data.table with origin-destination matrix
#' @param or column in `OD` that contains ids of origins or both, origin and destination ids
#' @param ttime column in `OD` that contains travel time or distance
#' @param dest *optional* column in `OD` that contains ids of destinations.
#' It is omitted, if column `or` contains origin and destination ids.
#' @param destinations *optional* data set of available destinations.
#' If not specified, all  available destinations from `OD` data  set are used.
#' Accepted data formats: tibble, data.table, data.frame or sf object.
#' @param destinations_id *optional* column in destinations with ids of available destinations
#' @param pattern *optional* a pattern used to split `name` into origin and destination column. Default: `" - "`
#' @param or_dest a  boolean argument. When `TRUE`, it excludes trips where origin has the same ID as destination.
#' Default: TRUE
#' @param zero a  boolean argument. When `TRUE` it excludes trips of zero length. Default: `TRUE`
#'
#' @details `proximity()` calculates (selects) travel time (or distance)
#' to the closest destination out of all available ones.
#' It can select any destination of all available  within OD dataset
#' or to destination available in separate dataset (using join).
#' Additional parameters enable to exclude OD pairs where origin and destination
#' have the same ID or a measured time is equal to zero.
#'
#' @return a data.table object with distance / travel time to the nearest destination
#'
#' @export
proximity <- function(OD, or, ttime, # required
                      dest, pattern = " - ", # destinations in separate column or in or as trip ids
                      destinations, destinations_id, # if selected destinations are in another table
                      or_dest = TRUE, zero = TRUE) # parameters for exclusion
{
  # Tests of arguments. as they repeat in other functions, all tests are in utils.R
  # NOTE: the sequence of checks is important
  checks_base_args(OD = OD, or = or, ttime = ttime,
                      dest = dest, pattern = pattern,
                      or_dest = or_dest, zero = zero)

  if(!missing(destinations))
    checks_dest_args(destinations = destinations, destinations_id = destinations_id)

  checks_ttime_args(OD = OD, ttime = ttime)



  # copy OD ----
  # transform OD to data.table. if it is already a data.table than make a copy
  # all operations on data.table would modify original dataset
  ifelse(
    data.table::is.data.table(OD),
    od_copy <- data.table::copy(OD),
    od_copy <- data.table::as.data.table(od)
  )

  # split 'or' ----
  # if 'or' column in 'OD' contains trip ids instead of origin id
  # (e.g. in the form 'origin_id - destination_id') then it has to be split
  # to two columns, one for origin_id ('or') and second for destination_id ('dest')
  # also - if column 'or' is equal to 'or' -> rename as 'or_id',
  # same for `dest` -> rename to  'dest_id'
  ifelse(
    missing(dest),
    od_copy <- split_trip_id(OD = od_copy, or = or, pattern = pattern),
    {
      data.table::setnames(od_copy, dest, "dest_id")
      data.table::setnames(od_copy, or, "or_id")
    } )

  # select available destinations ----
  # OD may contain more destinations than it is needed
  # if destinations != "" join tables to select available destinations
  if(!missing(destinations))
  {
    data.table::setkey(od_copy, dest_id)
    data.table::setkeyv(destinations, destinations_id)

    od_copy <- merge(od_copy, destinations[, ..destinations_id], by.x = "dest_id", by.y = destinations_id)
    if(nrow(od_copy) == 0)
      stop(glue::glue("There is no matching records between 'OD' and 'destinations'.
                      Check if 'destinations_id' is defined correctly"))
  }

  # calculate proximity ----
  # 4 variants, depending on which trips should be considered
  ifelse(or_dest == TRUE,

         {ifelse(zero == TRUE,

                 # zero-length excluded (TRUE) & origin = destination excluded (TRUE)
                 od_copy <- od_copy[or_id != dest_id & c(ttime) != 0,
                                    .(proximity = min(c(ttime))), by = or_id],

                 # zero-length allowed (FALSE) & origin = destination excluded (TRUE)
                 od_copy <- od_copy[or_id != dest_id, .(proximity = min(c(ttime))), by = or_id]

                 )},

         {ifelse(zero == TRUE,

                 # zero-length excluded (TRUE) & origin = destination allowed (FALSE)
                 od_copy <- od_copy[c(ttime) != 0,
                                    .(proximity = min(c(ttime))), by = or_id],

                 # zero-length allowed (FALSE) & origin = destination allowed (FALSE)
                 od_copy <- od_copy[, .(proximity = min(c(ttime))), by = or_id]
         )}
  )

  return(od_copy)

}

utils::globalVariables(c("od", "dest_id", "..destinations_id", "or_id", "."))
