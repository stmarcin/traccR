#' @title Proximity accessibility indicator
#'
#' @description \code{proximity} uses origin-destination matrix
#' to calculate proximity indicator
#'
#' @param OD data.table with origin-destination matrix
#' @param or column in `OD` that contains ids of origins or both, origin and destination ids
#' @param ttime column in `OD` that contains travel time or distance
#' @param dest *optional* column in `OD` that contains ids of destinations.
#' It is ommited , if or contains origin and destination ids.
#' @param destinations *optional* ids of available destinations.
#' If not specified, all  available destinatinos from `OD` dataset are used.
#' Accepted data formats: tibble, data.table, data.frame or sf object.
#' @param pattern *optional* a pattern used to split `name` into origin and destination column. Default: `" - "`
#' @param or_dest a bolean argument. When `TRUE`, it excludes trips where origin has the same ID as destination.
#' Default: TRUE
#' @param zero a bolean argument. When `TRUE` it excludes trips of zero length. Default: `TRUE`
#'
#' @details `proximity()` calculates (selects) travel time (or distance)
#' to the closests destination out of all available ones.
#' It can select any destination of all available  within OD dataset
#' or to destination available in separate dataset (using join).
#' Additional parameters enable to exclude OD pairs where origin and destination
#' have the same ID or a measured time is equal to zero.
#'
#' @return a data.table object with distance / travel time to the nearest destination
#'
#' @export
proximity <- function(OD, or, ttime, # required
                      dest = "", pattern = " - ", # destinations in separate column or in or as trip ids
                      destinations, destinations_id = "", # if selected destinations are in another table
                      or_dest = TRUE, zero = TRUE) # parameters for exclusion
{
  # verify if arguments are corrected ----
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

  if(!missing(destinations))
  {
    if(!is.data.frame(destinations))
    stop(glue::glue("Provided 'destinations' is not a valid object.
                    'destinations' has to be a data.frame object"))

    if(destinations_id == "")
      stop(glue::glue("If 'destinations' is specified, 'destinations_id' is required.
                      Please provide 'destinations_id' as id column name"))
  }

  if(typeof(OD[[ttime]]) != "double")
    stop("The type of '", ttime, "' column has to 'double'")

  if(sum(is.na(OD[[ttime]])) > 0)
      stop(glue::glue("'{ttime}' column contains 'NA' values.
                    Please verify your data or remove NA by 'na.omit'"))

  # copy OD ----
  # transform OD to data.table. if it is already a data.table than make a copy
  # all operations on data.table would modify original dataset
  ifelse(
    data.table::is.data.table(OD),
    od_copy <- data.table::copy(OD),
    od_copy <- data.table::as.data.table(od)
  )

  # split 'or' ----
  # if destinations 'or' column contains trip ids instead of origin id
  # (e.g. in the form 'origin_id - destination_id') then it has to be split
  # to two columns, one for origin_id ('or') and second for destination_id ('dest')
  # also - if column 'or' is equal to 'or' -> rename as 'or_id',
  # same for `dest` -> rename to  'dest_id'
  ifelse(
    dest == "",
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
                 od_copy <- od_copy[or_id != dest_id,
                                    .(proximity = min(c(ttime))), by = or_id],

                 # zero-length allowed (FALSE) & origin = destination allowed (FALSE)
                 od_copy <- od_copy[, .(proximity = min(c(ttime))), by = or_id]
         )}
  )

  return(od_copy)

}


