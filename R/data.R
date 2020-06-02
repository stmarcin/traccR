#' Polygons of census tracks of Ursynów district
#' 
#' Spatial dataset of 128 census tracks of Ursynów district of Warsaw 
#' 
#' @docType data
#' 
#' @usage data(rej_ursynow)
#' 
#' 
#' @format A spatial dataset with 128 rows and 3 variables
#' \describe{
#'  \item{id}{official id of a census track}
#'  \item{POP}{total population of a census track}
#'  \item{geometry}{geometry of a census track}
#' }
#' 
#' @source \url{http://www.codgik.gov.pl/index.php/darmowe-dane/prg.html}
"rej_ursynow"
NULL
#' origin-destination matrix
#' 
#' Origin-destination matrix of travel times by car between all census tracks of Ursynów district in Warsaw. 
#' 
#' @docType data
#' 
#' @usage data(od_data)
#' 
#' 
#' @format A datatable with 16384 rows and 2 variables
#' \describe{
#'  \item{Name}{trip id; structure: 'origin_id - destination_id'}
#'  \item{travel_time}{travel time by car in minutes between origin and destination}
#' }
#' 
#' @source \url{https://marcinstepniak.eu/project/giservice/}
"od_data"