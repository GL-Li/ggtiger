#' fips of each state
#'
#'
#' @format A data.table with 53 rows and 3 variables
#' \describe{
#'   \item{fips}{fips codes of state}
#'   \item{abbr}{abbreviations of state}
#'   \item{state}{lower case state names}
#'
#' }
#'
#' @docType data
#'
#'
#' @format data.table
#'
#' @keywords datasets
#'

"all_state_fips"


#' fips of each county
#'
#'
#' @format A data.table with 3109 rows and 3 variables
#' \describe{
#'   \item{fips}{fips codes of state}
#'   \item{state}{lower case state names}
#'   \item{county}{lower case county names}
#'
#' }
#'
#' @docType data
#'
#'
#' @format data.table
#'
#' @keywords datasets
#'

"all_county_fips"


#' First two digits of zip code in each state
#'
#'
#' @format A data.table with 172 rows and 2 variables
#' \describe{
#'   \item{abbr}{abbreviation of state}
#'   \item{ZCTA5}{first two digits of ZCTA5}
#' }
#'
#' @docType data
#'
#'
#' @format data.table
#'
#' @keywords datasets
#'

"state_zipstart"


#' First two digits of zip code in each county
#'
#'
#' @format A data.table with 172 rows and 2 variables
#' \describe{
#'   \item{abbr}{abbreviation of state}
#'   \item{county}{county name}
#'   \item{ZCTA5}{first two digits of ZCTA5}
#' }
#'
#' @docType data
#'
#'
#' @format data.table
#'
#' @keywords datasets
#'

"state_county_zipstart"


#' all zip code in each county
#'
#'
#' @format A data.table with 172 rows and 2 variables
#' \describe{
#'   \item{abbr}{abbreviation of state}
#'   \item{county}{county name}
#'   \item{ZCTA5}{ZCTA5}
#' }
#'
#' @docType data
#'
#'
#' @format data.table
#'
#' @keywords datasets
#'

"all_zipcode"
