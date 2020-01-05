#' make_filename
#'
#' \code{make_file_name} Take as input the year and returns a character string
#'    that's a filename in the working directory. Used in conjunction with
#'    \code{\link{fars_summarize_year}} and \code{\link{fars_map_state}}.
#'
#'@param fars_summarize_years year: an interger value for the year in
#' the FARS data sourced from the fars_summarize_years function
#'
#'@param fars_map_state year: an integer value for year in the FARS data sourced from the mapping
#' function.
#'
#'@return A character string identical to the filename of the corresponding year in the
#'     WD
#'
#'@examples
#'make_filename(2015)
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'
#' fars_read()
#'
#'  \code{fars_read} reads FARS data .csv and converts it to tibble i.e. dplyr::tbl_df(). it used with
#'  \code{\link{fars_summarize_years}}, \code{\link{fars_read_years}}, \code{\link{make_filename}}
#'   and \code{\link{fars_map_state}}.
#'
#'@param make_filename A string of characters located in WD
#'
#'@return This function returns a tibble of FARS data. As a side
#'     side effect, it also prints the tibble.
#'
#'
#'@examples
#'fars_read("accident_2015.csv.bz2")
#'
#'@note if filename is not in WD than it returns an error
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'
#' fars_read_years
#'
#' \code{fars_read_years} produces a set of two columned tibbles (MONTH , year), indiced by
#' year for each year in the FARS data. year <- c(2013, 2014, 2015), or, year <- c(2000:2020), etc.
#' Returns a warning if the
#'  year is not in the data. It is used in conjunction with \code{\link{fars_summarize_years}}.
#'
#'@param fars_summarize_years years: A vector of the years which exist in the data, or which are
#'    being searched for. Created un the global environment but passed from the summarize function.
#'
#'@return The object returned is set of two-columned tibbles indiced by year where each
#'   element of the tibble is an instance of a fatal autocollision during that month
#'   and year.
#'
#'@importFrom magrittr "%>%"
#'
#'@examples fars_read_years(2013:2015)
#'
#'@note if 'years' vector has not been created in the global environment then it return  an
#'   error
#'
#'@note if year[i] isnt an element of years, it returns an error: "invalid year".
#'
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' \code{fars_summarize_years}
#' aggregates the the data into a tibble of the number of autocollisions in each month
#' spread across the span of years
#'
#' @param years the numeric vector of years which are being search for.
#'
#' @return returns  tibble of the number of autocollisions in each month over the span
#' of years
#'
#'@importFrom magrittr "%>%"
#'
#' @examples
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2013)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' \code{fars_map_state} returns a plot of the coordinates of each fatal collision by the
#'  state and year in which they occured
#'
#' @param state.num An integer value which identifies a state in the FARS dataset
#' @param year An integer value which for one of the years in the FARS dataset
#'
#' @return A plot of the states map with the coordinates of each fatality that year in
#'      that state plotted as a point
#'
#'
#'
#'@examples
#'library(mapdata)
#'fars_map_state(12, 2014)
#'fars_map_state(36, 2014)
#'
#'@note if the state number isnt included in the dataset STATE column, it returns an error
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to  plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

