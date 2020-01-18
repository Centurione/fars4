#' make_filename
#'
#' This function takes as input the \code{year} and returns a character string
#'    that's a filename in the working directory. .
#'
#'@param year a four digit interger
#'
#'
#'@return A character string identical to the filename of the corresponding year in the
#'     WD
#'
#'@examples
#' \dontrun{make_filename(2015)}
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'
#' fars_read()
#'
#'  \code{fars_read} reads FARS data .csv and converts it to tibble i.e. dplyr::tbl_df(). it will
#'  return an error if the  \code{filename} is not in the working directory.
#'
#'@param filename A string of characters whose data is to become a tibble
#'
#'@return This function returns a tibble with the data from the filname.
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@examples
#'\dontrun{fars_read("accident_2015.csv.bz2")}
#'
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
#' This function takes a 4 digit numeric \code{years} and produces a set of two columned tibbles
#' (MONTH , year), indiced by year for each year in the FARS data. year <- c(2013, 2014, 2015), or,
#' year <- c(2000:2020), etc. Returns a warning if the year is not in the data.
#'
#'@param years A vector of the years which exist in the data, or which are
#'    being searched for.
#'
#'@return The object returned is a list dataframes indiced by year where each
#'   element of the tibble is an instance of a fatal autocollision during that month
#'   and year.
#'
#'@importFrom magrittr "%>%"
#'
#'@importFrom dplyr mutate select
#'
#'@examples
#'   \dontrun{fars_read_years(2013:2015)}
#'
#'@note if 'years' vector has not been created in the global environment then it return  an
#'   error
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
#' A funtion which takes a 4 digit vector \code{years}, reads FARS data into a list of dataframes via
#' \code{fars_read_years} then aggregates the the data into a tibble of the number of fatal
#' autocollisions in each month spread across the span of years
#'
#' @param years the numeric vector of years which are being search for.
#'
#' @return returns  tibble of the number of autocollisions in each month over the span
#' of years
#'
#'@importFrom magrittr "%>%"
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(years = c(2013, 2014, 2015))}
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
#' this funtion takes an integer represting the \code{state.num} and another representing the
#' \code{year} returns a plot of the coordinates of each fatal collision by the
#'  state and year in which they occured. It does this with the use of \code{make_filename}, and
#'  \code{fars_read}. It will throw and error if the numeric provided does not exist in the data set
#'  for \code{state.num} and will return a message "no accident to plot" if there is no accident to
#'  report for the state and year.
#'
#' @param state.num An integer value which identifies a state in the FARS dataset. values are 1:51,
#' except 3
#'
#' @param year A 4 digit integer value indicating one of the years in the FARS dataset
#'
#' @return A plot of the states map with the coordinates of each fatality that year in
#'      that state plotted as a point
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'
#'@note if the state number isnt included in the dataset STATE column, it returns an error
#'
#'@example \dontrun{fars_map_state(1, 2013)}
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

