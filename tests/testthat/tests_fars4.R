

context('Errors')

test_that('Throws errors', {
  throws_error(fars_read_years(years = 1999))
  throws_error(fars_summarize_years(years = 1999))
  throws_error(make_filename(year = 'two thousand fifteen'))
  
  library(mapdata)
  throws_error(fars_map_state(3, 2015))
  throws_error(fars_map_state(36, 1999))
})

context('File load and summary')

test_that('Mapping works', {
  library(mapdata)
  map <- fars_map_state(36, 2014)
  expect_that(map, is_null())
})