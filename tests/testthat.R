 library(testthat)
library(fars4)

#test_check("fars4")
test_that("File name",{
  filename <- make_filename(2015)
  expect_that(filename,equals("accident_2015.csv.bz2"))
})