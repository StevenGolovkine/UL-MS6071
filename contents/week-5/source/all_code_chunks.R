################################################################################
# Script file for Week 05 - Reproducibility
################################################################################

library(devtools)

create_package('./converter')

meter_to_yard <- function(x) {
    1.0936 * x
}

yard_to_meter <- function(x) {
    0.9144 * x
}


use_r("conversion")

load_all()

check()


#' Convert meter to yard
#'
#' @param x A number.
#'
#' @return A number
#' @export
#'
#' @examples
#' meter_to_yard(1)
meter_to_yard <- function(x) {
    1.0936 * x
}

#' Convert yard to meter
#'
#' @param x A number.
#'
#' @return A number
#' @export
#'
#' @examples
#' yard_to_meter(1)
yard_to_meter <- function(x) {
    0.9144 * x
}

document()

use_testthat()

use_test("meter_to_yard")

test_that("meter_to_yard() works", {
  expect_equal(meter_to_yard(1), 1.0936)
})

test()

use_readme_rmd()

install()


