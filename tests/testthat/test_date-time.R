context("Testing data-time.R")

test_that("is.POSIXct(x) works", {
    date <- as.POSIXct("2018/02/13 12-55-51")
    expect_true(is.POSIXct(date))
})

test_that("format_utc() works for Date", {
    date <- lubridate::ymd(20101215)
    result <- format_utc(date)

    expect_match(result, "2010-12-15")
})

test_that("format_utc() works for Date, with format", {
    date <- lubridate::ymd(20101215)
    result <- format_utc(date, "%Y/%m/%d")

    expect_match(result, "2010/12/15")
})

test_that("format_utc() works for POSIXct", {
    date <- lubridate::ymd_hms("2018/02/13 12-55-51")
    result <- format_utc(date)

    expect_match(result, "2018-02-13 12:55:51 UTC")
})

test_that("format_utc() works for POSIXct with format", {
    date <- lubridate::ymd_hms("2018-02-13 12:55:51")
    result <- format_utc(date, "%Y/%m/%d %H-%M-%S")

    expect_match(result, "2018/02/13 12-55-51 UTC")
})

test_that("format_utc() works for Date, with format, with tz", {
    date <- lubridate::ymd_hms("2018-02-13 12:55:51")
    lubridate::tz(date) <- "America/Chicago" # UTC-6
    result <- format_utc(date, format = "%Y/%m/%d %H-%M-%S", usetz = TRUE)

    expect_match(result, "2018/02/13 18-55-51 UTC")
})

test_that("format_utc() stops whith wrong input", {
    wrong_date <- "2018-02-13 12:55:51"
    expect_error(format_utc(wrong_date))
}) 
