context("Testing keep functions")

test_that("keep_if_in works", {
    expect_equal(keep_if_in(-1:10, 1:4), 1:4)
    expect_equal(keep_if_in("xyz", 1:10), character())
    expect_equal(keep_if_in(c(1, 4, NA, 5 ), 1:4), c(1, 4))
    expect_equal(keep_if_in(1, 1:4), 1)
})


test_that("keep_if_in stops with x NULL", {
    expect_error(keep_if_in(NULL, 1:10))
})

test_that("keep_if_in stops with x NA", {
    expect_error(keep_if_in(NA, 1:10))
    expect_error(keep_if_in(NA_character_, 1:10))
    expect_error(keep_if_in(NA_real_, 1:10))
})


test_that("keep_if_in stops with x empty vector", {
    expect_error(keep_if_in(c(), 1:10))
})


test_that("keep_if_in stops with y NULL", {
    expect_error(keep_if_in(1:10, NULL))
})


test_that("keep_if_in stops with y empty vector", {
    expect_error(keep_if_in(1:10, c()))
})

test_that("keep_if_in stops with y NA", {
    expect_error(keep_if_in(1:10, NA))
})


test_that("keep_if_not_in works", {
    expect_equal(keep_if_not_in(-1:10,1:4), c(-1,0,5:10))
    expect_equal(keep_if_not_in(1:10, 1:10), numeric())
})


test_that("keep_if_not_in stops with x NULL", {
    expect_error(keep_if_not_in(NULL, 1:10))
})


test_that("keep_if_not_in stops with x NA", {
    expect_error(keep_if_not_in(NA, 1:10))
    expect_error(keep_if_not_in(NA_character_, 1:10))
    expect_error(keep_if_not_in(NA_real_, 1:10))
})


test_that("keep_if_not_in stops with x empty vector", {
    expect_error(keep_if_not_in(c(), 1:10))
})


test_that("keep_if_in stops with y NULL", {
    expect_error(keep_if_not_in(1:10, NULL))
})


test_that("keep_if_in stops with y empty vector", {
    expect_error(keep_if_not_in(1:10, c()))
})


test_that("keep_if_in stops with y NA", {
    expect_error(keep_if_not_in(1:10, NA))
})


test_that("setequal_na works", {
    expect_equal(setequal_na(c(2, 1, 3), c(1, 2, 3)), TRUE)
    expect_equal(setequal_na(c(1, NA, 3), c(3, NA, 1), na.rm = TRUE), TRUE)
    expect_equal(setequal_na(c(NA, NA), c(NA), na.rm = TRUE), TRUE)
    expect_equal(setequal_na(c(NA, NA), c(NA)), TRUE)
    expect_equal(setequal_na(c(1, 2, 3), c(1, 2, 3, NA)), FALSE)
    expect_equal(setequal_na(c(NA, 1), c(1), na.rm = TRUE), TRUE)
})


test_that("setequal_na stops with NULL inputs", {
    expect_error(setequal_na(NULL, c(1, 2)))
    expect_error(setequal_na(c(1, 2), NULL))
})


test_that("setequal_na stops with empty inputs", {
    expect_error(setequal_na(2, c()))
    expect_error(setequal_na(c(), 2))
})

