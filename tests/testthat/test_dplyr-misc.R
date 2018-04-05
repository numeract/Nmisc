context("Testing dplyr-misc")

test_that("pull_with_names works", {
    result <- pull_with_names(iris, 4, "Species")
    expect_equal(names(result), as.character(iris[["Species"]]))
})


test_that("pull_with_names stops with NA", {
    expect_error(pull_with_names(iris, 4, NA))
})


test_that("pull_with_names stops with NULL", {
    expect_error(pull_with_names(iris, 4, NULL))
})


test_that("pull_with_names stops with name_col vector", {
    expect_error(pull_with_names(iris, 1, c("Species1", "Species2")))
})


test_that("pull_with_names stops with NA var", {
    expect_error(pull_with_names(iris, NA, "Species"))
})
