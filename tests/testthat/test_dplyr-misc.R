context("Testing dplyr-misc")

test_that("pull_with_names works", {
    result <- pull_with_names(iris, 4, "Species")
    expect_equal(names(result), as.character(iris[["Species"]]))
})
