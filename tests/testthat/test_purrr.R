context("Testing purrr-misc.R")

# Keep --------------------------------------------------------------------


test_that("keep_at(.x, .at) works for numeric input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    result <- keep_at(x, 1)

    expect_equal(result, x[1])
})

test_that("keep_at(.x, .at) works for character input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    result <- keep_at(x, "Second")

    expect_equal(result, x[2])
})

test_that("keep_at(.x, .at) keeps multiple values", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Second")
    result <- keep_at(x, "Second")

    expect_equal(result, x[2:3])
})

# Discard -----------------------------------------------------------------


test_that("discard_at(.x, .at) works for numeric input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    result <- discard_at(x, 1)

    expect_equal(result, x[2:3])
})

test_that("keep_at(.x, .at) works for character input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    result <- discard_at(x, "Last")

    expect_equal(result, x[1:2])
})

test_that("keep_at(.x, .at) discards multiple values", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Second")
    result <- discard_at(x, "Second")

    expect_equal(result, x[1])
})