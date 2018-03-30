context("Testing purrr-misc.R")

# Keep --------------------------------------------------------------------
test_that("keep_at(.x, .at) works zero length input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(keep_at(x, NULL), x[0])
    expect_equal(keep_at(x, character()), x[0])
    expect_equal(keep_at(l, NULL), l[0])
    expect_equal(keep_at(l, character()), l[0])
})


test_that("keep_at(.x, .at) works with NA", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(keep_at(x, NA_character_), x[0])
    expect_equal(keep_at(x, NA_integer_), x[0])
    expect_equal(keep_at(x, NA), x[0])
    expect_equal(keep_at(x, c(1, 2, NA)), x[0])
    expect_equal(keep_at(x, c(NA, 2, NA)), x[0])
    expect_equal(keep_at(x, c(NA, NA, NA)), x[0])
})


test_that("keep_at(.x, .at) works for numeric input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(keep_at(x, 1), x[1])
    expect_equal(keep_at(l, 1), l[1])
})

test_that("keep_at(.x, .at) works for character input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(keep_at(x, "Second"), x[2])
    expect_equal(keep_at(l, "Second"), l[2])
})


test_that("keep_at(.x, .at) keeps multiple values", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Second")
    l <- as.list(x)
    
    expect_equal(keep_at(x, "Second"), x[2:3])
    expect_equal(keep_at(l, "Second"), l[2:3])
})


test_that("keep_at(.x, .at) stops with wrong input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_error(keep_at(x, l))
})


# Discard -----------------------------------------------------------------
test_that("discard_at(.x, .at) works zero length input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(discard_at(x, NULL), x)
    expect_equal(discard_at(x, character()), x)
    expect_equal(discard_at(l, NULL), l)
    expect_equal(discard_at(l, character()), l)
})


test_that("discard_at(.x, .at) works with NA", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(discard_at(x, NA_character_), x)
    expect_equal(discard_at(x, NA_integer_), x)
    expect_equal(discard_at(x, NA), x)
})


test_that("discard_at(.x, .at) works for numeric input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(discard_at(x, 1), x[2:3])
    expect_equal(discard_at(l, 1), l[2:3])
})


test_that("discard_at(.x, .at) works for character input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_equal(discard_at(x, "Last"), x[1:2])
    expect_equal(discard_at(l, "Last"), l[1:2])
})


test_that("discard_at(.x, .at) discards multiple values", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Second")
    l <- as.list(x)
    
    expect_equal(discard_at(x, "Second"), x[1])
    expect_equal(discard_at(l, "Second"), l[1])
})


test_that("discard_at(.x, .at) stops with wrong input", {
    x <- c(1, 2, 3)
    names(x) <- c("First", "Second", "Last")
    l <- as.list(x)
    
    expect_error(discard_at(x, l))
})
