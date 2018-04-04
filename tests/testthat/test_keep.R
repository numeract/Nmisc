context("Testing keep functions")

test_that("keep_if_in works", {
    expect_equal(keep_if_in(-1:10, 1:4), 1:4)
    expect_equal(keep_if_in(NA, 1:10), logical())
    expect_equal(keep_if_in("xyz", 1:10), character())
    expect_equal(keep_if_in(c(1, 4, NA, 5), 1:4), c(1, 4))
})


test_that("keep_if_not_in works", {
    expect_equal(keep_if_not_in(-1:10, 1:4), c(-1, 0, 5:10))
    expect_equal(keep_if_not_in(NA, 1:10), NA)
    expect_equal(keep_if_not_in(1:10, 1:10), numeric())
})
