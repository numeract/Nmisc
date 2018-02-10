context("Testing warning function")


test_that("clear_warnings works", {
    x <- as.numeric(c("1", "2", "X"))
    clear_warnings()
    expect_equal(length(last.warning), 0)
})
