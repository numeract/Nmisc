context("Testing warning function")


test_that("clear_warnings works", {
    x <- as.numeric(c("1", "2", "X"))
    clear_warnings()
    last_warning <- baseenv()[['last.warning']]
    expect_equal(length(last_warning), 0)
})
