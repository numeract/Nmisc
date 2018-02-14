context("Testing sequence functions")

df1 <- data.frame(col1 = c(1:4), col2  = c(5,9))
df2 <- data.frame(col1 = c(1:4))
df3 <- data.frame(col1 = c(NA), col2 = c(1))
df4 <- data.frame(col1 = numeric(), col2 = numeric())

test_that("seq_nrow works", {
    expect_equal(seq_nrow(df1), 1:4)
    expect_equal(seq_nrow(df2), 1:4)
    expect_equal(seq_nrow(df3), 1)
})

test_that("seq_ncol works", {
    expect_equal(seq_ncol(df1), 1:2)
    expect_equal(seq_ncol(df2), 1)
    expect_equal(seq_ncol(df3), 1:2)
    expect_equal(seq_ncol(df4), 1:2)
})
