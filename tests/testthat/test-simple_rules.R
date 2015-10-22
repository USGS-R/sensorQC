context("Test simple rules")

values.1 <- c(seq(1,12),100,NA,NA)
values.2 <- c(seq(1,12),100,1,1)
values.3 <- c(seq(1,12),100,1/0,1)
test_that("MAD(x)", {
  expect_true(MAD(values.1)[13] > 3)
  expect_true(MAD(values.2)[13] > 3)
  expect_true(MAD(values.3)[13] > 3)
})
