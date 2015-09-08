context("Test class")


test_that("webprocess can set algorithms", {
  expect_is(sensor(data.frame(c(1,1,1))), 'sensor')
})