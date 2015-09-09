context("Test flag logic")

dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
values <- c(seq(1,12),NA,NA)
sensor <- sensor(data.frame("DateTime"=dates,"sensor.obs"=values))
test_that("is.na(x)", {
  expect_equal(sum(calc_flags(sensor, 'is.na(x)', which.flagged=FALSE)), 2)
  expect_equal(length(calc_flags(sensor, 'is.na(x)')), 2)
})

values = c(1,3,2,3,4,5,5,5,4,3,5,NA,5,NA)
sensor <- sensor(data.frame("DateTime"=dates,"sensor.obs"=values))
test_that("persistent", {
  expect_equal(sum(calc_flags(sensor, 'n > 3', which.flagged=FALSE)), 0)
  expect_equal(length(calc_flags(sensor, 'n > 2')), 3)
  expect_equal(length(calc_flags(sensor, 'is.na(x)')), 2)
})
