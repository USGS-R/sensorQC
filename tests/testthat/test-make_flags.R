context('make flags')

test_that("define_flags", {
  
  flags = sensorQC:::define_flags('x > 3', 'is.na(x)')
  expect_equal(sapply(flags, function(x) x$expression), c("x > 3","is.na(x)"))
  flags = sensorQC:::define_flags('x > 3')
  expect_equal(sapply(flags, function(x) x$expression), "x > 3")
})

dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
values <- c(seq(1,12),NA,NA)
x <- data.frame("DateTime"=dates,"sensor.obs"=values)

test_that("re-use existing flags",{
  sn = sensor(x, flag.defs = 'x > 3')
  flags = sensorQC:::flags(sn)
  expect_equal(sapply(flags, function(x) x$expression), "x > 3")
  
  sn2 = sensor(sn, 'is.na(x)')
  flags = sensorQC:::flags(sn2)
  expect_equal(sapply(flags, function(x) x$expression), c("x > 3","is.na(x)"))
  
  sn3 = sensor(sn, list(expression='n > 2'))
  flags = sensorQC:::flags(sn3)
  expect_equal(sapply(flags, function(x) x$expression), c("x > 3","n > 2"))
})