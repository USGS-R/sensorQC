context("expression parsing")

test_that("parse simple variable", {
  
  expr_var = sensorQC:::expr_var
  
  expect_equal(expr_var('MAD(x)'), 'x')
  expect_equal(expr_var('x == 9'), 'x')
  expect_equal(expr_var('y == 9'), 'y')
})

test_that("parse multiple variables", {
  
  expr_var = sensorQC:::expr_var
  
  expect_equal(expr_var('MAD(x)'), 'x')
  expect_equal(expr_var('MAD(x,y)'), c('x','y'))
  
})
