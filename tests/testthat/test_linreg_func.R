context("linreg function")




test_that("function rejects errounous input.", {
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  #expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
  
  
  
  
})

testobject <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
controlobject <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)



test_that("function returns correct outputs.", {
  expect_is(testobject, class = "linreg")
  expect_equal(is.list(testobject), TRUE)
  expect_equal(as.vector(unname(testobject$B_hat)), as.vector(unname(controlobject$coefficients)))
  expect_equal(as.vector(unname(testobject$y_hat)), unname(controlobject$fitted.values))
  expect_equal(as.vector(unname(testobject$res)), unname(controlobject$residuals))
  expect_equal(as.vector(unname(testobject$df)), unname(controlobject$df.residual))
#   expect_equal(as.vector(unname(testobject$p_values)), ?????) 
  
  
})








