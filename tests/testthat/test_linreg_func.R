context("linreg function")




test_that("function rejects errounous input.", {
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


testobject <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
controlobject <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

test_that("function returns correct results from specific data.", {
  expect_is(testobject, class = "linreg")
  expect_true(is.list(testobject))
  expect_equal(length(testobject), 15)
  expect_equal(as.vector(unname(testobject$B_hat)), as.vector(unname(controlobject$coefficients)))
  expect_equal(as.vector(unname(testobject$y_hat)), unname(controlobject$fitted.values))
  expect_equal(as.vector(unname(testobject$res)), unname(controlobject$residuals))
  expect_equal(as.vector(unname(testobject$df)), unname(controlobject$df.residual))
  #   expect_equal(as.vector(unname(testobject$p_values)), ?????) 
  
  
})



context("linreg methods")


test_that("function rejects errounous input.", {
 expect_error(Lab4::print.linreg("a"))
 expect_error(Lab4::plot.linreg("a"))
 expect_error(Lab4::summary.linreg("a"))
 expect_error(Lab4::predict.linreg("a"))
 expect_error(Lab4::residuals.linreg("a"))
 expect_error(Lab4::coefficients.linreg("a"))
})

test_that("function returns correct results from specific data.", {

  expect_equal(as.vector(unname(predict(testobject))),unname(controlobject$fitted.values))
  expect_equal(as.vector(unname(resid(testobject))),unname(controlobject$residuals))
  expect_equal(as.vector(unname(coef(testobject))),unname(controlobject$coef))
  
  
})


