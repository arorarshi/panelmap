context("errors with illdefined group labels")

test_that("group labels with NA",{

  expect_error(panelet_group(c(rep(1,10), c(rep(2,10),NA,NA)),c("red","black") ,soln.name="group"))

})
