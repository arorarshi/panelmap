context("check panelet continuous group label")

test_that("group labels unequal",{

  expect_error(panelet_continuous(mtcars$mpg, c("white","darkred"), mtcars$cyl[-1], "mpg"))

})

test_that("group labels NULL",{

  expect_error(panelet_continuous(mtcars$mpg, NULL, mtcars$cyl, "mpg"))

})

context("check panelet continuous group")

test_that("get pval summary",{

  expect_setequal(panelet_continuous(mtcars$mpg[order(mtcars$cyl)], c("white","darkred"), mtcars$cyl[order(mtcars$cyl)], "mpg", get.pval=TRUE)$tab[1,],
                 c( "26[21.4-33.9]","19.7[17.8-21.4]", "15.2[10.4-19.2]", "","P<0.0001"))

})


test_that("get pval summary for gr NULL",{

  expect_setequal(panelet_continuous(mtcars$mpg[order(mtcars$cyl)], c("white","darkred"), gr=NULL, "mpg", get.pval=TRUE)$tab[1,],
                  c("19.2(10.4,33.9); NA=0"))

})


