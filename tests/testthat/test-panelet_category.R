context("check panelet category group label")

test_that("group labels unequal",{

  expect_error(panelet_category(mtcars$am, c("white","black"), mtcars$cyl[-1], "am"))

})

test_that("group labels NULL",{

  expect_error(panelet_category(mtcars$am, c("white"),gr=NULL, "am"))

})

context("get pval category")

test_that("get pval summary",{

  expect_setequal(panelet_category(mtcars$am[order(mtcars$cyl)], c("white", "black"),gr=mtcars$cyl[order(mtcars$cyl)], "am", get.pval=TRUE)$tab[2,],
                 c("3(27%)","4(57%)","12(86%)","19(59.38%)",""))

})


test_that("get pval for gr NULL",{

  expect_setequal(panelet_category(mtcars$am, c("white", "black"),gr=NULL, "am", get.pval=TRUE)$tab[,1],
                  c("19(59.38%)", "13(40.62%)"))

})


context("check panelet category legend")

test_that("legend output",{

  expect_setequal(panelet_category(mtcars$am[order(mtcars$cyl)], c("white", "black"),gr=NULL, "am", get.pval=TRUE, legend=TRUE)$key[,1], c("am=0","white"))

})
