context("errors with illdefined group labels")

test_that("group labels with NA",{

  expect_error(panelet_group(gr = c(rep(1,10), c(rep(2,10),NA,NA) ),gr.col=c("red","black") ,gr.name="group"))

})

test_that("group labels with NULL",{

  expect_error(panelet_group(gr = c(rep(1,10), c(rep(2,10)) ),gr.col=NULL ,gr.name="group"))

})


context("legend options")

test_that("legend output is ok",{

  expect_setequal(panelet_group(gr=c(rep(1,10), gr.col=c(rep(2,10))),c("red","black"), legend=TRUE)$key[,1], c("group=1","red"))

})

test_that("legend output is ok for factor",{

  expect_setequal(panelet_group(gr=factor(c(rep(1,10), gr.col=c(rep(2,10))), levels=c(2,1)),c("red","black") ,gr.name="groupf", legend=TRUE)$key[,1], c("groupf=2","red"))

})
