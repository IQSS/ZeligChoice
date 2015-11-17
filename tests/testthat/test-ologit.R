z <- zologit$new()
test <- z$mcunit(minx=-2, maxx=4, plot=FALSE)
expect_true(test)