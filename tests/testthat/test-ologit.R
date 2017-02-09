#### Ordered Logistic Regression Tests ####

# REQUIRE TEST Monte Carlo ologit ----------------------------------------------
test_that('REQUIRE TEST Monte Carlo ologit', {
    z <- zologit$new()
    test <- z$mcunit(minx = 0, maxx = 2, plot = FALSE)
    expect_true(test)
})
