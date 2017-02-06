# REQUIRE TEST mlogit example --------------------------------------------------

test_that('', {
    data(mexico)
    z.out <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok,
                 model = "mlogit", data = mexico)
    x.out <- setx(z.out)
    expect_error(s.out <- sim(z.out, x.out), NA)
})