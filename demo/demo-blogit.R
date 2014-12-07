# Zelig 4 code:
# library(Zelig4)
library(ZeligChoice)
data(sanction)
z.out1 <- Zelig4::zelig(cbind(import, export) ~ coop + cost + target,
                model = "blogit", data = sanction)
summary(z.out1)
x.low <- Zelig4::setx(z.out1, cost = 1)
set.seed(42)
s.out1 <- Zelig4::sim(z.out1, x.low, num=100)
summary(s.out1)

# Zelig 5 code:
data(sanction)
z5 <- zblogit$new()
z5$zelig(cbind(import, export) ~ coop + cost + target, data = sanction)
z.out2 <- zelig(cbind(import, export) ~ coop + cost + target, model = "blogit", data = sanction)
z.out2 <- zelig(list(import ~ coop, export ~ cost + target), model = "blogit", data = sanction)
z5$zelig(list(import ~ coop, export ~ cost + target), model = "blogit", data = sanction)

z.out2 <- zelig(import ~ coop, model = "logit", data = sanction)

z5
z5$zelig.out$z.out[1]
z5$setx(cost = 1)
z5
set.seed(42)
z5$sim(num = 100)
simparam <- z5$simparam$simparam[[1]]
mm <- z5$setx.out$x$mm[[1]]

q <- z5$qi(simparam, mm)
names(q) <- c("ev", "pv")
Zelig:::statmat(q$ev)
Zelig:::statmat(q$pv)

z5$sim.out$x[[1]]
z5$summarize()
 z5$cite()

