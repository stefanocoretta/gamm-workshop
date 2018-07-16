library(mgcv)
x1 <- 1:1000
um <- smoothCon(
  s(x1),
  data = data.frame(x1 = sort(x1)),
  knots = NULL,
  absorb.cons = TRUE
)
X <- um[[1]]$X
plot(sort(x1), X[, 1], ylim = range(X), type = "l")
for (i in 2:ncol(X))
  lines(sort(x1), X[, i], col = i)

x1 <- 1:100
um <- smoothCon(
  s(x1, bs = "cr"),
  data = data.frame(x1 = sort(x1)),
  knots = NULL,
  absorb.cons = TRUE
)
X <- um[[1]]$X
plot(sort(x1), X[, 1], ylim = range(X), type = "l")
for (i in 2:ncol(X))
  lines(sort(x1), X[, i], col = i)

X <- predict(
  simple,
  newdata = data.frame(x = sim_traj_a$x),
  type = "lpmatrix"
)[,2:10]

plot(sim_traj_a$x, X[, 1], type = "n", ylim = range(X))
for (i in 1:ncol(X)) {
  lines(sim_traj_a$x, X[, i], col = i, lw = 1)
}

X2 <- X %*% diag(coef(simple)[2:10])
plot(
  sim_traj_a$x,
  predict(simple, newdata = data.frame(x = sim_traj_a$x)) -
    coef(simple)[1],
  type = "n",
  ylim = c(-8, 3))
for (i in 1:ncol(X2)) {
  lines(sim_traj_a$x, X2[, i], col = i, lw = 1)
}
lines(
  sim_traj_a$x,
  predict(simple, newdata = data.frame(x = sim_traj_a$x)) -
    coef(simple)[1],
  lw = 3
)
