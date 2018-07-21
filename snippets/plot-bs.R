library(mgcv)

# Plot Thin Plate regression splines

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

# Plot cubic regression splines

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

# Plot P-splines

x1 <- 1:100
um <- smoothCon(
  s(x1, bs = "ps"),
  data = data.frame(x1 = sort(x1)),
  knots = NULL,
  absorb.cons = TRUE
)
X <- um[[1]]$X
plot(sort(x1), X[, 1], ylim = range(X), type = "l")
for (i in 2:ncol(X))
  lines(sort(x1), X[, i], col = i)

# Plot Duchon splines

x1 <- 1:100
um <- smoothCon(
  s(x1, bs = "ds"),
  data = data.frame(x1 = sort(x1)),
  knots = NULL,
  absorb.cons = TRUE
)
X <- um[[1]]$X
plot(sort(x1), X[, 1], ylim = range(X), type = "l")
for (i in 2:ncol(X))
  lines(sort(x1), X[, i], col = i)
