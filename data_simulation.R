set.seed(20160227)
x <- seq(0,100,1)
y <- ((runif(1,10,20)*x)/(runif(1,0,10) + x)) + rnorm(101,0,2)
x_1 <- seq(0,100,1)
y_1 <- ((runif(1,10,20)*x_1)/(runif(1,0,100) + x_1)) + rnorm(101,0,2)

test <- tibble(
  x = c(x, x_1),
  y = c(y, y_1),
  g = rep(c("a", "b"), each = 101)
)

ggplot(test, aes(x, y, group = g)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(test, aes(x, y, group = g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

ggplot(test, aes(x, y, group = g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3))

ggplot(test, aes(x, y, group = g)) +
  geom_point() +
  geom_smooth()
