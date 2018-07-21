set.seed(4321)
x <- seq(0, 100, 10)
y <- ((runif(1, 10, 20) * x) / (runif(1, 0, 10) + x)) + rnorm(11, 0, 1)
x_1 <- seq(0, 100, 10)
y_1 <- ((runif(1, 10, 20) * x_1) / (runif(1, 0, 200) + x_1)) + rnorm(11, 0, 1)

test <- tibble(
  x = c(x, x_1),
  y = c(y, y_1),
  g = rep(c("a", "b"), each = 11)
)

ggplot(test, aes(x, y, colour = g)) +
  geom_point()

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

filter(test, g == "a") %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10))

filter(test, g == "a") %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x))

