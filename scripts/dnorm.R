

Vtas <- seq(from = 165 - 65, to = 165 + 65, by = 1)

y <- dnorm(Vtas, mean = 165, sd = 21)

plot(Vtas, y)