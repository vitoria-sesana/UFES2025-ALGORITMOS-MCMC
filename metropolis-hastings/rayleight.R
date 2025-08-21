 # install.packages("VGAM")
library(VGAM)

set.seed(123)

scale_param <- 4

rayleigh_samples <- rrayleigh(n = 100000, scale = scale_param)

print(head(rayleigh_samples))

hist(rayleigh_samples, breaks = 20, main = "Histogram of Rayleigh Samples",
     xlab = "Value", ylab = "Frequency", col = "lightblue")



drayleigh(4, scale = 4, log = FALSE)
dexp(4, 1, log = FALSE)

4*sqrt(pi/ 2)
mean(rayleigh_samples)

((4-pi)/2)*(4^2)
sd(rayleigh_samples)^2
