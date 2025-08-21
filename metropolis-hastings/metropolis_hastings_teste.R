
# teste  ------------------------------------------------------------------

# distribuição alvo = exponencial 
# distribuição proposta = normal
target = function(x){
  return(ifelse(x<0,0,exp(-x)))
}

x = rep(2,10)
x[1] = 3     
for(i in 2:10){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x) 
  if(runif(1)<A){
    x[i] = proposed_x       
  } else {
    x[i] = current_x        
  }
}
x

plot(x,main="values of x visited by the MH algorithm")


# aplicação rayleight -----------------------------------------------------

# distribuição alvo = rayleight 
# distribuição proposta = exponencial

  

# Função densidade da distribuição Rayleigh com sigma = 4
rayleigh_target <- function(x, sigma = 4) {
  ifelse(x < 0, 0, (x / sigma^2) * exp(-x^2 / (2 * sigma^2)))
}

# Função densidade da Exponencial(1)
exp_proposal <- function(x, lambda = 1) {
  ifelse(x < 0, 0, lambda * exp(-lambda * x))
}

# Algoritmo de Metropolis-Hastings
set.seed(123)  # para reprodutibilidade
n_samples <- 10000
samples <- numeric(n_samples)
samples[1] <- rexp(1)  # ponto inicial

for (i in 2:n_samples) {
  current_x <- samples[i - 1]
  proposed_x <- rexp(1)  # proposta a partir de Exponencial(1)
  
  # Cálculo da razão de aceitação
  A <- (rayleigh_target(proposed_x) / rayleigh_target(current_x)) *
    (exp_proposal(current_x) / exp_proposal(proposed_x))
  
  # Aceita ou rejeita
  if (runif(1) < A) {
    samples[i] <- proposed_x
  } else {
    samples[i] <- current_x
  }
}

# Visualização
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Amostras via Metropolis-Hastings",
     xlab = "x")
curve((x / 16) * exp(-x^2 / (2 * 16)), add = TRUE, col = "red", lwd = 2)  # Densidade Rayleigh(σ=4)


# comparando --------------------------------------------------------------

library(VGAM)

# Função densidade da distribuição Rayleigh com sigma = 4
rayleigh_target <- function(x, sigma = 4) {
  ifelse(x < 0, 0, (x / sigma^2) * exp(-x^2 / (2 * sigma^2)))
}

# Função densidade da Exponencial(1)
exp_proposal <- function(x, lambda = 1) {
  ifelse(x < 0, 0, lambda * exp(-lambda * x))
}

# Algoritmo de Metropolis-Hastings
set.seed(123)
n_samples <- 10000
samples <- numeric(n_samples)
samples[1] <- rexp(1)

for (i in 2:n_samples) {
  current_x <- samples[i - 1]
  proposed_x <- rexp(1)
  
  A <- (rayleigh_target(proposed_x) / rayleigh_target(current_x)) *
    (exp_proposal(current_x) / exp_proposal(proposed_x))
  
  if (runif(1) < A) {
    samples[i] <- proposed_x
  } else {
    samples[i] <- current_x
  }
}

# Gráfico: histograma + densidade teórica com dRayleigh
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Amostras Metropolis-Hastings vs Rayleigh(σ=4)",
     xlab = "x", xlim = c(0, max(samples)))

# Curva da densidade Rayleigh(σ=4) com dRayleigh do pacote VGAM
curve(drayleigh(x, scale = 4), add = TRUE, col = "red", lwd = 2)

legend("topright", legend = c("Densidade teórica Rayleigh(σ=4)"),
       col = c("red"), lwd = 2)

samples

mean(samples)
sd(samples)^2


# gamma -------------------------------------------------------------------

# comparando --------------------------------------------------------------

library(VGAM)

# Função densidade da distribuição Rayleigh com sigma = 4
rayleigh_target <- function(x, sigma = 4) {
  ifelse(x < 0, 0, (x / sigma^2) * exp(-x^2 / (2 * sigma^2)))
}

# Função densidade da Exponencial(1)
exp_proposal <- function(x, lambda = 1) {
  ifelse(x < 0, 0, lambda * exp(-lambda * x))
}

# Algoritmo de Metropolis-Hastings
set.seed(123)
n_samples <- 10000
samples <- rpois(n_samples, lambda = 6)
samples[1] <- rexp(1)

for (i in 2:n_samples) {
  current_x <- samples[i - 1]
  proposed_x <- rexp(1)
  
  A <- (rayleigh_target(proposed_x) / rayleigh_target(current_x)) *
    (exp_proposal(current_x) / exp_proposal(proposed_x))
  
  if (runif(1) < A) {
    samples[i] <- proposed_x
  } else {
    samples[i] <- current_x
  }
}

# Gráfico: histograma + densidade teórica com dRayleigh
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Amostras Metropolis-Hastings vs Rayleigh(σ=4)",
     xlab = "x", xlim = c(0, max(samples)))

# Curva da densidade Rayleigh(σ=4) com dRayleigh do pacote VGAM
curve(drayleigh(x, scale = 4), add = TRUE, col = "red", lwd = 2)

legend("topright", legend = c("Densidade teórica Rayleigh(σ=4)"),
       col = c("red"), lwd = 2)

samples

mean(samples)
sd(samples)^2

