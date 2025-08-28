# -----------------------------------------------------
# Aplicação Rayleigh com proposta Log-Normal
# -----------------------------------------------------

# Distribuição alvo = Rayleigh
# Distribuição proposta = Log-Normal

# Função densidade da distribuição Rayleigh com sigma = 4
rayleigh_target <- function(x, sigma = 4) {
  ifelse(x < 0, 0, (x / sigma^2) * exp(-x^2 / (2 * sigma^2)))
}

# Função densidade da Log-Normal
lognorm_proposal <- function(x, meanlog = 0, sdlog = 1) {
  dlnorm(x, meanlog = meanlog, sdlog = sdlog)
}

# Parâmetros da proposta Log-Normal
meanlog <- 0
sdlog <- 1

# Algoritmo de Metropolis-Hastings
set.seed(123)  # para reprodutibilidade
n_samples <- 10000
samples <- numeric(n_samples)
samples[1] <- rlnorm(1, meanlog = meanlog, sdlog = sdlog)  # ponto inicial

for (i in 2:n_samples) {
  current_x <- samples[i - 1]
  proposed_x <- rlnorm(1, meanlog = meanlog, sdlog = sdlog)  # proposta a partir de Log-Normal
  
  # Cálculo da razão de aceitação
  A <- (rayleigh_target(proposed_x) / rayleigh_target(current_x)) *
    (lognorm_proposal(current_x, meanlog, sdlog) / lognorm_proposal(proposed_x, meanlog, sdlog))
  
  # Aceita ou rejeita
  if (runif(1) < A) {
    samples[i] <- proposed_x
  } else {
    samples[i] <- current_x
  }
}

# Visualização
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Amostras via Metropolis-Hastings (Proposta Log-Normal)",
     xlab = "x",
     ylab = "Densidade")
curve((x / 16) * exp(-x^2 / (2 * 16)), add = TRUE, col = "red", lwd = 2)  # Densidade Rayleigh(σ=


media_obs <- mean(samples)
var_obs <- sd(samples)^2

# valor teorico

media_teorica <- ((4-pi)/2)*(4^2)
var_teorica <- 4*sqrt(pi/ 2)

tab1 <- 
  cbind(
      media = c(media_teorica, media_obs),
      var = c(var_teorica, var_obs)
      ) %>% t() %>% round(4) %>%  
  as.data.frame() 
  
colnames(tab1) <- c("Valor teórico", "Média amostral")

tab1 %>% 
  xtable::xtable()
