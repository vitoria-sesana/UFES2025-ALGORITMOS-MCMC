y <- data$Contagem
n <- length(y)

# ----------------------------
# 2. Hiperparâmetros
# ----------------------------
alpha <- 1
beta <- 1
gamma <- 1
delta <- 1

n_iter <- 10000

# ----------------------------
# 3. Inicialização
# ----------------------------
lambda_samples <- numeric(n_iter)
nu_samples <- numeric(n_iter)
k_samples <- numeric(n_iter)

lambda <- 1
nu <- 1
k <- floor(n / 2)

# ----------------------------
# 4. Gibbs Sampling
# ----------------------------
for (t in 1:n_iter) {
  # Atualizar lambda
  shape_lambda <- alpha + sum(y[1:k])
  rate_lambda <- beta + k
  lambda <- rgamma(1, shape_lambda, rate_lambda)
  
  # Atualizar nu
  shape_nu <- gamma + sum(y[(k+1):n])
  rate_nu <- delta + (n - k)
  nu <- rgamma(1, shape_nu, rate_nu)
  
  # Atualizar k
  log_probs <- numeric(n - 1)
  for (ki in 1:(n - 1)) {
    sum1 <- sum(y[1:ki])
    sum2 <- sum(y[(ki + 1):n])
    log_probs[ki] <- sum1 * log(lambda) - ki * lambda +
      sum2 * log(nu) - (n - ki) * nu
  }
  
  probs <- exp(log_probs - max(log_probs)) # estabilização
  probs <- probs / sum(probs)
  k <- sample(1:(n - 1), 1, prob = probs)
  
  # Armazenar
  lambda_samples[t] <- lambda
  nu_samples[t] <- nu
  k_samples[t] <- k
}


# analises ----------------------------------------------------------------
# -------------------------------------------------------


# Estimativas pontuais
lambda_est <- mean(lambda_samples)
nu_est <- mean(nu_samples)
k_est <- round(mean(k_samples))

cat("Estimativas pontuais:\n")
cat(sprintf("λ (antes): %.2f\n", lambda_est))
cat(sprintf("ν (depois): %.2f\n", nu_est))
cat(sprintf("Ponto de mudança estimado (k): %d (ano %d)\n", k_est, 1850 + k_est))

# Intervalos de credibilidade
ci_lambda <- quantile(lambda_samples, c(0.025, 0.975))
ci_nu <- quantile(nu_samples, c(0.025, 0.975))
ci_k <- quantile(k_samples, c(0.025, 0.975))

cat("\nIntervalos de credibilidade 95%:\n")
cat(sprintf("λ: (%.2f, %.2f)\n", ci_lambda[1], ci_lambda[2]))
cat(sprintf("ν: (%.2f, %.2f)\n", ci_nu[1], ci_nu[2]))
cat(sprintf("k: (%d, %d) → anos (%d, %d)\n",
            ci_k[1], ci_k[2], 1850 + ci_k[1], 1850 + ci_k[2]))


plot(data$Ano, y, type = "b", pch = 16, col = "darkblue",
     xlab = "Ano", ylab = "Número de desastres",
     main = "Desastres em Minas de Carvão com Ponto de Mudança Estimado")
abline(v = 1850 + k_est, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Ponto de mudança:", 1850 + k_est),
       col = "red", lty = 2, lwd = 2)

estimativas <- rbind(lambda_est, nu_est, k_est) %>% 
  round(2)

ics <- 
  rbind(t(ci_lambda), t(ci_nu), t(ci_k)) %>% 
  round(2)

cbind(estimativas, ics) %>% 
  xtable::xtable()

