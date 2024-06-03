#Ex D1
# Citirea datelor din fisierul CSV
data <- read.csv("probabilitati.csv", header = FALSE)
values <- unlist(data)

# Calcularea statisticilor esantionului
mean_sample <- mean(values)
n <- length(values)
sigma <- sqrt(92.16)

# Calcularea intervalelor de incredere
confidence_95 <- 1.96
confidence_99 <- 2.576

margin_error_95 <- confidence_95 * (sigma / sqrt(n))
margin_error_99 <- confidence_99 * (sigma / sqrt(n))

lower_bound_95 <- mean_sample - margin_error_95
upper_bound_95 <- mean_sample + margin_error_95

lower_bound_99 <- mean_sample - margin_error_99
upper_bound_99 <- mean_sample + margin_error_99

# Afișarea rezultatelor
cat("Intervalul de încredere de 95%: [", lower_bound_95, ", ", upper_bound_95, "]\n")
cat("Intervalul de încredere de 99%: [", lower_bound_99, ", ", upper_bound_99, "]\n")

#EX D2
# Citirea datelor din fisierul CSV
data <- read.csv("statistica.csv", header = FALSE)

# Conversia datelor intr-un vector numeric
values <- as.numeric(data$V1)

# Calcularea statisticilor esantionului
mean_sample <- mean(values, na.rm = TRUE)
sd_sample <- sd(values, na.rm = TRUE)
n <- length(values)

# Calcularea intervalului de incredere de 95%
confidence_level_95 <- 0.95
error_margin_95 <- qt(1 - (1 - confidence_level_95) / 2, df = n - 1) * (sd_sample / sqrt(n))
lower_bound_95 <- mean_sample - error_margin_95
upper_bound_95 <- mean_sample + error_margin_95

# Calcularea intervalului de incredere de 99%
confidence_level_99 <- 0.99
error_margin_99 <- qt(1 - (1 - confidence_level_99) / 2, df = n - 1) * (sd_sample / sqrt(n))
lower_bound_99 <- mean_sample - error_margin_99
upper_bound_99 <- mean_sample + error_margin_99

# Afisarea rezultatelor
cat("Intervalul de incredere de 95%: [", lower_bound_95, ", ", upper_bound_95, "]\n")
cat("Intervalul de incredere de 99%: [", lower_bound_99, ", ", upper_bound_99, "]\n")

#Ex D3
# Datele problemei
p_hat <- 14 / 100  # Proportia observata
p_0 <- 0.15       # Proportia asteptata
n <- 100          # Marimea esantionului

# Calculul statisticii z
z <- (p_hat - p_0) / sqrt((p_0 * (1 - p_0)) / n)

# Calculul valorilor critice
z_critical_5 <- qnorm(0.05)
z_critical_1 <- qnorm(0.01)

# Calculul valorii p
p_value <- pnorm(z)

# Afisarea rezultatelor
cat("Statistica z: ", z, "\n")
cat("Valoarea p: ", p_value, "\n")
cat("Valoarea critica pentru 5% nivel de semnificatie: ", z_critical_5, "\n")
cat("Valoarea critica pentru 1% nivel de semnificatie: ", z_critical_1, "\n")

# Concluzii
if (z < z_critical_5) {
  cat("La nivelul de semnificatie de 5%, respingem ipoteza nula. Schimbarea a fost utila.\n")
} else {
  cat("La nivelul de semnificatie de 5%, nu putem respinge ipoteza nula. Schimbarea nu a fost utila.\n")
}

if (z < z_critical_1) {
  cat("La nivelul de semnificatie de 1%, respingem ipoteza nula. Schimbarea a fost utila.\n")
} else {
  cat("La nivelul de semnificatie de 1%, nu putem respinge ipoteza nula. Schimbarea nu a fost utila.\n")
}

