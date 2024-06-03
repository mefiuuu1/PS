#Ex. B1
# Functie pentru a verifica daca un punct se afla in interiorul torului
is_inside_torus <- function(x1, x2, x3, R, r) {
  ((x3^2 + (sqrt(x1^2 + x2^2) - R)^2) <= r^2)
}

# Functie pentru a estima volumul torului folosind metoda Monte Carlo
monte_carlo_torus_volume <- function(R, r, samples) {
  # Initializam contorul pentru punctele din interiorul torului
  points_inside_torus <- 0
  
  # Generam punctele aleatoare
  for (i in 1:samples) {
    # Generam coordonatele aleatoare
    x1 <- runif(1, -R - r, R + r)
    x2 <- runif(1, -R - r, R + r)
    x3 <- runif(1, -r, r)
    
    # Verificam daca punctul se afla in interiorul torului
    if (is_inside_torus(x1, x2, x3, R, r)) {
      points_inside_torus <- points_inside_torus + 1
    }
  }
  
  # Calculam volumul torului
  torus_volume <- (points_inside_torus / samples) * (4 * pi^2 * R * r^2)
  
  return(torus_volume)
}

# Estimare pentru R = 10, r = 3 si diferite dimensiuni de esantioane
R <- 10
r <- 3
samples <- c(10000, 20000, 50000)

for (s in samples) {
  estimated_volume <- monte_carlo_torus_volume(R, r, s)
  exact_volume <- 2 * pi^2 * R * r^2
  
  relative_error <- abs(estimated_volume - exact_volume) / exact_volume * 100
  
  cat("Eșantion:", s, "\n")
  cat("Volum estimat:", estimated_volume, "\n")
  cat("Volum exact:", exact_volume, "\n")
  cat("Eroare relativă:", relative_error, "%\n\n")
}

#Ex B2
# Functie pentru a verifica daca un punct se afla in interiorul triunghiului T
is_inside_triangle <- function(x, y) {
  y >= 0 & y <= 2 * x & y <= 6 - 3 * x
}

# Determinarea zonei rectangulare care include toate punctele interioare ale triunghiului T
a <- 0
b <- 2  # Alegem 2 pentru că y = 2x este limita superioară a triunghiului
c <- 0
d <- 6  # Alegem 6 pentru că y = 6 - 3x este limita superioară a triunghiului

# Functie pentru a estima aria triunghiului T folosind metoda Monte Carlo
monte_carlo_triangle_area <- function(a, b, c, d, samples) {
  # Initializam contorul pentru punctele din interiorul triunghiului
  points_inside_triangle <- 0
  
  # Generam punctele aleatoare
  for (i in 1:samples) {
    # Generam coordonatele aleatoare
    x <- runif(1, a, b)
    y <- runif(1, c, d)
    
    # Verificam daca punctul se afla in interiorul triunghiului
    if (is_inside_triangle(x, y)) {
      points_inside_triangle <- points_inside_triangle + 1
    }
  }
  
  # Calculam aria triunghiului
  rectangle_area <- (b - a) * (d - c)
  triangle_area <- (points_inside_triangle / samples) * rectangle_area
  
  return(triangle_area)
}

# Estimare pentru dimensiunea esantionului de cel putin 20000
samples <- 20000

estimated_area <- monte_carlo_triangle_area(a, b, c, d, samples)
cat("Aria estimată a triunghiului T este:", estimated_area, "\n")

#Ex B3
#a)
# Definim functia integrand
integrand <- function(x) {
  (2*x - 1) / (x^2 - x - 6)
}

# Calculam integrala definita
result_a <- integrate(integrand, lower = -1, upper = 1)

# Afișăm rezultatul
print(result_a)

#b)
# Definim functia integrand
integrand_b <- function(x) {
  (x + 4) / (abs(x - 3)^(1/3))
}

# Definim limita pentru a
a <- 10000  # Valoare mare pentru a

# Calculam integrala improprie pentru limita superioara
result_b_upper <- integrate(integrand_b, lower = 3, upper = a)

# Calculam integrala definita pentru limita inferioara
result_b_lower <- integrate(integrand_b, lower = a, upper = 11)

# Calculam rezultatul final
result_b <- result_b_upper[["value"]] + result_b_lower[["value"]]

# Afișăm rezultatul
print(result_b)


#c)
# Definim functia integrand
integrand_c <- function(x) {
  x * exp(-x^2)
}

# Calculam integrala improprie
result_c <- integrate(integrand_c, lower = 0, upper = Inf)

# Afișăm rezultatul
print(result_c)


#Comparam rezultatele cu valorile exacte:

# Valorile exacte date în enunț
exact_values <- c(log(3) - log(2), 61.2, 0.5)

# Valorile estimate
estimated_values <- c(result_a$value, result_b, result_c$value)

# Calculul erorilor relative
relative_errors <- abs(estimated_values - exact_values) / exact_values * 100

# Afișăm rezultatele
print("Valorile estimate:")
print(estimated_values)
print("Valorile exacte:")
print(exact_values)
print("Erorile relative (%):")
print(relative_errors)


#Ex B4
# Setăm valorile
n <- 1000
p <- 0.25
q <- 0.01
target_users <- 15000

# (a) Estimarea numărului mediu de ani necesari
mean_years <- (target_users - 10000) / (n * p)
cat("Numărul mediu de ani necesari până la 15000 de utilizatori:", mean_years, "\n")

# (b) Estimarea probabilității pentru 40 de ani și 10 luni
prob_40_years <- pbinom(target_users - 10000, size = 40*12, prob = dbinom(0, size = 1, prob = q), lower.tail = FALSE)
cat("Probabilitatea pentru 40 de ani și 10 luni:", prob_40_years, "\n")

# (c) Estimarea probabilității cu o eroare de ±0.01 și o probabilitate de 0.99
desired_error <- 0.01
desired_probability <- 0.99
estimated_probability <- 0
while (TRUE) {
  sim_users <- rbinom(10000, size = 40*12, prob = dbinom(0, size = 1, prob = q))
  estimated_probability <- mean(sim_users >= target_users - 10000)
  if (abs(estimated_probability - prob_40_years) <= desired_error) {
    break
  }
}
cat("Probabilitatea cu o eroare de ±0.01 și o probabilitate de 0.99:", estimated_probability, "\n")
