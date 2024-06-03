#Ex C1
# Funcția pentru generarea unei permutări aleatoare folosind metoda Las Vegas
generate_random_permutation <- function(n) {
  # Generăm n valori aleatoare uniforme standard
  random_values <- runif(n)
  
  # Sortăm valorile generate și reținem indicii sortați
  sorted_indices <- order(random_values)
  
  # Returnăm permutarea corespunzătoare indiciilor sortate
  return(sorted_indices)
}

# Testarea funcției cu n = 10
n <- 10
random_permutation <- generate_random_permutation(n)
cat("Permutarea aleatoare generată pentru n =", n, "este:", random_permutation, "\n")


#b)
# Functie pentru compararea lexicografica stricta a doua cuvinte
compare_lexicographic <- function(word1, word2) {
  # Determinam lungimea minima a celor doua cuvinte
  min_length <- min(length(word1), length(word2))
  
  # Parcurgem cuvintele caracter cu caracter
  for (i in 1:min_length) {
    # Daca gasim un caracter diferit
    if (word1[i] != word2[i]) {
      # Comparam caracterele si determinam care cuvant este lexicografic mai mic
      if (word1[i] < word2[i]) {
        return(TRUE)  # word1 este lexicografic mai mic decat word2
      } else {
        return(FALSE) # word2 este lexicografic mai mic decat word1
      }
    }
  }
  
  # Daca caracterele sunt egale pana la lungimea minima a cuvintelor
  # Generam aleator bit cu bit pana cand un cuvant devine lexicografic mai mic decat celalalt
  while (length(word1) < length(word2)) {
    word1 <- c(word1, sample(c(0, 1), 1)) # Adaugam un bit aleator la word1
  }
  while (length(word2) < length(word1)) {
    word2 <- c(word2, sample(c(0, 1), 1)) # Adaugam un bit aleator la word2
  }
  
  # Generam perechi de biti pana cand un cuvant devine lexicografic mai mic decat celalalt
  for (i in min_length:length(word1)) {
    if (word1[i] < word2[i]) {
      return(TRUE)  # word1 este lexicografic mai mic decat word2
    } else if (word1[i] > word2[i]) {
      return(FALSE) # word2 este lexicografic mai mic decat word1
    }
  }
  
  # Daca nu am gasit nicio diferenta, inseamna ca cuvintele sunt egale
  return(FALSE)
}

# Testam functia cu doua cuvinte aleatoare de lungimi diferite
word1 <- sample(c(0, 1), 5, replace = TRUE)
word2 <- sample(c(0, 1), 7, replace = TRUE)

cat("Cuvantul 1:", word1, "\n")
cat("Cuvantul 2:", word2, "\n")

result <- compare_lexicographic(word1, word2)
if (result) {
  cat("Cuvantul 1 este lexicografic mai mic decat cuvantul 2.\n")
} else {
  cat("Cuvantul 2 este lexicografic mai mic decat cuvantul 1.\n")
}

#Ex C1 c)
quicksort_randomized <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  } else {
    pivot <- sample(arr, 1)  # Alegerea pivotului aleatoriu
    smaller <- arr[arr < pivot]
    equal <- arr[arr == pivot]
    larger <- arr[arr > pivot]
    return(c(quicksort_randomized(smaller), equal, quicksort_randomized(larger)))
  }
}

# Exemplu de utilizare
words <- c("banana", "apple", "orange", "grape", "cherry", "kiwi")
sorted_words <- quicksort_randomized(words)
print(sorted_words)

#d)
# Funcție pentru generarea unui cuvânt aleatoriu de lungime k
generate_random_word <- function(k) {
  alphabet <- c(letters, LETTERS)  # Alfabetul
  word <- paste0(sample(alphabet, k, replace = TRUE), collapse = "")  # Generare cuvânt
  return(word)
}

# Funcție pentru generarea unei permutări aleatoare a cuvintelor
random_permutation <- function(n, k) {
  words <- replicate(n, generate_random_word(k))  # Generare n cuvinte aleatoare de lungime k
  sorted_words <- quicksort_randomized(words)  # Sortare folosind QuickSort aleator
  return(sorted_words)
}

# Exemplu de utilizare
n <- 10  # Numărul de cuvinte
k <- 5   # Lungimea fiecărui cuvânt
random_order <- random_permutation(n, k)
print(random_order)

#Ex C2
# Definirea matricei de adiacență pentru un graf bipartit simplu
adj_matrix <- matrix(0, nrow = 6, ncol = 6)  # Matrice de adiacență inițializată cu 0-uri
adj_matrix[c(1,2,3), c(4,5,6)] <- 1  # Muchii între prima jumătate de noduri și a doua jumătate

# Funcția pentru determinarea unei tăieturi de cardinal maxim într-un graf bipartit
max_bipartite_cut <- function(adj_matrix) {
  n <- nrow(adj_matrix)  # Numărul de noduri în graf
  A <- sample(1:n, n/2, replace = FALSE)  # Alegem n/2 noduri aleator din V pentru A
  B = setdiff(1:n, A)  # Restul nodurilor merg în B
  # Calculate cut_size considering only the elements within the bounds of adj_matrix
  cut_size <- sum(adj_matrix[A[A <= nrow(adj_matrix)], B[B <= ncol(adj_matrix)]])
  return(list(A = A, B = B, cut_size = cut_size))
}

# Apelarea funcției și afișarea rezultatului
rezultat <- max_bipartite_cut(adj_matrix)
print(rezultat)

# b) Pentru a creste sansele de a gasi o taietura de cardinal cat mai mare intr-un graf, 
# putem folosi metode de optimizare stocastica sau euristici care exploreaza mai multe 
# taieturi posibile. Una dintre metodele comune este Algoritmul Monte Carlo, 
# care implica rularea algoritmului de mai multe ori si selectarea celei mai bune 
# solutii gasite.
