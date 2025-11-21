library(mice)  
library(dplyr) 

# Ustawienie ziarna dla powtarzalności wyników
set.seed(123)

# Liczba iteracji
liczba_iteracji <- 1000

# Inicjalizacja listy do przechowywania wyników RMSE
lista_rmse <- numeric()

# Pętla symulująca generowanie danych, wprowadzanie braków, imputację i ocenę jakości
for (powtorzenie in 1:liczba_iteracji) {
  
  # Generowanie danych z rozkładu normalnego
  ni <- 500
  A <- 5
  macierz <- matrix(rnorm(ni * A, mean = 150, sd = 53), nrow = ni)
  
  # Wprowadzenie braków danych typu MAR
  braki <- runif(ni) > 0.5 + (macierz[, 2] / 567)
  macierz_z_brakami <- macierz
  macierz_z_brakami[which(braki), 1] <- NA
  
  # Wdrożenie metody imputacji 
  imputacja <- mice(macierz_z_brakami, method = "norm.predict", m = 2)
  dane_uzupelnione <- complete(imputacja)
  
  # Obliczenie RMSE
  RMSE <- sqrt(mean((macierz[, 1] - dane_uzupelnione[, 1])^2, na.rm = TRUE))
  
  # Dodanie wyniku RMSE do listy
  lista_rmse <- c(lista_rmse, RMSE)
}

# Zasymuluj 200 razy
lista_rmse_1000 <- lista_rmse[1:200]

# Uśredniony błąd z symulacji
sredni_blad <- mean(lista_rmse)

# Wyświetlenie wyniku
cat("Średni błąd RMSE z symulacji:", sredni_blad, "\n")

# Wizualizacja wyników RMSE
hist(lista_rmse, breaks = 30, main = "Rozkład błędów RMSE", xlab = "RMSE")
