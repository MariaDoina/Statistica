# Proiect Statistica 2025

# Problema 1
# Ex 1
# a) -------------------------------------------------------------------
set.seed(123)  
n <- 10
x0 <- 2
gamma <- 1

#Folosim functia de densitate pentru a genera functia Cauchy centrata in x0,
#avand abaterea medie absoluta data de gamma
f_Cauchy <- function(x,x0, gamma){
  1/(pi*gamma) * 1/(1+((x-x0)/gamma)^2)
}

#F_Cauchy am generat-o folosind metoda inversei
F_Cauchy <- function(n, x0, gamma){
  U <- runif(n)
  gamma*tan(pi*(U - 1/2)) + x0
  
}

#X ia valorile generate de functia cuantila
x <- F_Cauchy(n, x0, gamma)

#generam histograma
hist(x, breaks = 50, probability = TRUE, main = "Distribuția Cauchy", col = "lightblue")
t <- seq(min(x),max(x),1)
#creem graficul functiei f care urmeaza repartitia Cauchy
lines(t,f_Cauchy(t, x0, gamma),col="magenta",xlim=c(-10,10))

# b) -------------------------------------------------------------------

set.seed(123)  
n <- 1000  

# Generam n valori uniforme intre 0 si 1
U <- runif(n)

# Aplicam metoda transformarii inverse
X_discret <- ifelse(U < 1/2, 1, ifelse(U < 5/6, 2, 3))

# Creare histograma pentru distributia discreta
hist(X_discret, breaks = seq(0.5, 3.5, 1),
     main = "Distributia discreta a lui X", 
     col = "lightblue", border = "black")

# c) -------------------------------------------------------------------
n <- 10
# Functie Bernoulli
bernoulli <- function(x1, x2, p, n) {
  generare_bern <- ifelse(runif(n) < p, x2, x1)
  # Creare histograma pentru distributia Bernoulli
  hist(generare_bern, breaks = seq(min(x1, x2) - 0.5, max(x1, x2) + 0.5, 1),
       probability = TRUE, main = "Distributia Bernoulli", col = "pink", 
       border = "black", ylim = c(0, 1))
  return(generare_bern)
}

# Citirea valorilor pentru Bernoulli
test_x1 <- 1
test_x2 <- 2
#p-ul ia valori intre 0 si 1
test_p <- 0.7

# Generare si vizualizare Bernoulli
bernoulli(test_x1, test_x2, test_p, n)

# d) -------------------------------------------------------------------
# a)
n <- 10
# Functie pentru generarea unui numar intreg între a si b
perturbare_aleat <- function(a, b) {
  secunda <- as.numeric(format(Sys.time(), "%S"))  # Extrage secunda curenta
  perturbare <- sin(secunda) * (b - a)/15   # vrem unif(0,1)
  # Generam un numar aleator intre a si b și aplicam perturbarea (i.e esantionul)
  val_aleat <- sample(a:b, 1)
  val_perturbata <- round(perturbare + val_aleat) 
  # ne asiguram rezultatul ramane in intervalul [a, b]
  return(max(min(val_perturbata, b), a))
}

# Functie pentru simularea numarului de calatori
simulare_pasageri <- function(xmin, xmax, y) {
  pasageri <- perturbare_aleat(xmin, xmax)  
  # Creste probabilitatea de a alege un numar apropiat de medie
  pasageri <- round((pasageri + y) / 2)  # Ajustare spre medie
  return(pasageri)
}

# Setam parametrii pentru linia 501 STB
xmin <- perturbare_aleat(150, 300)   # Numarul minim de calatori pe zi
xmax <- perturbare_aleat(700, 1000)   # Numarul maxim de calatori pe zi
y <- perturbare_aleat(300, 700)      # Valoarea medie aproximativa

# Generam numarul de calatori pentru fiecare zi din decembrie 2024
decembrie_2024 <- sapply(1:31, function(x) simulare_pasageri(xmin, xmax, y))

# Cream histograma folosind hist()
hist(decembrie_2024, breaks = 10, col = "purple", border = "black", main = "Distributia numarului de calatori - Decembrie 2024",xlab = "Numar de calatori pe zi",ylab = "Frecventa")

# b) -------------------------------------------------------------------

# Functie pentru simularea fiecarei luni din anul 2024
simuleaza_luna <- function(luna, zile) {
  min_calatori <- perturbare_aleat(150, 300)   # Numarul minim de calatori pe zi
  max_calatori <- perturbare_aleat(700, 1000)  # Numarul maxim de calatori pe zi
  medie_calatori <- perturbare_aleat(300, 700) # Valoarea medie aproximativa
  calatori_zi <- numeric(zile)  # Initializam vectorul
  for (i in 1:zile) {
    calatori_zi[i] <- simulare_pasageri(min_calatori, max_calatori, medie_calatori)  # Generam datele zilnice
  }
  zile_usoare <- sum(calatori_zi < 350)
  zile_normale <- sum(calatori_zi >= 351 & calatori_zi <= 670)
  zile_aglomerate <- sum(calatori_zi > 671)
  return(data.frame(
    Luna = luna,
    Medie_Calatori = mean(calatori_zi),
    Min_Calatori = min(calatori_zi),
    Max_Calatori = max(calatori_zi),
    Proc_Zile_Usoare = zile_usoare / zile * 100,
    Proc_Zile_Normale = zile_normale / zile * 100,
    Proc_Zile_Aglomerate = zile_aglomerate / zile * 100
  ))
}

# Definim numarul de zile pentru fiecare luna
luni_2024 <- list(
  "Ianuarie" = 31, "Februarie" = 29, "Martie" = 31, "Aprilie" = 30,
  "Mai" = 31, "Iunie" = 30, "Iulie" = 31, "August" = 31,
  "Septembrie" = 30, "Octombrie" = 31, "Noiembrie" = 30, "Decembrie" = 31
)

# Simulam datele pentru fiecare luna
rezultate_2024 <- data.frame()  # Initializam un dataframe gol
for (luna in names(luni_2024)) {
  rezultate_2024 <- rbind(rezultate_2024, simuleaza_luna(luna, luni_2024[[luna]]))
}

# Afisam rezultatele
print(rezultate_2024)

# c)--------------------------------------------------------
# Proiect Statistica 2025

# Problema 1
# Ex 1
# a) -------------------------------------------------------------------
set.seed(123)  
n <- 10
x0 <- 2
gamma <- 1

#Folosim functia de densitate pentru a genera functia Cauchy centrata in x0,
#avand abaterea medie absoluta data de gamma
f_Cauchy <- function(x,x0, gamma){
  1/(pi*gamma) * 1/(1+((x-x0)/gamma)^2)
}

#F_Cauchy am generat-o folosind metoda inversei
F_Cauchy <- function(n, x0, gamma){
  U <- runif(n)
  gamma*tan(pi*(U - 1/2)) + x0
  
}

#X ia valorile generate de functia cuantila
x <- F_Cauchy(n, x0, gamma)

#generam histograma
hist(x, breaks = 50, probability = TRUE, main = "Distribuția Cauchy", col = "lightblue")
t <- seq(min(x),max(x),1)
#creem graficul functiei f care urmeaza repartitia Cauchy
lines(t,f_Cauchy(t, x0, gamma),col="magenta",xlim=c(-10,10))

# b) -------------------------------------------------------------------

set.seed(123)  
n <- 1000  

# Generam n valori uniforme intre 0 si 1
U <- runif(n)

# Aplicam metoda transformarii inverse
X_discret <- ifelse(U < 1/2, 1, ifelse(U < 5/6, 2, 3))

# Creare histograma pentru distributia discreta
hist(X_discret, breaks = seq(0.5, 3.5, 1),
     main = "Distributia discreta a lui X", 
     col = "lightblue", border = "black")

# c) -------------------------------------------------------------------
n <- 10
# Functie Bernoulli
bernoulli <- function(x1, x2, p, n) {
  generare_bern <- ifelse(runif(n) < p, x2, x1)
  # Creare histograma pentru distributia Bernoulli
  hist(generare_bern, breaks = seq(min(x1, x2) - 0.5, max(x1, x2) + 0.5, 1),
       probability = TRUE, main = "Distributia Bernoulli", col = "pink", 
       border = "black", ylim = c(0, 1))
  return(generare_bern)
}

# Citirea valorilor pentru Bernoulli
test_x1 <- 1
test_x2 <- 2
#p-ul ia valori intre 0 si 1
test_p <- 0.7

# Generare si vizualizare Bernoulli
bernoulli(test_x1, test_x2, test_p, n)

# d) -------------------------------------------------------------------

n <- 10
# Functie pentru generarea unui numar intreg intre a si b
perturbare_aleat <- function(a, b) {
  secunda <- as.numeric(format(Sys.time(), "%S"))  # Extrage secunda curenta
  perturbare <- sin(secunda) * (b - a)/15   # vrem unif(0,1)
  # Generam un numar aleator intre a si b si aplicam perturbarea (i.e esantionul)
  val_aleat <- sample(a:b, 1)
  val_perturbata <- round(perturbare + val_aleat) 
  # ne asiguram rezultatul ramane in intervalul [a, b]
  return(max(min(val_perturbata, b), a))
}

# Functie pentru simularea numarului de calatori
simulare_pasageri <- function(xmin, xmax, y) {
  pasageri <- perturbare_aleat(xmin, xmax)  
  # Creste probabilitatea de a alege un numar apropiat de medie
  pasageri <- round((pasageri + y) / 2)  # Ajustare spre medie
  return(pasageri)
}

# Setam parametrii pentru linia 501 STB
xmin <- perturbare_aleat(150, 300)   # Numarul minim de calatori pe zi
xmax <- perturbare_aleat(700, 1000)   # Numarul maxim de calatori pe zi
y <- perturbare_aleat(300, 700)      # Valoarea medie aproximativa

# Generam numarul de calatori pentru fiecare zi din decembrie 2024
decembrie_2024 <- sapply(1:31, function(x) simulare_pasageri(xmin, xmax, y))

# Cream histograma folosind hist()
hist(decembrie_2024, breaks = 10, col = "purple", border = "black", main = "Distributia numarului de calatori - Decembrie 2024",xlab = "Numar de calatori pe zi",ylab = "Frecventa")

# b) -------------------------------------------------------------------

# Functie pentru simularea fiecarei luni din anul 2024
simuleaza_luna <- function(luna, zile) {
  min_calatori <- perturbare_aleat(150, 300)   
  max_calatori <- perturbare_aleat(700, 1000)  
  medie_calatori <- perturbare_aleat(300, 700) # Valoarea medie aproximativa
  calatori_zi <- numeric(zile)  # Initializam vectorul
  for (i in 1:zile) {
    calatori_zi[i] <- simulare_pasageri(min_calatori, max_calatori, medie_calatori)  # Generam datele zilnice
  }
  zile_usoare <- sum(calatori_zi < 350)
  zile_normale <- sum(calatori_zi >= 351 & calatori_zi <= 670)
  zile_aglomerate <- sum(calatori_zi > 671)
  return(data.frame(
    Luna = luna,
    Medie_Calatori = mean(calatori_zi),
    Min_Calatori = min(calatori_zi),
    Max_Calatori = max(calatori_zi),
    Proc_Zile_Usoare = zile_usoare / zile * 100,
    Proc_Zile_Normale = zile_normale / zile * 100,
    Proc_Zile_Aglomerate = zile_aglomerate / zile * 100))}
# Definim numarul de zile pentru fiecare luna
luni_2024 <- list(
  "Ianuarie" = 31, "Februarie" = 29, "Martie" = 31, "Aprilie" = 30,
  "Mai" = 31, "Iunie" = 30, "Iulie" = 31, "August" = 31,
  "Septembrie" = 30, "Octombrie" = 31, "Noiembrie" = 30, "Decembrie" = 31
)
# Simulam datele pentru fiecare luna
rezultate_2024 <- data.frame()  # Initializam un dataframe gol
for (luna in names(luni_2024)) {
  rezultate_2024 <- rbind(rezultate_2024, simuleaza_luna(luna, luni_2024[[luna]]))
}
# Afisam rezultatele
print(rezultate_2024)

# c)--------------------------------------------------------
# Adaugam simularea pentru pasagerii cu abonament, cei care platesc bilet si cei care nu platesc
simuleaza_pasageri_detaliat <- function(luna, zile) {
  min_calatori <- perturbare_aleat(150, 300)
  max_calatori <- perturbare_aleat(700, 1000)
  medie_calatori <- perturbare_aleat(300, 700)
  pret_bilet <- 3  # Preț bilet
  pret_abonament <- 70 # Pret abonament
  
  total_calatori <- numeric(zile)
  pasageri_abonament <- numeric(zile)
  pasageri_bilet <- numeric(zile)
  pasageri_fara_plata <- numeric(zile)
  venit_bilete <- numeric(zile)
  venit_abonamente <- numeric(zile)
  pierderi_neplata <- numeric(zile)
  
  for (i in 1:zile) {
    total_calatori[i] <- simulare_pasageri(min_calatori, max_calatori, medie_calatori)
    
    # Procent pasageri cu abonament intre 1% si 99%
    x_abonament <- perturbare_aleat(1, 99) / 100 
    pasageri_abonament[i] <- round(total_calatori[i] * x_abonament)
    
    # Pasagerii ramasi
    ramasi <- total_calatori[i] - pasageri_abonament[i]
    
    # Procent pasageri care platesc bilet intre 1% si 99%
    x_bilet <- perturbare_aleat(1, 99) / 100 
    pasageri_bilet[i] <- round(ramasi * x_bilet)
    pasageri_fara_plata[i] <- ramasi - pasageri_bilet[i]
    
    # Calcul venituri
    venit_bilete[i] <- pasageri_bilet[i] * pret_bilet
    venit_abonamente[i] <- pasageri_abonament[i] * pret_abonament
    pierderi_neplata[i] <- pasageri_fara_plata[i] * pret_bilet
  }
  
  zile_usoare <- sum(total_calatori < 350)
  zile_normale <- sum(total_calatori >= 351 & total_calatori <= 670)
  zile_aglomerate <- sum(total_calatori > 671)
  
  return(data.frame(
    Luna = luna,
    Medie_Calatori = mean(total_calatori),
    Min_Calatori = min(total_calatori),
    Max_Calatori = max(total_calatori),
    Proc_Zile_Usoare = zile_usoare / zile * 100,
    Proc_Zile_Normale = zile_normale / zile * 100,
    Proc_Zile_Aglomerate = zile_aglomerate / zile * 100,
    Pasageri_Abonamente = mean(pasageri_abonament), # facem media pt ca vrem pe luna
    Pasageri_Bilete = mean(pasageri_bilet),
    Pasageri_Fara_Plata = mean(pasageri_fara_plata),
    Venit_Bilete = sum(venit_bilete),
    Venit_Abonamente = sum(venit_abonamente),
    Pierderi_Neplata = sum(pierderi_neplata)))
  }

# Simulam pentru fiecare luna
rezultate_2024_detaliat <- data.frame()
for (luna in names(luni_2024)) {
  rezultate_2024_detaliat <- rbind(rezultate_2024_detaliat, simuleaza_pasageri_detaliat(luna, luni_2024[[luna]]))}

# Afisam rezultatele
print(rezultate_2024_detaliat)


#Problema 3
# Exercitiul 3
#a)-----------------------------------------------------

#Binomiala

p_binomial <- function(n, p, x) {
  miu <- n * p #se calculeaza media 
  sigma <- sqrt(n * p * (1 - p)) #se calc deviatia standard
  
  z <- (x - miu) / sigma #vrem sa aflam z_n prin standardizare
  # Calculam P(Z_n <= x) folosind distributia normala standard
  print(paste("Media sumei:", miu))
  print(paste("Deviatia standard a sumei:", sigma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
}

n <- 1000  # nr de incercari
p <- 0.3   # probabilitatea de succes
x <- 20    # valoarea pentru care calculam probabilitatea

#Calculam P(Z_n<=x)
prob <- p_binomial(n,p,x)
print(prob)

# 2)
#Binomiala
# Cream un vector de valori x pentru care calculam probabilitatea
x_vals <- seq(0, 500, by = 1)  # De la 0 la 500 cu pasul 1
y_vals <- sapply(x_vals, function(x) p_binomial(n, p, x))  

# Generam graficul
plot(x_vals, y_vals, type = "s", col = "blue", lwd = 2, 
     main = "Distributia Binomiala",
     xlab = "x", ylab = "P(Z_n <= x)")


#----------------------------------------------------------------
p_geometrica <- function(n, p, x) {
  # Calculam media si deviataia standard
  miu <- 1/ p
  sigma <- sqrt( (1 - p) / p^2)
  miu_sum <- n * miu
  sigma_sum <- sqrt(n * sigma^2)
  z <- (x - miu_sum) / sigma_sum # Transformam x in variabila standardizata Z_n
  print(paste("Media sumei:", miu))
  print(paste("Deviatia standard a sumei:", sigma))
  print(paste("Valoarea lui z:", z))
  return(pnorm(z))
}

n <- 10  # Numarul de variabile aleatoare geometrice
p <- 0.2   # Probabilitatea de succes
x <- 5   # Valoarea pentru care calculam probabilitatea


prob <- p_geometrica(n, p, x)
print(prob)

#Graficul Geom.
x_values <- seq(1, 100, by = 10) 
prob_values <- sapply(x_values, function(x) p_geometrica(n, p, x))

# Reprezentam grafic functia
plot(x_values, prob_values,  type = "s",col = "blue", lwd = 2, 
     xlab = "x", ylab = "P(Z_n <= x)", main = "Distributia geometrica")

#--------------------------------------------------------------------------------
#Poisson
p_poisson <- function(n, lambda,x) {
  # Calculam media si deviataia standard
  miu <- n*lambda
  sigma <- sqrt(n*lambda)
  z <- (x - miu) / sigma # Transformam x in variabila standardizata Z_n
  print(paste("Media sumei:", miu))
  print(paste("Deviatia standard a sumei:", sigma))
  print(paste("Valoarea lui z:", z))
  
  return(pnorm(z))
}
n <- 100  
lambda <- 5 
prob <- p_poisson(n,lambda,x)
print(prob)
# Generam valori pentru x
x_values <- seq(411, 589, by = 1) # avem nevoie de intervalul [miu+4*tetha;miu-4*tetha]
prob_values <- sapply(x_values, function(x) p_poisson(n, lambda, x))

# Reprezentam grafic functia
plot(x_values, prob_values, type = "s", col = "blue", lwd = 2, 
     xlab = "x", ylab = "P(Z_n <= x)", main = "Functia de repartitie Poisson")

#-------------------------------------------------------------------------------
#Uniforma de caz discret

p_uniforma_discreta <- function(a, b, n, x) {
  miu_suma <- n * (a + b) / 2
  sigma_suma <- sqrt(n * ((b - a + 1)^2 - 1) / 12)
  z <- (x - miu_suma) / sigma_suma
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
}

a <- 1    # limita inferioara a intervalului
b <- 6    # limita superioara a intervalului
n <- 50 # nr de variabile aleatoare
x <- 350  # Valoarea pentru care calculam probabilitatea

# Calculam P(Z_n ≤ x)
prob <- p_uniforma_discreta(a, b, n, x)
print(prob)

# 2) -------------------------------------------------------
#Graficul uniforma caz discret
x_values <- seq(-100, 400, by = 10) 
prob_values <- sapply(x_values, function(x) p_uniforma_discreta(a, b, n, x))

# Reprezentam grafic functia
title <- "Distributia Uniforma Discreta"
plot(x_values, prob_values, type = "s", col = "red", lwd = 2, 
     xlab = "x", ylab = "P(Z_n <= x)", main = title)


#------------------------------------------------------------------------------

#Uniforma de caz continuu

p_uniforma_continuu <- function(a, b, n, x) {
  miu_suma <- n * (a + b) / 2
  sigma_suma <- sqrt(n * ((b - a )^2  / 12))
  z <- (x - miu_suma) / sigma_suma
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
}


a <- 1    # limita inferioara a intervalului
b <- 6    # limita superioara a intervalului
n <- 100  # nr de variabile aleatoare
x <- 350  # Valoarea pentru care calculam probabilitatea

# Calculam P(Z_n ≤ x)
prob <- p_uniforma_continuu(a, b, n, x)
print(prob)

#---------------------------------------------------------------------------

# Exponentiala

p_exponentiala <- function(lambda, n, x) {
  miu_suma <- n / lambda
  sigma_suma <- sqrt(n) / lambda
  z <- (x - miu_suma) / sigma_suma
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
}


lambda <- 1    # Rata evenimentelor
n <- 100       # Numărul de variabile aleatoare
x <- 120       # Valoarea pentru care calculăm probabilitatea

# Calculam P(Z_n ≤ x)
prob <- p_exponentiala(lambda, n, x)
print(prob)

# 2) -----------------------------------------------------------
# Generam valori pentru x
x_values_exp <- seq(90, 150, by = 5) 
prob_values_exp <- sapply(x_values_exp, function(x) p_exponentiala(lambda, n, x))

# Reprezentam grafic functia exponentiala
title_exp <- "Distributia Exponentiala"
plot(x_values_exp, prob_values_exp, type = "l", col = "blue", lwd = 2, 
     xlab = "x", ylab = "P(Z_n <= x)", main = title_exp)


#--------------------------------------------------------------------------

#Gamma

p_gamma <- function(k, theta, n, x) {
  miu_suma <- n * k * theta
  sigma_suma <- sqrt(n * k) * theta
  z <- (x - miu_suma) / sigma_suma
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
}


k <- 2        # Parametrul de forma
theta <- 30    # Parametrul de scala
n <- 50   # Numarul de variabile aleatoare
x <- 30   

# Calculam P(Z_n ≤ x)
prob <- p_gamma(k, theta, n, x)
print(prob)

# 2) -------------------------------------------------------
# Generam valori pentru x
x_values_gamma <- seq(0, 100, by = 0.1) 
prob_values_gamma <- sapply(x_values_gamma, function(x) p_gamma(k, theta, n, x))

# Reprezentam grafic functia gamma
title_gamma <- "Distributia Gamma"
plot(x_values_gamma, prob_values_gamma, type = "l", col = "green", lwd = 2, 
     xlab = "x", ylab = "P(Z_n <= x)", main = title_gamma)

#---------------------------------------------------------
#Beta

p_beta <- function(alpha, beta, n, x) {
  miu_suma <- n * (alpha / (alpha + beta))
  sigma_suma <- sqrt(n * (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  z <- (x - miu_suma) / sigma_suma
  
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  pnorm(z)
  
}
alpha <- 2
beta <- 30
n <- 50
x <- 3.5  

prob <- p_beta(alpha, beta, n, x)
print(prob)

# 3) -----------------------------------------------
#BINOMIALA
# Functia pentru calculul probabilitatii binomiale fara standardizare
p_binomial <- function(n, p, x) {
  miu <- n * p  # Media
  sigma <- sqrt(n * p * (1 - p))  # Deviatia standard
  
  # Calculam probabilitatea folosind distributia binomiala
  prob <- pbinom(x, n, p)
  
  print(paste("Media sumei:", miu))
  print(paste("Deviatia standard a sumei:", sigma))
  print(paste("P(X <= x):", prob))
  
  return(prob)
}

# Functia pentru supremum folosind p_binomial
sup_binomial <- function(n, p) {
  miu <- n * p
  sigma <- sqrt(n * p * (1 - p))
  
  # Definim functia care calculeaza diferenta dintre p_binomial si normala
  diff_function <- function(x) {
    abs(p_binomial(n, p, x) - pnorm((x - miu) / sigma))
  }
  
  # Calculam limitele suportului binomial
  lower_bound <- 0
  upper_bound <- n
  
  # Gasim maximul diferentei folosind optimize pe suportul binomialei
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  print(result)
  return(result$objective)
}

n <- 1000
p <- 0.3
sup_value <- sup_binomial(n, p)
print(sup_value)

#GEOMETRICA
# Functia pentru calculul probabilitatii geometrice fara standardizare
p_geometrica <- function(n, p, x) {
  # Calculam media si deviataia standard
  miu <- 1/ p
  sigma <- sqrt( (1 - p) / p^2)
  miu_sum <- n * miu
  sigma_sum <- sqrt(n * sigma^2)
  prob <- pgeom(x, p)
  print(paste("Media sumei:", miu))
  print(paste("Deviatia standard a sumei:", sigma))
  print(paste("P(X <= x):", prob))
  return(prob)
}

# Functia pentru supremum folosind p_geometrica
sup_geom <- function(n, p) {
  miu <- 1/ p
  sigma <- sqrt( (1 - p) / p^2)
  miu_sum <- n * miu
  sigma_sum <- sqrt(n * sigma^2)
  # Definim functia care calculeaza diferenta dintre p_geometrica si normala
  diff_function <- function(x) {
    abs(p_geometrica(n, p, x) - pgeom(floor((x - miu) / sigma), p))
  }
  # Calculam limitele suportului geometric
  lower_bound <- 0
  upper_bound <- n
  # Gasim maximul diferentei folosind optimize pe suportul geometric
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  print(result)
  return(result$objective)
}

n <- 1000
p <- 0.3
sup_value <- sup_geom(n, p)
print(sup_value)

#3) POISSON

lambda <- 3 
n <- 100
p <- 0.5

# Functia pentru supremum folosind p_binomial
sup_poisson <- function(n, lambda) {
  miu <- n*lambda
  sigma <- sqrt(n*lambda)
  # Definim functia care calculeaza diferenta dintre p_binomial si normala
  diff_function <- function(x) {
    abs(p_poisson(n, lambda, x) - pnorm((x - miu) / sigma))
  }
  # Calculam limitele suportului binomial
  lower_bound <- 0
  upper_bound <- n
  # Gasim maximul diferentei folosind optimize pe suportul binomialei
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  print(result)
  return(result$objective)
}

sup_value <- sup_poisson(n, lambda)
print(sup_value)

# EXPONENTIALA
# Functia pentru calculul probabilitatii exponentiale fara standardizare
p_exponentiala <- function(lambda, n, x) {
  miu_suma <- n / lambda
  sigma_suma <- sqrt(n) / lambda
  z <- (x - miu_suma) / sigma_suma
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("Valoarea lui z:", z))
  prob <- pnorm(z)
  print(paste("Probabilitatea P(Z_n <= x):", prob))
  return(prob)
}

# Functia pentru supremum folosind p_exponentiala
sup_exponentiala <- function(lambda, n) {
  miu_suma <- n / lambda
  sigma_suma <- sqrt(n) / lambda
  
  # Definim functia care calculeaza diferenta dintre p_exponentiala si normala
  diff_function <- function(x) {
    abs(p_exponentiala(lambda, n, x) - pnorm((x - miu_suma) / sigma_suma))
  }
  
  # Calculam limitele suportului exponentialei
  lower_bound <- 0
  upper_bound <- n / lambda
  
  # Gasim maximul diferentei folosind optimize pe suportul exponentialei
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  return(result$objective)
}

lambda <- 0.0000001  # Rata evenimentelor
n <- 10      # Numarul de variabile aleatoare
x <- 150        # Valoarea pentru care calculam probabilitatea

# Calculam P(Z_n ≤ x)
prob <- p_exponentiala(lambda, n, x)

# Exemplu de calcul al supremului
sup_value <- sup_exponentiala(lambda, n)
print(paste("Supremul diferentei:", sup_value))

#BETA
sup_beta <- function(alpha, beta,n) {
  miu <- n * (alpha / (alpha + beta))
  sigma <- sqrt(n * (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
  # Definim functia care calculeaza diferenta dintre p_binomial si normala
  diff_function <- function(x) {
    abs(p_beta(alpha, beta,n, x) - pnorm((x - miu) / sigma))
  }
  # Calculam limitele suportului binomial
  lower_bound <- 0
  upper_bound <- n
  # Gasim maximul diferentei folosind optimize pe suportul binomialei
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  print(result)
  return(result$objective)
}
alpha <- 2
beta <- 0.5
n <- 50
x <- 3.5  
sup_value <- sup_beta(alpha, beta,n)
print(sup_value)

#GAMMA
p_gamma <- function(k, theta, n, x) {
  miu_suma <- n * k * theta
  sigma_suma <- sqrt(n * k) * theta
  prob <- pgamma(x, k*n, theta)
  print(paste("Media sumei:", miu_suma))
  print(paste("Deviatia standard a sumei:", sigma_suma))
  print(paste("P(X <= x):", prob))
  return(prob)
}
# Functia pentru supremum folosind p_gamma
sup_gamma <- function(n, k, theta) {
  miu_suma <- n * k * theta
  sigma_suma <- sqrt(n * k) * theta
  # Definim functia care calculeaza diferenta dintre p_gamma si normala
  diff_function <- function(x) {
    abs(p_gamma(k, theta, n, x) - pnorm((x - miu_suma) / sigma_suma))
  }
  # Calculam limitele suportului gamma
  lower_bound <- 0
  upper_bound <- n
  # Gasim maximul diferentei folosind optimize pe suportul gamma
  result <- optimize(diff_function, c(lower_bound, upper_bound), maximum = TRUE)
  print(result)
  return(result$objective)
}

k <- 2        # Parametrul de forma
theta <- 30    # Parametrul de scala
n <- 50   # Numarul de variabile aleatoare
x <- 30   
sup_value <- sup_gamma(n, k, theta)
print(sup_value)

# 4)
calc <- function(nume, param) {
  if (nume == "binomial") {
    n <- param$n
    p <- param$p
    medie <- n * p
    varianta <- n * p * (1 - p)
  } else if (nume == "uniform") {
    a <- param$a
    b <- param$b
    medie <- (a + b) / 2
    varianta <- (b - a)^2 / 12
  } else if (nume == "gamma") {
    a <- param$a
    b <- param$b
    medie <- a * b
    varianta <- a * b^2
  } else if (nume == "beta") {
    alpha <- param$alpha
    beta <- param$beta
    medie <- alpha / (alpha + beta)
    varianta <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  } else if (nume == "poisson") {
    lambda <- param$lambda
    medie <- lambda
    varianta <- lambda
  } else if (nume == "geometric") {
    p <- param$p
    medie <- 1 / p
    varianta <- (1 - p) / p^2
  } else if (nume == "exponential") {
    lambda <- param$lambda
    medie <- 1 / lambda
    varianta <- 1 / lambda^2
  } else {
    stop("Distributie necunoscuta")
  }
  return(list(medie = medie, varianta = varianta))
}
 
# Exemplu de utilizare:
print("Binomiala")
param <- list(n = 10, p = 0.5) # Parametri pentru o distributie binomiala
calc("binomial", param)
 
print("Beta")
param <- list(alpha=2, beta= 2)
calc("beta",param)
# Exemplu de utilizare:
param <- list(n = 10, p = 0.5) # Parametri pentru o distributie binomiala
calc("binomial", param)

param <- list(alpha=2, beta= 2)
calc("beta",param)

# 5) ------------------------------------------------------------------------- 
calculeaza_E_abs_X_miu <- function(f_dens_masa, miu, lower = -Inf, upper = Inf, discreta= FALSE) {
  if (discreta) {
    val <- lower:upper  # x1,x2...,xn
    E_abs_X_miu <- sum(f_dens_masa(val) * (abs(val - miu))^3)
  } else {
    E_abs_X_miu <- integrate(function(x) f_dens_masa(x) * (abs(x - miu))^3, lower, upper)$value
  }
  return(E_abs_X_miu)
}

#pt Normala(0,1)
f_dens_normal <- function(x) dnorm(x, mean = 0, sd = 1)
miu_normal <- 0

# Calculam E|X_1 - mu|^3 pentru o distributie normala standard
calculeaza_E_abs_X_miu(f_dens_normal, miu_normal, lower = -Inf, upper = Inf, discreta = FALSE)

# ----------------------------------------------------------------------------------------------------------------
#Problema 2
"
Plotam graficele densitatilor:
"

#GRAFIC BINOMIALA- DISCRETA
div_space_bin = seq(0,5,1)
dens_bin = function(x) (dbinom(x,5,1))
plot(div_space_bin,sapply(div_space_bin,dens_bin))


#GRAFIC GEOMETRICA- DISCRETA
div_space_geom = seq(0,30,1)
dens_geom = function (x) (dgeom(x,1/2))
plot(div_space_geom,sapply(div_space_geom,dens_geom))

#GRAFIC POISSON-DISCRETA
div_space_poiss = seq(0,30,1)
dens_poiss = function(x) (dpois(x,2))
plot(div_space_poiss,sapply(div_space_poiss,dens_poiss))

#GRAFIC CHI-PATRAT-UNIFORMA
div_space_chi = seq(0,20,0.05)
dens_chi2 = function (x,nu) (dchisq(x,nu))
plot(div_space_chi,sapply(div_space_chi,function(x) dchisq(x,5)),col = 'blue',ylim = c(0,0.2),main = 'Densitatea Chi2')
lines(div_space_chi,sapply(div_space_chi,function(x) dchisq(x,10)),col = 'red')
lines(div_space_chi,sapply(div_space_chi,function(x) dchisq(x,3)),col = 'green')
legend('topright',legend = c('chi^2(5)','Chi2(10)','chi2(3)'),col = c('blue','red','green'),lty = 1)

#GRAFIC GAMMA-UNIFORMA
div_space_gamma = seq(0,3,0.01)
dens_gamma = function (x,alpha,lambda) (dgamma(x,alpha,shape = 1/lambda))
plot(div_space_gamma,sapply(div_space_gamma,function(x) dgamma(x,2,2)),col = 'blue',main = 'Densitatea Gamma')
lines(div_space_gamma,sapply(div_space_gamma,function(x) dgamma(x,3,2)),col = 'red')
lines(div_space_gamma,sapply(div_space_gamma,function(x)dgamma(x,2,3)),col = 'green')
legend('topright',legend = c('Gamma(2,2)','Gamma(3,2)','Gamma(2,3)'),col = c('blue','red','green'),lty = 1)

#GRAFIC BETA-UNIFORMA
div_space_beta = seq(0,1,0.01)
dens_beta = function (x,alpha,beta) (dbeta(x,alpha,beta))
plot(div_space_beta,sapply(div_space_beta,function(x) dbeta(x,5,5)),col = 'blue',main = 'Densitatea Beta')
lines(div_space_beta,sapply(div_space_beta,function(x) dbeta(x,6,4)),col = 'red')
lines(div_space_beta,sapply(div_space_beta,function(x) dbeta(x,4,6)),col = 'green')
lines(div_space_beta,sapply(div_space_beta,function(x) dbeta(x,3,7)),col = 'yellow')
legend('topright',legend = c('Beta(5,5)','Beta(6,4)','Beta(4,6)','Beta(3,7)'),col = c('blue','red','green','yellow'),lty = 1)


#4)-5)
#________________________________________________________________________________
#Binomiala
comb = function (n,k)
{
  return (factorial(n)/(factorial(k)*factorial(n-k)))
}

log_ver_bin = function (x,n,p) 
{
  return (sum(sapply(x,function (k) log(comb(n,k))))+log(p/(1-p))*sum(x)+n*length(x)*log(1-p))
}
max_ver_bin = function(x,n)
{
  "Returneaza estimatia de verosimilitate maxima pentru repartitia chi2"
  log_ver_bin = function (p) 
  {sum(sapply(x,function (k) log(comb(n,k))))+log(p/(1-p))*sum(x)+n*length(x)*log(1-p)}
  res_bin = optimize(function (x)-log_ver_bin(x),c(0,1), maximum = TRUE)
  return (res_bin)
}

res_bin1 = max_ver_bin(x,5)

print(res_bin1)

n = 10   # Numarul de incercari
p = 0.5  # Probabilitatea de succes
esantion = rbinom(100,n, p)  # 100 valori dintr-o Binomiala(n, p)
valori_p = seq(0.01, 0.99, by = 0.01)  
# Calculam log-verosimilitatea pentru fiecare valoare a lui p
valori_log_ver = sapply(valori_p, function(p) log_ver_bin(esantion, n, p))
# Construim graficul
plot(valori_p, valori_log_ver, type = "l", col = "blue", lwd = 2,
     xlab = "p (probabilitatea de succes)", 
     ylab = "Log-verosimilitate", 
     main = "Funcția de log-verosimilitate pentru distribuția Binomiala")


#_____________________________________________________________________________________
# Geometrica
log_ver_geom = function(x,theta)
{
  return ((sum(x)-length(x))*log(1-p)+length(x)*log(p)) 
}
max_ver_geom = function(x)
{
  "Returneaza estimatia p(sau theta) care maximizeaza verosimilitatea"
  log_ver_geom = function (p) {(sum(x)-length(x))*log(1-p)+length(x)*log(p)}
  return (optimize(function (x) -log_ver_geom(x),c(0,1), maximum = TRUE))
}

res_geom1 = max_ver_geom(x)

print(res_geom1)


# Generam un esantion de 100 de valori dintr-o distributie geometrica cu p = 0.4
esantion_geom <- rgeom(100, prob = 0.4)

# Functia de log-verosimilitate pentru distributia geometrica
log_ver_geom <- function(x, p) {
  return ((sum(x) - length(x)) * log(1 - p) + length(x) * log(p))
}

# Cream o secventa de valori pentru p (probabilitatea de succes)
valori_p <- seq(0.01, 0.99, by = 0.01)

# Calculam log-verosimilitatea pentru fiecare valoare a lui p
valori_log_ver_geom <- sapply(valori_p, function(p) log_ver_geom(esantion_geom, p))

# Construim graficul
plot(valori_p, valori_log_ver_geom, type = "l", col = "blue", lwd = 2,
     xlab = "p (probabilitatea de succes)", 
     ylab = "Log-verosimilitate", 
     main = "Functia de log-verosimilitate pentru distributia geometrica")

#______________________________________________________________________________________

#Poisson

log_ver_poiss = function(x,lbd)
{
  n = length(x)
  return (-n*lbd+log(lbd)*sum(x))
}
max_ver_poiss = function(x,xmax)
{
  n = length(x)
  log_ver = function(lbd) -n*lbd+log(lbd)*sum(x)
  res = optimize(function (lbd) -log_ver(lbd),c(0,xmax), maximum = TRUE)
  return (res)
}

res_poiss1 = max_ver_poiss(x,10)

print(res_poiss1)

#Grafic
esantion = rpois(100, lambda = 4)  # Generam 100 de valori dintr-o distributie Poisson(4)

#  (evitam 0, deoarece log(0) nu este definit)
valori_lambda = seq(0.1, 10, by = 0.1)  

# Calculam log-verosimilitatea pentru fiecare valoare a lui lambda
valori_log_ver = sapply(valori_lambda, function(lambda) log_ver_poiss(esantion, lambda))

# Construim graficul
plot(valori_lambda, valori_log_ver, type = "l", col = "blue", lwd = 2,
     xlab = "Lambda (intensitatea evenimentelor)", 
     ylab = "Log-verosimilitate", 
     main = "Functia de log-verosimilitate pentru distributia Poisson")






