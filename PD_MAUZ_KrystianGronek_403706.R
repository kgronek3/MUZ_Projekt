# Praca domowa z "Metod aktuarialnych w ubezpieczeniach na życie I"
# Krystian Gronek; indeks 403706

# ZADANIE 1 ####

# Kolory do wykresów

colors = c("red", "blue", "green", "orange", "purple", "deeppink", "yellow")


## de Moivre'a ####

# 1
{
title_ = "Prawdopodobieństwo przeżycia t lat w zależności od wieku (x) osoby dla omega = 100"
ylim_ = NULL
ylab_ = "Prawdopodobieństwo przeżycia t lat"
xlab_ = "Dalsze trwanie życia (t)"

x1 = 30
x2 = 50
x3 = 70
x4 = 90
omega_ = 100

deMoivre_tpx30 = function(t) {omega = omega_; x = x1; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx50 = function(t) {omega = omega_; x = x2; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx70 = function(t) {omega = omega_; x = x3; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx90 = function(t) {omega = omega_; x = x4; return( (omega - x - t) / (omega - x))}    

plot(deMoivre_tpx30, from = 0, to = omega_ - x1,
     xaxt = "n", ylim = ylim_, lwd = 2,
     type = "l", xlab = xlab_, ylab = ylab_, col = "black")
axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
     labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
title(title_)
curve(deMoivre_tpx50,from = 0, to = omega_ - x2, add = TRUE, col = colors[1], lwd = 2,)
curve(deMoivre_tpx70,from = 0, to = omega_ - x3, add = TRUE, col = colors[2], lwd = 2,)
curve(deMoivre_tpx90,from = 0, to = omega_ - x4, add = TRUE, col = colors[3], lwd = 2,)
legend(60, 1, 
       legend = c(paste0("(x) = ", x1),
                  paste0("(x) = ", x2),
                  paste0("(x) = ", x3),
                  paste0("(x) = ", x4)),
       fill = c("black",colors[1],colors[2],colors[3]))
}

# 2
{
title_ = "Prawdopodobieństwo przeżycia t lat w zależności od omega dla 60 latka"
ylim_ = NULL
ylab_ = "Prawdopodobieństwo przeżycia t lat"
xlab_ = "Dalsze trwanie życia (t)"

omega1 = 100
omega2 = 90
omega3 = 80
omega4 = 70

deMoivre_tpx60_omega1 = function(t) {omega = omega1; x = 60; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx60_omega2 = function(t) {omega = omega2; x = 60; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx60_omega3 = function(t) {omega = omega3; x = 60; return( (omega - x - t) / (omega - x))}    
deMoivre_tpx60_omega4 = function(t) {omega = omega4; x = 60; return( (omega - x - t) / (omega - x))}    

plot(deMoivre_tpx60_omega1, from = 0, to = omega1 - 60,
     xaxt = "n", ylim = ylim_, lwd = 2,
     type = "l", xlab = xlab_, ylab = ylab_, col = "black")
axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
     labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
title(title_)
curve(deMoivre_tpx60_omega2,from = 0, to = omega2 - 60, add = TRUE, col = colors[1], lwd = 2,)
curve(deMoivre_tpx60_omega3,from = 0, to = omega3 - 60, add = TRUE, col = colors[2], lwd = 2,)
curve(deMoivre_tpx60_omega4,from = 0, to = omega4 - 60, add = TRUE, col = colors[3], lwd = 2,)
legend(30, 1, 
       legend = c(paste0("omega = ", omega1),
                  paste0("omega = ", omega2),
                  paste0("omega = ", omega3),
                  paste0("omega = ", omega4)),
       fill = c("black",colors[1],colors[2],colors[3]))
}

# 

# 3
{
    title_ = "Prawdopodobieństwo przeżycia 1 roku dla różnych wartości\nomega w zależności od wieku osoby"
    ylim_ = NULL
    ylab_ = "Prawdopodobieństwo przeżycia 1 roku"
    xlab_ = "Wiek osoby (x)"
    
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    deMoivre_1px_omega1 = function(x) {omega = omega1; return( (omega - x - 1) / (omega - x))}    
    deMoivre_1px_omega2 = function(x) {omega = omega2; return( (omega - x - 1) / (omega - x))}    
    deMoivre_1px_omega3 = function(x) {omega = omega3; return( (omega - x - 1) / (omega - x))}    
    deMoivre_1px_omega4 = function(x) {omega = omega4; return( (omega - x - 1) / (omega - x))}    

    plot(deMoivre_1px_omega1, from = 0, to = 100,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(deMoivre_1px_omega2, from = 0, to = 100, add = TRUE, col = colors[1], lwd = 2,)
    curve(deMoivre_1px_omega3, from = 0, to = 100, add = TRUE, col = colors[2], lwd = 2,)
    curve(deMoivre_1px_omega4, from = 0, to = 100, add = TRUE, col = colors[3], lwd = 2,)
    legend(0, 0.3, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}



## Wykładniczy ####

# 1
{
title_ = "Prawdopodobieństwo przeżycia t lat dla różnych wartości\nmu w rozkładzie wykładnicznym"
ylim_ = c(0,1)
ylab_ = "Prawdopodobieństwo przeżycia t lat"
xlab_ = "Dalsze trwanie życia (t)"

mu1 = 0.01
mu2 = 0.02
mu3 = 0.04
mu4 = 0.06
mu5 = 0.08
mu6 = 0.10

Wykladniczy1 <- function(t) {exp(-1 * mu1 * t)} # mu = 0.01
Wykladniczy2 <- function(t) {exp(-1 * mu2 * t)} # mu = 0.02
Wykladniczy3 <- function(t) {exp(-1 * mu3 * t)} # mu = 0.04
Wykladniczy4 <- function(t) {exp(-1 * mu4 * t)} # mu = 0.06
Wykladniczy5 <- function(t) {exp(-1 * mu5 * t)} # mu = 0.08
Wykladniczy6 <- function(t) {exp(-1 * mu6 * t)} # mu = 0.10   

plot(Wykladniczy1, from = 0, to = 100,
     xaxt = "n", ylim = ylim_, lwd = 2,
     type = "l", xlab = xlab_, ylab = ylab_, col = "black")
axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
     labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
title(title_)
curve(Wykladniczy2,from = 0, to = 100, ylim = c(0,1), add = TRUE, col = colors[1], lwd = 2)
curve(Wykladniczy3,from = 0, to = 100, ylim = c(0,1), add = TRUE, col = colors[2], lwd = 2)
curve(Wykladniczy4,from = 0, to = 100, ylim = c(0,1), add = TRUE, col = colors[3], lwd = 2)
curve(Wykladniczy5,from = 0, to = 100, ylim = c(0,1), add = TRUE, col = colors[4], lwd = 2)
curve(Wykladniczy6,from = 0, to = 100, ylim = c(0,1), add = TRUE, col = colors[5], lwd = 2)
legend(80, 1, 
       legend = c(paste0("mu = ", mu1),
                  paste0("mu = ", mu2),
                  paste0("mu = ", mu3),
                  paste0("mu = ", mu4),
                  paste0("mu = ", mu5),
                  paste0("mu = ", mu6)),
       fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}

# 2
{
    title_ = "Prawdopodobieństwo przeżycia t lat w zależności od mu"
    ylim_ = c(0,1)
    ylab_ = "Prawdopodobieństwo przeżycia t lat"
    xlab_ = "Wartość współczynnika wymieralności mu"

    # dalsze trwanie życia x latka
    dalsze_trwanie_zycia1 = 10
    dalsze_trwanie_zycia2 = 20
    dalsze_trwanie_zycia3 = 30
    dalsze_trwanie_zycia4 = 40
    dalsze_trwanie_zycia5 = 50
    dalsze_trwanie_zycia6 = 60
    
    Wykladniczy1 <- function(mu) {wiek = dalsze_trwanie_zycia1; exp(-1 * mu * wiek)} 
    Wykladniczy2 <- function(mu) {wiek = dalsze_trwanie_zycia2; exp(-1 * mu * wiek)} 
    Wykladniczy3 <- function(mu) {wiek = dalsze_trwanie_zycia3; exp(-1 * mu * wiek)} 
    Wykladniczy4 <- function(mu) {wiek = dalsze_trwanie_zycia4; exp(-1 * mu * wiek)} 
    Wykladniczy5 <- function(mu) {wiek = dalsze_trwanie_zycia5; exp(-1 * mu * wiek)} 
    Wykladniczy6 <- function(mu) {wiek = dalsze_trwanie_zycia6; exp(-1 * mu * wiek)} 
    
    plot(Wykladniczy1, from = 0, to = 0.15,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), 
         labels = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
    title(title_)
    curve(Wykladniczy2,from = 0, to = 1, ylim = c(0,1), add = TRUE, col = colors[1], lwd = 2)
    curve(Wykladniczy3,from = 0, to = 1, ylim = c(0,1), add = TRUE, col = colors[2], lwd = 2)
    curve(Wykladniczy4,from = 0, to = 1, ylim = c(0,1), add = TRUE, col = colors[3], lwd = 2)
    curve(Wykladniczy5,from = 0, to = 1, ylim = c(0,1), add = TRUE, col = colors[4], lwd = 2)
    curve(Wykladniczy6,from = 0, to = 1, ylim = c(0,1), add = TRUE, col = colors[5], lwd = 2)
    legend(0.1, 1, 
           legend = c(paste0("t = ", dalsze_trwanie_zycia1),
                      paste0("t = ", dalsze_trwanie_zycia2),
                      paste0("t = ", dalsze_trwanie_zycia3),
                      paste0("t = ", dalsze_trwanie_zycia4),
                      paste0("t = ", dalsze_trwanie_zycia5),
                      paste0("t = ", dalsze_trwanie_zycia6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}



# 3 
{
title_ = "Prawdopodobieństwo przeżycia 1 roku dla różnych wartości\nomega w zależności od wieku osoby"
ylim_ = c(0.6,1)
ylab_ = "Prawdopodobieństwo przeżycia 1 roku"
xlab_ = "Wiek osoby (x)"

mu1 = 0.01
mu2 = 0.02
mu3 = 0.04
mu4 = 0.06
mu5 = 0.08
mu6 = 0.10

# to są stałe wartości !!!
Wykladniczy1 <- exp(-1 * mu1 * 1) # mu = 0.01
Wykladniczy2 <- exp(-1 * mu2 * 1) # mu = 0.02
Wykladniczy3 <- exp(-1 * mu3 * 1) # mu = 0.04
Wykladniczy4 <- exp(-1 * mu4 * 1) # mu = 0.06
Wykladniczy5 <- exp(-1 * mu5 * 1) # mu = 0.08
Wykladniczy6 <- exp(-1 * mu6 * 1) # mu = 0.10    

plot(y=rep(0,100), x = 1:100,
     xaxt = "n", ylim = ylim_, lwd = 2,
     type = "l", xlab = xlab_, ylab = ylab_, col = "white")
axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
     labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
title(title_)
abline(h = Wykladniczy1, col = "black"  , lwd = 2)
abline(h = Wykladniczy2, col = colors[1], lwd = 2)
abline(h = Wykladniczy3, col = colors[2], lwd = 2)
abline(h = Wykladniczy4, col = colors[3], lwd = 2)
abline(h = Wykladniczy5, col = colors[4], lwd = 2)
abline(h = Wykladniczy6, col = colors[5], lwd = 2)
legend(80, 0.8, 
       legend = c(paste0("mu = ", mu1),
                  paste0("mu = ", mu2),
                  paste0("mu = ", mu3),
                  paste0("mu = ", mu4),
                  paste0("mu = ", mu5),
                  paste0("mu = ", mu6)),
       fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}


# ZADANIE 2 ####
# ZADANIE 3 ####
# ZADANIE 4 ####
# ZADANIE 5 ####



