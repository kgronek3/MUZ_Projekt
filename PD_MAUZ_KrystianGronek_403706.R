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

##  de Moivre'a ####

### Różne omega w zależności od (n) ####

{
    title_ = "Wartość JSN w ubezpieczeniu na życie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 20
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    JSN_ż_omega1 = function(n) {i = i_; x = x_; omega = omega1; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_omega2 = function(n) {i = i_; x = x_; omega = omega2; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_omega3 = function(n) {i = i_; x = x_; omega = omega3; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_omega4 = function(n) {i = i_; x = x_; omega = omega4; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    
    plot(JSN_ż_omega1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_ż_omega2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_ż_omega3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_ż_omega4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 20
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    
    JSN_dż_omega1 = function(n) {i = i_; x = x_; omega = omega1; (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_omega2 = function(n) {i = i_; x = x_; omega = omega2; (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_omega3 = function(n) {i = i_; x = x_; omega = omega3; (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_omega4 = function(n) {i = i_; x = x_; omega = omega4; (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}

    plot(JSN_dż_omega1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_dż_omega2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_dż_omega3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_dż_omega4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}



{
    title_ = "Wartość JSN w ubezpieczeniu na dożycie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre"
    ylim_ = c(0.4,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 20
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    JSN_żdż_omega1 = function(n) {i = i_; x = x_; omega = omega1; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega2 = function(n) {i = i_; x = x_; omega = omega2; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega3 = function(n) {i = i_; x = x_; omega = omega3; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega4 = function(n) {i = i_; x = x_; omega = omega4; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    
    plot(JSN_żdż_omega1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_żdż_omega2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_żdż_omega3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_żdż_omega4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

### Różna stopa procentowa (i) w zależności od (n) ####

{
    title_ = "Wartość JSN w ubezpieczeniu na życie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre z omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    omega_ = 100
    x_ = 60
    n_ = 20
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_ż_i1 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_i2 = function(n) {i = i2; x = x_; omega = omega_; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_i3 = function(n) {i = i3; x = x_; omega = omega_; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    JSN_ż_i4 = function(n) {i = i4; x = x_; omega = omega_; (1 - exp(-log(1 + i)  * n)) / (log(1 + i)  * (omega - x))}
    
    plot(JSN_ż_i1, from = 0, to = omega_ - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_ż_i2,from = 0, to = omega_ - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_ż_i3,from = 0, to = omega_ - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_ż_i4,from = 0, to = omega_ - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre z omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    omega_ = 100
    x_ = 60
    n_ = 20
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_dż_i1 = function(n) {i = i1; x = x_; omega = omega_;  (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_i2 = function(n) {i = i2; x = x_; omega = omega_;  (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_i3 = function(n) {i = i3; x = x_; omega = omega_;  (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    JSN_dż_i4 = function(n) {i = i4; x = x_; omega = omega_;  (exp(-log(1 + i)  * n) * (omega - x - n)) / (omega - x)}
    
    plot(JSN_dż_i1, from = 0, to = omega_ - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_dż_i2,from = 0, to = omega_ - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_dż_i3,from = 0, to = omega_ - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_dż_i4,from = 0, to = omega_ - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na życie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre z omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    omega_ = 100
    x_ = 60
    n_ = 20
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_żdż_i1 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i2 = function(n) {i = i2; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i3 = function(n) {i = i3; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i4 = function(n) {i = i4; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    
    plot(JSN_żdż_i1, from = 0, to = omega_ - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_żdż_i2,from = 0, to = omega_ - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_żdż_i3,from = 0, to = omega_ - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_żdż_i4,from = 0, to = omega_ - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}



## Wykładniczy ####


### Różne mu w zależności od (n) ####

{
    title_ = "Wartość JSN w ubezpieczeniu na życie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    JSN_ż_mu1 = function(n) {i = i_; x = x_; mu = mu1; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_mu2 = function(n) {i = i_; x = x_; mu = mu2; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_mu3 = function(n) {i = i_; x = x_; mu = mu3; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_mu4 = function(n) {i = i_; x = x_; mu = mu4; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_mu5 = function(n) {i = i_; x = x_; mu = mu5; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_mu6 = function(n) {i = i_; x = x_; mu = mu6; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}

    
    plot(JSN_ż_mu1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_ż_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_ż_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_ż_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(JSN_ż_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(JSN_ż_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    JSN_dż_mu1 = function(n) {i = i_; x = x_; mu = mu1;(exp(-(log(1 + i) + mu) * n))}
    JSN_dż_mu2 = function(n) {i = i_; x = x_; mu = mu2;(exp(-(log(1 + i) + mu) * n))}
    JSN_dż_mu3 = function(n) {i = i_; x = x_; mu = mu3;(exp(-(log(1 + i) + mu) * n))}
    JSN_dż_mu4 = function(n) {i = i_; x = x_; mu = mu4;(exp(-(log(1 + i) + mu) * n))}
    JSN_dż_mu5 = function(n) {i = i_; x = x_; mu = mu5;(exp(-(log(1 + i) + mu) * n))}
    JSN_dż_mu6 = function(n) {i = i_; x = x_; mu = mu6;(exp(-(log(1 + i) + mu) * n))}
    
    
    plot(JSN_dż_mu1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_dż_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_dż_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_dż_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(JSN_dż_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(JSN_dż_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}




{
    title_ = "Wartość JSN w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    JSN_żdż_mu1 = function(n) {i = i_; x = x_; mu = mu1;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu2 = function(n) {i = i_; x = x_; mu = mu2;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu3 = function(n) {i = i_; x = x_; mu = mu3;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu4 = function(n) {i = i_; x = x_; mu = mu4;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu5 = function(n) {i = i_; x = x_; mu = mu5;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu6 = function(n) {i = i_; x = x_; mu = mu6;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    
    
    plot(JSN_żdż_mu1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_żdż_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_żdż_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_żdż_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(JSN_żdż_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(JSN_żdż_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}

### Różna stopa procentowa (i) w zależności od (n) ####
{
    title_ = "Wartość JSN w ubezpieczeniu na życie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie wykładnicznym z mu = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu_ = 0.05
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_ż_i1 = function(n) {i = i1; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_i2 = function(n) {i = i2; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_i3 = function(n) {i = i3; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    JSN_ż_i4 = function(n) {i = i4; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu)}
    
    
    plot(JSN_ż_i1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_ż_i2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_ż_i3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_ż_i4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie wykładnicznym z mu = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu_ = 0.05
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_dż_i1 = function(n) {i = i1; x = x_; mu = mu_; (exp(-(log(1 + i) + mu) * n))}
    JSN_dż_i2 = function(n) {i = i2; x = x_; mu = mu_; (exp(-(log(1 + i) + mu) * n))}
    JSN_dż_i3 = function(n) {i = i3; x = x_; mu = mu_; (exp(-(log(1 + i) + mu) * n))}
    JSN_dż_i4 = function(n) {i = i4; x = x_; mu = mu_; (exp(-(log(1 + i) + mu) * n))}
    
    
    plot(JSN_dż_i1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_dż_i2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_dż_i3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_dż_i4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Wartość JSN w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie wykładnicznym z mu = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu_ = 0.05
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_dżd_i1 = function(n) {i = i1; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_dżd_i2 = function(n) {i = i2; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_dżd_i3 = function(n) {i = i3; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_dżd_i4 = function(n) {i = i4; x = x_; mu = mu_; ((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    
    
    plot(JSN_dżd_i1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(JSN_dżd_i2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(JSN_dżd_i3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(JSN_dżd_i4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    legend(30, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}


# ZADANIE 3 ####

## de Moivre'a ####

### Różne omega w zależności od (n) ####

{
    title_ = "Wartość obecna aktuarialnej renty terminowej n-letniej dla 60 latka w zależności\nod długości okresu składkowania (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    renta_terminowa_omega1 = function(n) {x = x_; i = i_; omega = omega1;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega2 = function(n) {x = x_; i = i_; omega = omega2;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega3 = function(n) {x = x_; i = i_; omega = omega3;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega4 = function(n) {x = x_; i = i_; omega = omega4;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    
    plot(renta_terminowa_omega1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = NULL, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(renta_terminowa_omega2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(renta_terminowa_omega3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(renta_terminowa_omega4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(0, 10, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

### Różne i w zależności od (n) ####

{
    title_ = "Wartość obecna aktuarialnej renty terminowej n-letniej dla 60 latka w zależności\nod długości okresu składkowania (n) dla różnych wartości stopy procentowej (i)\nw rozkładzie De Moivre z omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    omega_ = 100
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    renta_terminowa_i1 = function(n) {x = x_; i = i1; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i2 = function(n) {x = x_; i = i2; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i3 = function(n) {x = x_; i = i3; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i4 = function(n) {x = x_; i = i4; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    
    plot(renta_terminowa_i1, from = 0, to = omega_ - x_,
         xaxt = "n", ylim = NULL, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(renta_terminowa_i2,from = 0, to = omega_ - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(renta_terminowa_i3,from = 0, to = omega_ - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(renta_terminowa_i4,from = 0, to = omega_ - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(0, 15, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}


## Wykładniczy ####

### Różne mu w zależności od (n) ####

{
    title_ = "Wartość obecna aktuarialnej renty terminowej n-letniej dla x latka w zależności\nod długości okresu składkowania (n) dla różnych wartości mu\nw rozkładzie wykładnicznym z i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    renta_terminowa_mu1 = function(n) {i = i_; mu = mu1; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu2 = function(n) {i = i_; mu = mu2; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu3 = function(n) {i = i_; mu = mu3; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu4 = function(n) {i = i_; mu = mu4; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu5 = function(n) {i = i_; mu = mu5; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu6 = function(n) {i = i_; mu = mu6; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    
    plot(renta_terminowa_mu1, from = 0, to = n_,
         xaxt = "n", ylim = NULL, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(renta_terminowa_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(renta_terminowa_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(renta_terminowa_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(renta_terminowa_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(renta_terminowa_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(0, 15, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}

### Różne i w zależności od (n) ####

{
    title_ = "Wartość obecna aktuarialnej renty terminowej n-letniej dla x latka w zależności\nod długości okresu składkowania (n) dla różnych wartości stopy procentowej (i)\nw rozkładzie wykładnicznym z mu = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    n_ = 40
    mu_ = 0.05
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    renta_terminowa_i1 = function(n) {i = i1; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i2 = function(n) {i = i2; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i3 = function(n) {i = i3; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i4 = function(n) {i = i4; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    
    plot(renta_terminowa_i1, from = 0, to = n_,
         xaxt = "n", ylim = NULL, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(renta_terminowa_i2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(renta_terminowa_i3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(renta_terminowa_i4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    legend(0, 15, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

# ZADANIE 4 ####

## de Moivre'a ####

### Różne omega w zależności od (n) ####

{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre dla i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 60
    omega1 = 100
    omega2 = 90
    omega3 = 80
    omega4 = 70
    
    JSN_żdż_omega1 = function(n) {i = i_; x = x_; omega = omega1; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega2 = function(n) {i = i_; x = x_; omega = omega2; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega3 = function(n) {i = i_; x = x_; omega = omega3; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_omega4 = function(n) {i = i_; x = x_; omega = omega4; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    
    renta_terminowa_omega1 = function(n) {x = x_; i = i_; omega = omega1;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega2 = function(n) {x = x_; i = i_; omega = omega2;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega3 = function(n) {x = x_; i = i_; omega = omega3;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_omega4 = function(n) {x = x_; i = i_; omega = omega4;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    
    P_żdż_omega1 = function(n) {JSN_żdż_omega1(n) / renta_terminowa_omega1(n)}
    P_żdż_omega2 = function(n) {JSN_żdż_omega2(n) / renta_terminowa_omega2(n)}
    P_żdż_omega3 = function(n) {JSN_żdż_omega3(n) / renta_terminowa_omega3(n)}
    P_żdż_omega4 = function(n) {JSN_żdż_omega4(n) / renta_terminowa_omega4(n)}
    
    plot(P_żdż_omega1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(P_żdż_omega2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(P_żdż_omega3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(P_żdż_omega4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(10, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}

{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej (i)\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    x_ = 60
    omega_ = 100
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    JSN_żdż_i1 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i2 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i3 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    JSN_żdż_i4 = function(n) {i = i1; x = x_; omega = omega_; (1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x)}
    
    renta_terminowa_i1 = function(n) {x = x_; i = i1; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i2 = function(n) {x = x_; i = i2; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i3 = function(n) {x = x_; i = i3; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    renta_terminowa_i4 = function(n) {x = x_; i = i4; omega = omega_;(1 - ((1 - exp(-log(1 + i) * n)) / (log(1 + i) * (omega - x)) + (exp(-log(1 + i) * n) * (omega - x - n)) / (omega - x))) / log(1 + i)}
    
    P_żdż_i1 = function(n) {JSN_żdż_i1(n) / renta_terminowa_i1(n)}
    P_żdż_i2 = function(n) {JSN_żdż_i2(n) / renta_terminowa_i2(n)}
    P_żdż_i3 = function(n) {JSN_żdż_i3(n) / renta_terminowa_i3(n)}
    P_żdż_i4 = function(n) {JSN_żdż_i4(n) / renta_terminowa_i4(n)}
    
    plot(P_żdż_i1, from = 0, to = omega1 - x_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(P_żdż_i2,from = 0, to = omega2 - x_, add = TRUE, col = colors[1], lwd = 2,)
    curve(P_żdż_i3,from = 0, to = omega3 - x_, add = TRUE, col = colors[2], lwd = 2,)
    curve(P_żdż_i4,from = 0, to = omega4 - x_, add = TRUE, col = colors[3], lwd = 2,)
    legend(10, 1, 
           legend = c(paste0(" = ", omega1),
                      paste0(" = ", omega2),
                      paste0(" = ", omega3),
                      paste0(" = ", omega4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}


## Wykładniczy ####


# Tylko na dożycie i życie bo tylko na życie lub tylko na dożycie stałe
{
    title_ = "Składka netto w ubezpieczeniu na życie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym"
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 20
    n_ = 20
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    P_ż_mu1 = function(n) {i = i_; x = x_; mu = mu1; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    P_ż_mu2 = function(n) {i = i_; x = x_; mu = mu2; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    P_ż_mu3 = function(n) {i = i_; x = x_; mu = mu3; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    P_ż_mu4 = function(n) {i = i_; x = x_; mu = mu4; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    P_ż_mu5 = function(n) {i = i_; x = x_; mu = mu5; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    P_ż_mu6 = function(n) {i = i_; x = x_; mu = mu6; (log(1 + i) * (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu))) / (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))))}
    
    
    ylim_ = c(-0.05 + min(P_ż_mu1(n_),
                          P_ż_mu2(n_),
                          P_ż_mu3(n_),
                          P_ż_mu4(n_),
                          P_ż_mu5(n_),
                          P_ż_mu6(n_)),
              max(P_ż_mu1(n_),
                  P_ż_mu2(n_),
                  P_ż_mu3(n_),
                  P_ż_mu4(n_),
                  P_ż_mu5(n_),
                  P_ż_mu6(n_)) + 0.001)
    
    plot(P_ż_mu1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(P_ż_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(P_ż_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(P_ż_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(P_ż_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(P_ż_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(0, 0, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}



### Różne mu w zależności od (n) ####

{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym dla i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    i_ = 0.05
    x_ = 20
    n_ = 20
    mu1 = 0.01
    mu2 = 0.02
    mu3 = 0.04
    mu4 = 0.06
    mu5 = 0.08
    mu6 = 0.10
    
    
    JSN_żdż_mu1 = function(n) {i = i_; x = x_; mu = mu1;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu2 = function(n) {i = i_; x = x_; mu = mu2;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu3 = function(n) {i = i_; x = x_; mu = mu3;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu4 = function(n) {i = i_; x = x_; mu = mu4;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu5 = function(n) {i = i_; x = x_; mu = mu5;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_mu6 = function(n) {i = i_; x = x_; mu = mu6;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    
    renta_terminowa_mu1 = function(n) {i = i_; mu = mu1; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu2 = function(n) {i = i_; mu = mu2; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu3 = function(n) {i = i_; mu = mu3; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu4 = function(n) {i = i_; mu = mu4; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu5 = function(n) {i = i_; mu = mu5; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_mu6 = function(n) {i = i_; mu = mu6; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    
    
    P_żdż_mu1 = function(n) {JSN_żdż_mu1(n) / renta_terminowa_mu1(n)}
    P_żdż_mu2 = function(n) {JSN_żdż_mu2(n) / renta_terminowa_mu2(n)}
    P_żdż_mu3 = function(n) {JSN_żdż_mu3(n) / renta_terminowa_mu3(n)}
    P_żdż_mu4 = function(n) {JSN_żdż_mu4(n) / renta_terminowa_mu4(n)}
    P_żdż_mu5 = function(n) {JSN_żdż_mu5(n) / renta_terminowa_mu5(n)}
    P_żdż_mu6 = function(n) {JSN_żdż_mu6(n) / renta_terminowa_mu6(n)}

    
    plot(P_żdż_mu1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(P_żdż_mu2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(P_żdż_mu3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(P_żdż_mu4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    curve(P_żdż_mu5,from = 0, to = n_, add = TRUE, col = colors[4], lwd = 2,)
    curve(P_żdż_mu6,from = 0, to = n_, add = TRUE, col = colors[5], lwd = 2,)
    legend(10, 1, 
           legend = c(paste0("mu = ", mu1),
                      paste0("mu = ", mu2),
                      paste0("mu = ", mu3),
                      paste0("mu = ", mu4),
                      paste0("mu = ", mu5),
                      paste0("mu = ", mu6)),
           fill = c("black",colors[1],colors[2],colors[3],colors[4],colors[5]))
}



### Różne i w zależności od (n) ####


{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla x latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości mu\nw rozkładzie wykładnicznym z mu = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    mu = 0.05
    x_ = 20
    n_ = 20
    i1 = 0.01
    i2 = 0.05
    i3 = 0.10
    i4 = 0.20
    
    
    JSN_żdż_i1 = function(n) {i = i1; x = x_; mu = mu_;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_i2 = function(n) {i = i2; x = x_; mu = mu_;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_i3 = function(n) {i = i3; x = x_; mu = mu_;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    JSN_żdż_i4 = function(n) {i = i4; x = x_; mu = mu_;((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n))}
    
    renta_terminowa_i1 = function(n) {i = i1; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i2 = function(n) {i = i2; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i3 = function(n) {i = i3; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    renta_terminowa_i4 = function(n) {i = i4; mu = mu_; (1 - (((1 - exp(-(log(1 + i) + mu) * n)) * mu) / (log(1 + i) + mu) + (exp(-(log(1 + i) + mu) * n)))) / log(1 + i)}
    
    P_żdż_i1 = function(n) {JSN_żdż_i1(n) / renta_terminowa_i1(n)}
    P_żdż_i2 = function(n) {JSN_żdż_i2(n) / renta_terminowa_i2(n)}
    P_żdż_i3 = function(n) {JSN_żdż_i3(n) / renta_terminowa_i3(n)}
    P_żdż_i4 = function(n) {JSN_żdż_i4(n) / renta_terminowa_i4(n)}
    
    
    plot(P_żdż_i1, from = 0, to = n_,
         xaxt = "n", ylim = ylim_, lwd = 2,
         type = "l", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(0,1,2,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    curve(P_żdż_i2,from = 0, to = n_, add = TRUE, col = colors[1], lwd = 2,)
    curve(P_żdż_i3,from = 0, to = n_, add = TRUE, col = colors[2], lwd = 2,)
    curve(P_żdż_i4,from = 0, to = n_, add = TRUE, col = colors[3], lwd = 2,)
    legend(10, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           fill = c("black",colors[1],colors[2],colors[3]))
}





# ZADANIE 5 ####


kpx = function(k, x, omega) {return( (omega - x - k) / (omega - x))}    
qxk = function(k, x, omega) {return(  1 /  (omega - (x + k)) ) }

## de Moivre'a ####
# JSN w ubezpieczeniu na życie 
JSN_ż = function(i, n, omega, x) {
    suma = 0
    v = (1 / (1 + i))

    for(k_ in 0:n-1) {
        suma = suma + v^(k_+1) * kpx(k_, x, omega) * qxk(k_, x, omega)
    }

    return(suma)
}

# JSN w ubezpieczeniu na dożycie 
JSN_dż = function(i, n, omega, x) {
    v = (1 / (1 + i))
    
    if (n >= omega - x){
        return(NA)    
    } else {
        return(v^n * ((omega - x - n) / (omega - x)))
    }
}


# JSN w ubezpieczeniu na życie i dożycie 
JSN_żdż = function(i, n, omega, x) {
    suma = 0
    v = (1 / (1 + i))
    
    for(k_ in 0:n-1) {
        suma = suma + v^(k_+1) * kpx(k_, x, omega) * qxk(k_, x, omega)
    }
    
    # Dodawanie JSN z ubezpieczenia na życie
    suma = suma + v^n * ((omega - x - n) / (omega - x))
    
    return(suma)
}

## de Moivre'a ####
# renta terminowa n-letnia
renta_terminowa = function(i, n, omega, x) {
    d = i / (1 + i)
    return( (1 - JSN_żdż(i, n, omega, x)) / d )
}

JSN_żdż(0.01, 1, 100, 60)
renta_terminowa(0.01, 1, 100, 60)

# Składka n-letnia na życie i dożycie
P_żdż = function(i, n, omega, x) {
    v = (1 / (1 + i))
    
    P = JSN_żdż(i, n, omega, x) / renta_terminowa(i, n, omega, x)
    
}

# zmienne ogólne do obliczeń
x_ = 60 # 60 latek

#for (max_n in c(omega1 - x_, omega2 - x_, omega3 - x_, omega4 - x_)) 

JSN_ż_wyniki_omega1 <- c()
JSN_ż_wyniki_omega2 <- c()
JSN_ż_wyniki_omega3 <- c()
JSN_ż_wyniki_omega4 <- c()
JSN_dż_wyniki_omega1 <- c()
JSN_dż_wyniki_omega2 <- c()
JSN_dż_wyniki_omega3 <- c()
JSN_dż_wyniki_omega4 <- c()
JSN_żdż_wyniki_omega1 <- c()
JSN_żdż_wyniki_omega2 <- c()
JSN_żdż_wyniki_omega3 <- c()
JSN_żdż_wyniki_omega4 <- c()
renta_terminowa_wyniki_omega1 <- c()
renta_terminowa_wyniki_omega2 <- c()
renta_terminowa_wyniki_omega3 <- c()
renta_terminowa_wyniki_omega4 <- c()
P_żdż_wyniki_omega1 <- c()
P_żdż_wyniki_omega2 <- c()
P_żdż_wyniki_omega3 <- c()
P_żdż_wyniki_omega4 <- c()

JSN_ż_wyniki_i1 <- c()
JSN_ż_wyniki_i2 <- c()
JSN_ż_wyniki_i3 <- c()
JSN_ż_wyniki_i4 <- c()
JSN_dż_wyniki_i1 <- c()
JSN_dż_wyniki_i2 <- c()
JSN_dż_wyniki_i3 <- c()
JSN_dż_wyniki_i4 <- c()
JSN_żdż_wyniki_i1 <- c()
JSN_żdż_wyniki_i2 <- c()
JSN_żdż_wyniki_i3 <- c()
JSN_żdż_wyniki_i4 <- c()
renta_terminowa_wyniki_i1 <- c()
renta_terminowa_wyniki_i2 <- c()
renta_terminowa_wyniki_i3 <- c()
renta_terminowa_wyniki_i4 <- c()
P_żdż_wyniki_i1 <- c()
P_żdż_wyniki_i2 <- c()
P_żdż_wyniki_i3 <- c()
P_żdż_wyniki_i4 <- c()


# Różne omegi
i_ = 0.05 # stopa procentowa 
omega1 = 100
omega2 = 90
omega3 = 80
omega4 = 70
for(n_ in 1:(omega1 - x_)) {
    JSN_ż_wyniki_omega1 <- c(JSN_ż_wyniki_omega1, JSN_ż(i_,n_,omega1,x_))
    JSN_ż_wyniki_omega2 <- c(JSN_ż_wyniki_omega2, JSN_ż(i_,n_,omega2,x_))
    JSN_ż_wyniki_omega3 <- c(JSN_ż_wyniki_omega3, JSN_ż(i_,n_,omega3,x_))
    JSN_ż_wyniki_omega4 <- c(JSN_ż_wyniki_omega4, JSN_ż(i_,n_,omega4,x_))
    JSN_dż_wyniki_omega1 <- c(JSN_dż_wyniki_omega1, JSN_dż(i_,n_,omega1,x_))
    JSN_dż_wyniki_omega2 <- c(JSN_dż_wyniki_omega2, JSN_dż(i_,n_,omega2,x_))
    JSN_dż_wyniki_omega3 <- c(JSN_dż_wyniki_omega3, JSN_dż(i_,n_,omega3,x_))
    JSN_dż_wyniki_omega4 <- c(JSN_dż_wyniki_omega4, JSN_dż(i_,n_,omega4,x_))
    JSN_żdż_wyniki_omega1 <- c(JSN_żdż_wyniki_omega1, JSN_żdż(i_,n_,omega1,x_))
    JSN_żdż_wyniki_omega2 <- c(JSN_żdż_wyniki_omega2, JSN_żdż(i_,n_,omega2,x_))
    JSN_żdż_wyniki_omega3 <- c(JSN_żdż_wyniki_omega3, JSN_żdż(i_,n_,omega3,x_))
    JSN_żdż_wyniki_omega4 <- c(JSN_żdż_wyniki_omega4, JSN_żdż(i_,n_,omega4,x_))
    renta_terminowa_wyniki_omega1 <- c(renta_terminowa_wyniki_omega1, renta_terminowa(i_, n_, omega1, x_))
    renta_terminowa_wyniki_omega2 <- c(renta_terminowa_wyniki_omega2, renta_terminowa(i_, n_, omega2, x_))
    renta_terminowa_wyniki_omega3 <- c(renta_terminowa_wyniki_omega3, renta_terminowa(i_, n_, omega3, x_))
    renta_terminowa_wyniki_omega4 <- c(renta_terminowa_wyniki_omega4, renta_terminowa(i_, n_, omega4, x_))
    P_żdż_wyniki_omega1 <- c(P_żdż_wyniki_omega1, P_żdż(i_, n_, omega1, x_))
    P_żdż_wyniki_omega2 <- c(P_żdż_wyniki_omega2, P_żdż(i_, n_, omega2, x_))
    P_żdż_wyniki_omega3 <- c(P_żdż_wyniki_omega3, P_żdż(i_, n_, omega3, x_))
    P_żdż_wyniki_omega4 <- c(P_żdż_wyniki_omega4, P_żdż(i_, n_, omega4, x_))
}

# Rózne stopy procentowe (i)
omega_ = 100
i1 = 0.03
i2 = 0.05
i3 = 0.10
i4 = 0.20
for(n_ in 1:(omega1 - x_)) {
    JSN_ż_wyniki_i1 <- c(JSN_ż_wyniki_i1, JSN_ż(i1,n_,omega_,x_))
    JSN_ż_wyniki_i2 <- c(JSN_ż_wyniki_i2, JSN_ż(i2,n_,omega_,x_))
    JSN_ż_wyniki_i3 <- c(JSN_ż_wyniki_i3, JSN_ż(i3,n_,omega_,x_))
    JSN_ż_wyniki_i4 <- c(JSN_ż_wyniki_i4, JSN_ż(i4,n_,omega_,x_))
    JSN_dż_wyniki_i1 <- c(JSN_dż_wyniki_i1, JSN_dż(i1,n_,omega_,x_))
    JSN_dż_wyniki_i2 <- c(JSN_dż_wyniki_i2, JSN_dż(i2,n_,omega_,x_))
    JSN_dż_wyniki_i3 <- c(JSN_dż_wyniki_i3, JSN_dż(i3,n_,omega_,x_))
    JSN_dż_wyniki_i4 <- c(JSN_dż_wyniki_i4, JSN_dż(i4,n_,omega_,x_))
    JSN_żdż_wyniki_i1 <- c(JSN_żdż_wyniki_i1, JSN_żdż(i1,n_,omega_,x_))
    JSN_żdż_wyniki_i2 <- c(JSN_żdż_wyniki_i2, JSN_żdż(i2,n_,omega_,x_))
    JSN_żdż_wyniki_i3 <- c(JSN_żdż_wyniki_i3, JSN_żdż(i3,n_,omega_,x_))
    JSN_żdż_wyniki_i4 <- c(JSN_żdż_wyniki_i4, JSN_żdż(i4,n_,omega_,x_))
    renta_terminowa_wyniki_i1 <- c(renta_terminowa_wyniki_i1, renta_terminowa(i1, n_, omega_, x_))
    renta_terminowa_wyniki_i2 <- c(renta_terminowa_wyniki_i2, renta_terminowa(i2, n_, omega_, x_))
    renta_terminowa_wyniki_i3 <- c(renta_terminowa_wyniki_i3, renta_terminowa(i3, n_, omega_, x_))
    renta_terminowa_wyniki_i4 <- c(renta_terminowa_wyniki_i4, renta_terminowa(i4, n_, omega_, x_))
    P_żdż_wyniki_i1 <- c(P_żdż_wyniki_i1, P_żdż(i1, n_, omega_, x_))
    P_żdż_wyniki_i2 <- c(P_żdż_wyniki_i2, P_żdż(i2, n_, omega_, x_))
    P_żdż_wyniki_i3 <- c(P_żdż_wyniki_i3, P_żdż(i3, n_, omega_, x_))
    P_żdż_wyniki_i4 <- c(P_żdż_wyniki_i4, P_żdż(i4, n_, omega_, x_))
}


plot(JSN_ż_wyniki_i1)
points(JSN_ż_wyniki_i2)
points(JSN_ż_wyniki_i3)
points(JSN_ż_wyniki_i4)


## Jednorazowe składki netto ####


### Zależność od omega ####

#### JSN - Na życie ####

{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na życie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_ż_wyniki_omega1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_ż_wyniki_omega2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_ż_wyniki_omega3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_ż_wyniki_omega4, col = colors[3], lwd = 2, pch = 4)
    legend(30, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### JSN - Na dożycie ####
{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_dż_wyniki_omega1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_dż_wyniki_omega2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_dż_wyniki_omega3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_dż_wyniki_omega4, col = colors[3], lwd = 2, pch = 4)
    legend(10, 1, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### JSN - Na życie i dożycie ####
{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na życie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_żdż_wyniki_omega1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_żdż_wyniki_omega2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_żdż_wyniki_omega3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_żdż_wyniki_omega4, col = colors[3], lwd = 2, pch = 4)
    legend(0, 0.3, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### Renta terminowa ####
{
    title_ = "Wartość renty n-letniej dla 60 latka w zależności\nod długości okresu  (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = NULL
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(renta_terminowa_wyniki_omega1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(renta_terminowa_wyniki_omega2, col = colors[1], lwd = 2, pch = 2)
    points(renta_terminowa_wyniki_omega3, col = colors[2], lwd = 2, pch = 3)
    points(renta_terminowa_wyniki_omega4, col = colors[3], lwd = 2, pch = 4)
    legend(0, 10, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### Składka netto w ubezpieczeniu na życie i dożycie ####

{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości omega\nw rozkładzie De Moivre z i = 0.05"
    ylim_ = NULL
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(P_żdż_wyniki_omega1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(P_żdż_wyniki_omega2, col = colors[1], lwd = 2, pch = 2)
    points(P_żdż_wyniki_omega3, col = colors[2], lwd = 2, pch = 3)
    points(P_żdż_wyniki_omega4, col = colors[3], lwd = 2, pch = 4)
    legend(30, 7, 
           legend = c(paste0("omega = ", omega1),
                      paste0("omega = ", omega2),
                      paste0("omega = ", omega3),
                      paste0("omega = ", omega4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}



### Zależność od stopy procentowej (i) ####

#### JSN - Na życie ####

{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na życie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_ż_wyniki_i1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_ż_wyniki_i2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_ż_wyniki_i3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_ż_wyniki_i4, col = colors[3], lwd = 2, pch = 4)
    legend(10, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### JSN - Na dożycie ####
{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_dż_wyniki_i1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_dż_wyniki_i2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_dż_wyniki_i3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_dż_wyniki_i4, col = colors[3], lwd = 2, pch = 4)
    legend(10, 1, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### JSN - Na życie i dożycie ####
{
    title_ = "Jednorazowa składka netto w ubezpieczeniu na życie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = c(0,1)
    ylab_ = "Wartość JSN"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(JSN_żdż_wyniki_i1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(JSN_żdż_wyniki_i2, col = colors[1], lwd = 2, pch = 2)
    points(JSN_żdż_wyniki_i3, col = colors[2], lwd = 2, pch = 3)
    points(JSN_żdż_wyniki_i4, col = colors[3], lwd = 2, pch = 4)
    legend(0, 0.3, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### Renta terminowa ####
{
    title_ = "Wartość renty n-letniej dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = NULL
    ylab_ = "Wartość renty"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(renta_terminowa_wyniki_i1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(renta_terminowa_wyniki_i2, col = colors[1], lwd = 2, pch = 2)
    points(renta_terminowa_wyniki_i3, col = colors[2], lwd = 2, pch = 3)
    points(renta_terminowa_wyniki_i4, col = colors[3], lwd = 2, pch = 4)
    legend(0, 12, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

#### Składka netto w ubezpieczeniu na życie i dożycie ####

{
    title_ = "Składka netto w ubezpieczeniu na życie i dożycie dla 60 latka w zależności\nod długości okresu ubezpieczenia (n) dla różnych wartości stopy procentowej\nw rozkładzie De Moivre dla omega = 100"
    ylim_ = NULL
    ylab_ = "Wartość składki netto"
    xlab_ = "Okres ubezpieczenia (n)"
    
    plot(P_żdż_wyniki_i1,
         xaxt = "n", ylim = ylim_, lwd = 2, pch = 1,
         type = "p", xlab = xlab_, ylab = ylab_, col = "black")
    axis(1, at =  c(1,5,10,20,30,40,50,60,70,80,90,100), 
         labels = c(1,5,10,20,30,40,50,60,70,80,90,100))
    title(title_)
    points(P_żdż_wyniki_i2, col = colors[1], lwd = 2, pch = 2)
    points(P_żdż_wyniki_i3, col = colors[2], lwd = 2, pch = 3)
    points(P_żdż_wyniki_i4, col = colors[3], lwd = 2, pch = 4)
    legend(30, 7, 
           legend = c(paste0("i = ", i1),
                      paste0("i = ", i2),
                      paste0("i = ", i3),
                      paste0("i = ", i4)),
           col = c("black",colors[1],colors[2],colors[3]),
           pch = c(1,2,3,4))
}

