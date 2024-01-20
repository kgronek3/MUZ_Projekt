



# ZADANIE 1 ####


# de Moivre'a

# W zależności od wieku

for (x in seq(1, 65, by = 1)) {
    
    deMoivre_tpx = function(t) {
        omega = 100
        return( (omega - x - t) / (omega - x))   
    }    
    if (x == 1) {
        curve(deMoivre_tpx,from = 0, 35, ylim = c(0,1))
    } else {
        curve(deMoivre_tpx,from = 0, 35, ylim = c(0,1), add = TRUE)
    }
    
    
}

# Zmiana omegi
for (omega in seq(51, 100, by = 1)) {
    x = 50
    deMoivre_tpx = function(t) {
        return( (omega - x - t) / (omega - x))   
    }    
    if (x == 1) {
        curve(deMoivre_tpx,from = 0, to = omega - x, ylim = c(0,1))
    } else {
        curve(deMoivre_tpx,from = 0, to = omega - x, ylim = c(0,1), add = TRUE)
    }
    
    
}

(100 - 65 - 1) / (100 - 35)
(100 - 65 - 2) / (100 - 35)
(100 - 65 - 3) / (100 - 35)
(100 - 65 - 4) / (100 - 35)
(100 - 65 - 5) / (100 - 35)
(100 - 65 - 6) / (100 - 35)
(100 - 65 - 7) / (100 - 35)

# Wykładniczy

Wykladniczy1 <- function(x) {exp(-1 * 0.01 * x)}
Wykladniczy2 <- function(x) {exp(-1 * 0.02 * x)}
Wykladniczy3 <- function(x) {exp(-1 * 0.03 * x)}
Wykladniczy4 <- function(x) {exp(-1 * 0.04 * x)}
Wykladniczy5 <- function(x) {exp(-1 * 0.05 * x)}
Wykladniczy6 <- function(x) {exp(-1 * 0.06 * x)}
Wykladniczy7 <- function(x) {exp(-1 * 0.07 * x)}
Wykladniczy8 <- function(x) {exp(-1 * 0.08 * x)}
Wykladniczy9 <- function(x) {exp(-1 * 0.09 * x)}

curve(Wykladniczy1,from = 0, 100, ylim = c(0,1))
curve(Wykladniczy2,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy3,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy4,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy5,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy6,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy7,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy8,from = 0, 100, ylim = c(0,1), add = TRUE)
curve(Wykladniczy9,from = 0, 100, ylim = c(0,1), add = TRUE)

for (mu in seq(0.01, 0.10, by = 0.01)) {

Wykladniczy <- function(x) {return(exp(-1 * mu * x))}
    
if (mu == 0.01) {
    curve(Wykladniczy,from = 0, 100, ylim = c(0,1))
} else {
    curve(Wykladniczy,from = 0, 100, ylim = c(0,1), add = TRUE)
}
    
    
}





# ZADANIE 2 ####
# ZADANIE 3 ####
# ZADANIE 4 ####
# ZADANIE 5 ####



