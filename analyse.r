library(readr)
basicTrendline
donnee <- read.csv("donnee.csv", header = TRUE, sep = ",", dec = ".")

g = donnee$Grandeur
p = donnee$Poids
a = donnee$Âge
nbTirs = donnee$Nombre.de.tirs
temps = donnee$Temps.sur.la.glace
buts = donnee$Nombre.de.buts

# layout(matrix(1:2,1,2)) # ceci permet de diviser la sortie graphique en 2

# Grandeur (maxime) -> but
plot(g, buts,main = "Nombre de buts en fonction de la grandeur", xlab = "Taille (en pouce)", ylab = "Nombre de buts")
fit <- glm(buts~g)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Poid -> but
plot(p, buts,main = "Nombre de buts en fonction du poid", xlab = "Poid (en livres)", ylab = "Nombre de buts")
fit <- glm(buts~p)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Age -> but
plot(a, buts,main = "Nombre de buts en fonction de l'âge", xlab = "Age (en annee)", ylab = "Nombre de buts")
fit <- glm(buts~a)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Nombre tirs -> but
plot(nbTirs, buts,main = "Nombre de buts en fonction du nombres de tirs", xlab = "Nombre de tirs", ylab = "Nombre de buts")
fit <- glm(buts~nbTirs)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Temps de glace -> Nb Tirs
plot(temps, nbTirs,main = "Nombres de tirs en fonction du temps sur la glace", xlab = "Temps sur la glace", ylab = "Nombre de tirs")
fit <- glm(nbTirs~temps)
co <- coef(fit)
abline(fit, col="blue", lwd=2)


