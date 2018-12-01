library(readr)
donnee <- read.csv("donnee.csv", header = TRUE, sep = ",", dec = ".")

g = donnee$Grandeur
p = donnee$Poids
a = donnee$Âge
nbTirs = donnee$Nombre.de.tirs
temps = donnee$Temps.sur.la.glace
tempsPartie = donnee$Temps.Partie.jouées
buts = donnee$Nombre.de.buts

# layout(matrix(1:2,1,2)) # ceci permet de diviser la sortie graphique en 2

# Grandeur (maxime) -> but
plot(g, buts,main = "Nombre de buts en fonction de la grandeur", xlab = "Taille (en pouces)", ylab = "Nombre de buts")
fit <- glm(buts~g)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Poid -> but
plot(p, buts,main = "Nombre de buts en fonction du poid", xlab = "Poid (en livres)", ylab = "Nombre de buts")
fit <- glm(buts~p)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Age -> but
plot(a, buts,main = "Nombre de buts en fonction de l'âge", xlab = "Âge (en années)", ylab = "Nombre de buts")
fit <- glm(buts~a)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Nombre tirs -> but
plot(nbTirs, buts,main = "Nombre de buts en fonction du nombre de tirs", xlab = "Nombre de tirs", ylab = "Nombre de buts")
fit <- glm(buts~nbTirs)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Temps de glace -> Nb Tirs
plot(tempsPartie, nbTirs,main = "Nombres de tirs en fonction du temps sur la glace", xlab = "Temps par partie (en minutes)", ylab = "Nombre de tirs")
fit <- glm(nbTirs~tempsPartie)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

 
# Tests Fisher - Ces tests fonctionnent pour trouver nos valeurs p, 
# mais les valeurs changent à chaque fois vu qu'on simule quelques données
# ===================================================================

# Si nous prenons un seuil de 0.05, on a quelques résultats:
library(Hmisc)
fisher.test(buts, g, simulate.p.value = TRUE)           # p > 0.05 - On ne rejette pas H0
fisher.test(buts, p, simulate.p.value = TRUE)           # p > 0.05
fisher.test(buts, a, simulate.p.value = TRUE)           # p > 0.05
fisher.test(buts, nbTirs, simulate.p.value = TRUE)      # p < 0.05 - On rejette H0, les valeurs ne sont pas indépendantes
fisher.test(buts, tempsPartie, simulate.p.value = TRUE) # p < 0.05 - On rejette H0
fisher.test(temps, nbTirs, simulate.p.value = TRUE) # p < 0.05 - On rejette H0


# Tests Khi-deux - Fonctionne pas pcq les valeurs estimées sont trop petites
# Il faudrait trouver un moyen pour regrouper des valeurs en blocs de "range"
# pour donner des valeurs estimées >= 5. J'ai gosser longtemps et j'ai pas trouvé comment faire.
# Je pense qu'on pourrait utiliser les résultats fisher en haut à place.
# ===================================================================
chisq.test(buts, g)
chisq.test(buts, p)
chisq.test(buts, a)

chisq.test(buts, nbTirs)
chisq.test(buts, temps)

anova(lm(buts~nbTirs))
anova(lm(buts~g), )