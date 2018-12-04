library(readr)
donnee <- read.csv("donnee.csv", header = TRUE, sep = ",", dec = ".")

g = donnee$Grandeur
p = donnee$Poids
a = donnee$Age
nbTirs = donnee$Nombre.de.tirs
temps = donnee$Temps.sur.la.glace
tempsPartie = donnee$Temps.Partie.jouees
buts = donnee$Nombre.de.buts


# Grandeur (maxime) -> but
plot(g, buts,main = "Nombre de buts en fonction de la grandeur", xlab = "Taille (en pouces)", ylab = "Nombre de buts")
fit <- glm(buts~g)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

# Poid -> but
plot(p, buts,main = "Nombre de buts en fonction du poids", xlab = "Poid (en livres)", ylab = "Nombre de buts")
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
plot(temps, buts, main = "Nombres de buts en fonction du temps sur la glace", xlab = "Temps joué (secondes)", ylab = "Nombre de buts")
fit <- glm(buts~temps)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

summary(lm(buts~g))
summary(lm(buts~p))
coef(summary(lm(buts~p)))[, "Std. Error"] 
summary(lm(buts~a))
summary(lm(buts~nbTirs))
summary(lm(buts~temps))
 
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


anova(lm(buts~nbTirs))
anova(lm(buts~g))

m1.lm <- lm(buts~nbTirs)
m1.res = resid(m1.lm)
m1.res
plot(m1.res,main = "Graphique des résidus pour les buts et le nombre de tirs", xlab = "Variable", ylab = "Résidus") 
abline(0, 0)  

m2.lm <- lm(buts~a)
m2.res = resid(m2.lm)     
plot(m2.res,main = "Graphique des résidus pour les buts et l'âge", xlab = "Variable", ylab = "Résidus") 
abline(0, 0)

m3.lm <- lm(buts~temps)
m3.res = resid(m3.lm)
plot(m3.res,main = "Graphique des résidus pour les buts et le temps de glace", xlab = "Variable", ylab = "Résidus") 
abline(0, 0)

m4.lm <- lm(buts~g)
m4.res = resid(m4.lm)
# m4.res
plot(m4.res,main = "Graphique des résidus pour les buts et la grandeur", xlab = "Variable", ylab = "Résidus") 
abline(0, 0)

m5.lm <- lm(buts~p)
m5.res = resid(m5.lm)     
plot(m5.res,main = "Graphique des résidus pour les buts et le poids", xlab = "Variable", ylab = "Résidus") 
abline(0, 0)

###############################
###############################

lm1 <- lm(nbTirs~buts)
res = residuals(lm1)
plot(lm1,main = "Graphique des résidus pour les buts et le nombre de tirs",sub.caption = "Nombre de tirs") 
abline(0, 0)

lm2 <- lm(g~buts, na.action = "na.exclude")
res = residuals(lm2)
plot(lm2,main = "Graphique des résidus pour les buts et la taille",sub.caption = "Taille (en pouces)") 
abline(0, 0) 

lm3 <- lm(p~buts)
res = residuals(lm3)
plot(lm3,main = "Graphique des résidus pour les buts et le poids",sub.caption = "Poids (en livres)") 
abline(0, 0)

lm4 <- lm(a~buts)
res = residuals(lm4)
plot(lm4,main = "Graphique des résidus pour les buts et l'âge",sub.caption = "Âge (en année)") 
abline(0, 0)

lm5 <- lm(temps~buts)
res = residuals(lm5)
plot(lm5,main = "Graphique des résidus pour les buts et le temps sur la glace",sub.caption = "Temps (en secondes)", na.action = na.exclude) 
abline(0, 0)


# Intervalles de confiance
# ===================================================================
n = length(buts)

# Changer ces variables pour trouver les autres intervalles
var(nbTirs)
sd = sd(nbTirs)
x_bar = mean(nbTirs)

# Moyennes
margin_error = qnorm(.95) * (sd / sqrt(n))
L = x_bar - margin_error
U = x_bar + margin_error

x_bar
L
U

# Variances
sd
sd/(1 + qnorm(.95)/(2*sqrt(n))) # L 
sd/(1 - qnorm(.95)/(2*sqrt(n))) # U

