################################################################################
## Café Analytica
## Thema: Vorhersage versus Interpretation
## Status: Studie "Daten verstehen / Daten vorbereiten"
## 
##
## Günter Faes, https://www.faes.de & https://www.faes.de/r-statistik/
## Version 0.2, 20.07.2021
## R-Version: 4.1.0
## RStudio-Version: 1.4.1717
##
################################################################################

## Benötigte Pakete laden:
library(doBy)    # summaryBy
library(psych)   # Faktorenanalyse: KMO

## Daten laden:
Daten <- read.csv2("Rotweindaten.csv")
str(Daten)
View(Daten)

## -------------- Deskriptive Statistik --------------

## Verteilungsübersicht für alle Merkmale:

# FixedAcidity:
summary(Daten$FixedAcidity)
par(mfrow = c(1,2))
hist(Daten$FixedAcidity, main = "Histogramm FixedAcidity",
     xlab = "FixedAcidity", ylab = "Häufigkeit")
boxplot(Daten$FixedAcidity, main = "Boxplot FixedAcidity",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# VolatileAcidity:
summary(Daten$VolatileAcidity)
par(mfrow = c(1,2))
hist(Daten$VolatileAcidity, main = "Histogramm VolatileAcidity",
     xlab = "VolatileAcidity", ylab = "Häufigkeit")
boxplot(Daten$VolatileAcidity, main = "Boxplot VolatileAcidity",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# CitricAcid:
summary(Daten$CitricAcid)
par(mfrow = c(1,2))
hist(Daten$CitricAcid, main = "Histogramm CitricAcid",
     xlab = "CitricAcid", ylab = "Häufigkeit")
boxplot(Daten$CitricAcid, main = "Boxplot CitricAcid",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# ResidualSugar:
summary(Daten$ResidualSugar)
par(mfrow = c(1,2))
hist(Daten$ResidualSugar, main = "Histogramm ResidualSugar",
     xlab = "ResidualSugar", ylab = "Häufigkeit")
boxplot(Daten$ResidualSugar, main = "Boxplot ResidualSugar",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# Chlorides:
summary(Daten$Chlorides)
par(mfrow = c(1,2))
hist(Daten$Chlorides, main = "Histogramm Chlorides",
     xlab = "Chlorides", ylab = "Häufigkeit")
boxplot(Daten$Chlorides, main = "Boxplot Chlorides",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# FreeSulfurDioxide:
summary(Daten$FreeSulfurDioxide)
par(mfrow = c(1,2))
hist(Daten$FreeSulfurDioxide, main = "Histogramm FreeSulfurDioxide",
     xlab = "FreeSulfurDioxide", ylab = "Häufigkeit")
boxplot(Daten$FreeSulfurDioxide, main = "Boxplot FreeSulfurDioxide",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# TotalSulfurDioxide:
summary(Daten$TotalSulfurDioxide)
par(mfrow = c(1,2))
hist(Daten$TotalSulfurDioxide, main = "Histogramm TotalSulfurDioxide",
     xlab = "TotalSulfurDioxide", ylab = "Häufigkeit")
boxplot(Daten$TotalSulfurDioxide, main = "Boxplot TotalSulfurDioxide",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# Sulphates:
summary(Daten$Sulphates)
par(mfrow = c(1,2))
hist(Daten$Sulphates, main = "Histogramm Sulphates",
     xlab = "Sulphates", ylab = "Häufigkeit")
boxplot(Daten$Sulphates, main = "Boxplot Sulphates",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# Density:
summary(Daten$Density)
par(mfrow = c(1,2))
hist(Daten$Density, main = "Histogramm Density",
     xlab = "Density", ylab = "Häufigkeit")
boxplot(Daten$Density, main = "Boxplot Density",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# pH:
summary(Daten$pH)
par(mfrow = c(1,2))
hist(Daten$pH, main = "Histogramm pH",
     xlab = "pH", ylab = "Häufigkeit")
boxplot(Daten$pH, main = "Boxplot pH",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# Alcohol:
summary(Daten$Alcohol)
par(mfrow = c(1,2))
hist(Daten$Alcohol, main = "Histogramm Alcohol",
     xlab = "Alcohol", ylab = "Häufigkeit")
boxplot(Daten$Alcohol, main = "Boxplot Alcohol",
        ylab = "Wertebereich")
par(mfrow = c(1,1))

# Quality:
table(Daten$Quality)
barplot(table(Daten$Quality), main = "Übersicht der Qualitäts-Beurteilungen",
     xlab = "Noten (0 = schlecht, 10 = sehr gut)", ylab = "Häufigkeit")

## Merkmalsübersicht auf Basis der Qualitätsnote:

# FixedAcidity:
S_FA <- summaryBy(FixedAcidity ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_FA
barplot(S_FA$FixedAcidity.mean ~ S_FA$Quality,
        main ="Übersicht Mittelwert\n FixedAcidity und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

# VolatileAcidity:
S_VA <- summaryBy(VolatileAcidity ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_VA
barplot(S_VA$VolatileAcidity.mean ~ S_VA$Quality,
        main ="Übersicht Mittelwert\n VolatileAcidity und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

# CitricAcid:
S_CA <- summaryBy(CitricAcid ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_CA
barplot(S_CA$CitricAcid.mean ~ S_CA$Quality,
        main ="Übersicht Mittelwert\n CitricAcid und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

# ResidualSugar:
S_RS <- summaryBy(ResidualSugar ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_RS
barplot(S_RS$ResidualSugar.mean ~S_RS$Quality,
        main ="Übersicht Mittelwert\n ResidualSugar und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

# Chlorides:
S_Cl <- summaryBy(Chlorides ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_Cl
barplot(S_Cl$Chlorides.mean ~S_Cl$Quality,
        main ="Übersicht Mittelwert\n Chlorides und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

## Schwefeldioxide und Sulfate werden gemeinsam grafisch dargestellt:
# FreeSulfurDioxide:
S_FSD <- summaryBy(FreeSulfurDioxide ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_FSD
# TotalSulfurDioxide:
S_TSD <- summaryBy(TotalSulfurDioxide ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_TSD
# Sulphates:
S_Sul <- summaryBy(Sulphates ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_Sul

par(mfrow = c(1,3))
barplot(S_FSD$FreeSulfurDioxide.mean ~S_FSD$Quality,
        main ="Übersicht Mittelwert\n FreeSulfurDioxide und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

barplot(S_TSD$TotalSulfurDioxide.mean ~S_TSD$Quality,
        main ="Übersicht Mittelwert\n TotalSulfurDioxide und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

barplot(S_Sul$Sulphates.mean ~S_Sul$Quality,
        main ="Übersicht Mittelwert\n Sulphates und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")
par(mfrow = c(1,1))
## ------------- Ende SO4 / SO2 ------------

# Density:
S_Den <- summaryBy(Density ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_Den
barplot(S_Den$Density.mean ~S_Den$Quality,
        main ="Übersicht Mittelwert\n Density und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte",
        ylim = c(0, 1.1))

# pH:
S_pH <- summaryBy(pH ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_pH
barplot(S_pH$pH.mean ~S_pH$Quality,
        main ="Übersicht Mittelwert\n pH und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte",
        ylim = c(0, 3.5))

# Alcohol:
S_Alco <- summaryBy(Alcohol ~ Quality, data = Daten, FUN = c(mean, median, sd, length)); S_Alco
barplot(S_Alco$Alcohol.mean ~S_Alco$Quality,
        main ="Übersicht Mittelwert\n Alcohol und Qualitätsnote",
        xlab = "Noten (0 = schlecht, 10 = sehr gut)",
        ylab = "Mittelwerte")

## -------------------- Ende Deskriptive Statistik ---------------------

## ------------------- Explorative Statistik ---------------------------

## Korrelieren die unabhängigen Beobachtungen unertereinander?
## Gibt es Merkmale, die nur ein geringes "Loading" zeigen?
## Das soll durch die Faktorenanalyse geprüft werden.

# Sub-Datensatz ohne die Qualitätsnote:
Daten_FA <- Daten[,1:11]

# KMO-Kriterium:
KMO(Daten_FA)$MSA

# Scree-Plot:
scree(Daten_FA)

# Faktorenanalyse:
FA_Modell <- factanal(Daten_FA, factors = 4, rotation = "varimax", scores = "Bartlett")
FA_Modell


        


        
