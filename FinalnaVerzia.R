data <- read.csv("SBAnational.csv")

# Iba Kalifornia
data <- data[data$State == "CA",]

# Výber premenných
data <- data[,c("NAICS", "Term", "NoEmp", "NewExist", "UrbanRural", "ChgOffDate", "MIS_Status", "SBA_Appv", "GrAppv", "DisbursementDate")]

# Faktorizácia MIS_Status
data$MIS_Status <- factor(data$MIS_Status, levels=c("P I F", "CHGOFF"), labels=0:1)

# SBA_Appv upravi na pomer ku celkovej výške pôžièky
library(readr)
data$GrAppv <- parse_number(data$GrAppv)
data$SBA_Appv <- parse_number(data$SBA_Appv)
data$SBA_Appv <- data$SBA_Appv / data$GrAppv

# Vymazanie dát, kde nieèo chýba
data <- data[!is.na(data$NAICS) & 
               !is.na(data$Term) & 
               !is.na(data$NoEmp) & 
               !is.na(data$MIS_Status) & 
               !is.na(data$NewExist) & 
               !is.na(data$GrAppv) & 
               !is.na(data$SBA_Appv) & 
               !is.na(data$UrbanRural),]

# Vytvorenie premennej Industry (prvá cifra)
data$Industry <- substr(data$NAICS,1,1)

# Pri všetkých dátach chceme vedie odvetvie priemyslu, takže nebudeme pracova s tými, ktorých odvetvie nepoznáme
data <- data[data$NAICS != 0,]

# Pri všetkých dátach chceme vedie NewExist a UrbanRural, takže nebudeme pracova s tými, ktorých info nepoznáme
data <- data[data$NewExist != 0,]
data <- data[data$UrbanRural != 0,]

# Faktorizácia NewExist a UrbanRural
data$NewExist <- factor(data$NewExist, levels=c(1, 2), labels=0:1)
data$UrbanRural <- factor(data$UrbanRural, levels=c(1, 2), labels=0:1)


# Vytvorenie DummyVariables
D1 <- rep(0, nrow(data))
D2 <- rep(0, nrow(data))
D3 <- rep(0, nrow(data))
D4 <- rep(0, nrow(data))
D5 <- rep(0, nrow(data))
D6 <- rep(0, nrow(data))
D7 <- rep(0, nrow(data))
data <- cbind(D1, data)
data <- cbind(D2, data)
data <- cbind(D3, data)
data <- cbind(D4, data)
data <- cbind(D5, data)
data <- cbind(D6, data)
data <- cbind(D7, data)

for (i in 1:nrow(data)) {
  if (as.numeric(data$Industry[i]) == 1) {
    data$D1[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 2) {
    data$D2[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 3) {
    data$D3[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 4) {
    data$D4[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 5) {
    data$D5[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 6) {
    data$D6[i] <- 1
  } else if (as.numeric(data$Industry[i]) == 7) {
    data$D7[i] <- 1
  }
}

data$D1 <- factor(data$D1, levels=c(0, 1), labels=0:1)
data$D2 <- factor(data$D2, levels=c(0, 1), labels=0:1)
data$D3 <- factor(data$D3, levels=c(0, 1), labels=0:1)
data$D4 <- factor(data$D4, levels=c(0, 1), labels=0:1)
data$D5 <- factor(data$D5, levels=c(0, 1), labels=0:1)
data$D6 <- factor(data$D6, levels=c(0, 1), labels=0:1)
data$D7 <- factor(data$D7, levels=c(0, 1), labels=0:1)

# Rozdelenie dátumov na zložky
library(tidyr)
data$DisbursementDate[data$DisbursementDate == ""] <- NA
data <- data %>% separate(DisbursementDate, c("DisbursementDay", "DisbursementMonth", "DisbursementYear"), "-")
data$ChgOffDate[data$ChgOffDate == ""] <- NA
data <- data %>% separate(ChgOffDate, c("ChgOffDay", "ChgOffMonth", "ChgOffYear"), "-")

# Vymazanie dát, kde nie je urèený dátum schválenia pôžièky bankou
data <- data[!is.na(data$DisbursementYear),]

# K jednociferným dátumom pridáme 0 na zaèiatok
for (i in 1:nrow(data)) {
  if(!is.na(data$DisbursementDay[i])) {
    if(as.numeric(data$DisbursementDay[i]) < 10) {
      data$DisbursementDay[i] <- paste0("0", data$DisbursementDay[i])
    }
  }
}

for (i in 1:nrow(data)) {
  if(!is.na(data$ChgOffDay[i])) {
    if(as.numeric(data$ChgOffDay[i]) < 10) {
      data$ChgOffDay[i] <- paste0("0", data$ChgOffDay[i])
    }
  }
}

# Opätovné zlepenie dní, mesiacov a rokov (ApprovalDate zakomentované)
data$DisbursementDate <- apply(data[,c("DisbursementDay", "DisbursementMonth", "DisbursementYear")], 1, paste0, collapse = "-")
data <- data[,!(names(data) %in% c("DisbursementDay", "DisbursementMonth", "DisbursementYear"))]

data$ChgOffDate <- apply(data[,c("ChgOffDay", "ChgOffMonth", "ChgOffYear")], 1, paste0, collapse = "-")
data <- data[,!(names(data) %in% c("ChgOffDay", "ChgOffMonth", "ChgOffYear"))]

# Zo Stringov urobíme formát Dátum
# install.packages("lubridate")
require(lubridate)
data$DisbursementDate <- dmy(data$DisbursementDate)
data$ChgOffDate <- dmy(data$ChgOffDate)

# Vymazanie chybných dát
data <- data[!(data$MIS_Status == 0 & !is.na(data$ChgOffDate)),]
data <- data[!(data$MIS_Status == 1 & is.na(data$ChgOffDate)),]

# Vytvorenie premennej EndDate
data$EndDate <- data$DisbursementDate

# lubridate (Pridávame Term ku EndDate ALEBO ChgOffDate bude EndDate - ak iba Kalifornia, zhruba 2 minúty)
for (i in 1:nrow(data)) {
  if (is.na(data$ChgOffDate[i])) {
    data$EndDate[i] = data$EndDate[i] %m+% months(data$Term[i])
  } else {
    data$EndDate[i] = data$ChgOffDate[i]
  }
}

data2 <- cbind(data)
# data3 <- cbind(data)

# Vytvoríme testovacie dáta, t.j. dáta, na ktorých vytvoríme prvý model logistickej regresie
testData <- data[data$EndDate < ymd("2010-01-01"),]
testData <- testData[order(testData$EndDate),]

fit <- glm(MIS_Status ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + Term + NoEmp + NewExist + SBA_Appv + GrAppv + UrbanRural, data=testData, family=binomial)

# Koeficienty, ktoré vypoèítala glm funkcia (pri Industry sú celkom ve¾ké, okolo 10)
beta <- fit$coefficients
# print(beta)
# summary(fit)

# Vytvorenie DataFrame-u, v ktorom sú iba ståpce, ktoré sa nachádzajú v modeli
zvektoruj <- function(fit, data) {
  data2 = data.frame()
  for (i in 1:length(beta)) {
    f = labels(terms(fit))
    data2 <- data[,f]
  }
  return(data2)
}

# Vytvoríme tabu¾ku, kde sú iba ståpce použité vo fite a pridáme do nej ståpec jednotiek (absolútny èlen)
tabulka <- zvektoruj(fit, testData)
jednotky <- rep(1, nrow(tabulka))
tabulka <- cbind(jednotky, tabulka)

# Slúži na výpoèet pravdepodobnosti
fi <- function(z) {
  return(1/(1+exp(-z)))
}

# Koeficienty vynásobené nejakým riadkom
pravdepodobnost <- function(beta, x) {
  return(t(beta) %*% as.numeric(sapply(x, as.character)))
}

# Kontrola (porovnanie našej funkcie a fitu)
# predict(fit, data.frame(D1 = as.factor(0), D2 = as.factor(0), D3 = as.factor(0), D4 = as.factor(1), D5 = as.factor(0), D6 = as.factor(0), D7 = as.factor(0), Term = 4, NoEmp = 46, NewExist = as.factor(1), SBA_Appv = 0.75, GrAppv = 510000, UrbanRural = as.factor(0)), type="response")
# fi(pravdepodobnost(beta, tabulka[1,]))
# Rovnajú sa

# Funkcie z èlánku
a <- function(z) {
  return(-fi(z) * (1-fi(z)))
}

b <- function(z,c) {
  return(fi(z) - c + z*a(z))
}

psi <- function(beta) {
  v = 0
  for(i in 1:nrow(testData)) {
    u = c(a(pravdepodobnost(beta, tabulka[i,]))) * as.numeric(sapply(tabulka[i,], as.character)) %*% t(as.numeric(sapply(tabulka[i,], as.character)))
    v = v + u
  }
  return(v)
}

theta <- function(beta) {
  v = 0
  for(i in 1:nrow(testData)) {
    u = c(b(pravdepodobnost(beta, tabulka[i,]), fit$y[i])) * as.numeric(sapply(tabulka[i,], as.character))
    v = v + u 
  }
  return(v)
}

# install.packages("matlib")
require(matlib)
beta2 <- function(psi, theta) {
  return(inv(psi) %*% theta)
}

# Kontrola správnosti (aký je rozdiel medzi koeficientami modelu a odhadom keoficientov)
# B2 <- beta2(psi(beta), theta(beta))
# print(B2)
# print(B2 - beta)

# Zvyšné dáta - po pridaní každého riadku sa model aktualizuje
noveData <- data[data$DisbursementDate >= ymd("2010-01-01"),]
noveData <- noveData[order(noveData$DisbursementDate),]
netestovacieData <- data[data$EndDate >= ymd("2010-01-01"),]
netestovacieData <- netestovacieData[order(netestovacieData$EndDate),]

tabulka2 <- zvektoruj(fit, noveData)
jednotky <- rep(1, nrow(tabulka2))
tabulka2 <- cbind(jednotky, tabulka2)

tabulka4 <- zvektoruj(fit, netestovacieData)
jednotky <- rep(1, nrow(tabulka4))
tabulka4 <- cbind(jednotky, tabulka4)

# Vytvorenie nových ståpcov
PravdepodobnostPN <- rep(0, nrow(noveData))
Rozhodnutie <- rep(-1, nrow(noveData))
noveData <- cbind(PravdepodobnostPN, noveData)
noveData <- cbind(Rozhodnutie, noveData)

PravdepodobnostNTD <- rep(0, nrow(netestovacieData))
netestovacieData <- cbind(PravdepodobnostNTD, netestovacieData)

# Prvý cut-off
PravdepodobnostTD <- rep(0, nrow(testData))
testData <- cbind(PravdepodobnostTD, testData)
for (i in 1:(nrow(testData))) {
  testData$PravdepodobnostTD[i] = fi(pravdepodobnost(beta, tabulka[i,]))
}
require(pROC)
rocobj <- roc(testData$MIS_Status, testData$PravdepodobnostTD)
CO <- coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- as.numeric(CO[1])
# plot(rocobj)

pocetNet = 100

ST = Sys.time()
psi <- psi(beta)
theta <- theta(beta)
j = 1

# Pridávanie riadkov, zakaždým sa model aktualizuje (zhruba 5 minút)
for (i in 1:pocetNet) {

  if (j < nrow(noveData)) {
    while (noveData$DisbursementDate[j] <= netestovacieData$EndDate[i]) {
      noveData$PravdepodobnostPN[j] = fi(pravdepodobnost(beta, tabulka2[j,]))
      if (noveData$PravdepodobnostPN[j] >= cutoff) {
        noveData$Rozhodnutie[j] <- 1
      } else {
        noveData$Rozhodnutie[j] <- 0
      }
      j = j + 1
    }
  }

  novyRiadok <- tabulka4[i,]
  MIS <- netestovacieData$MIS_Status[i]
  psiNova <- psi + c(a(pravdepodobnost(beta, novyRiadok))) * unlist(novyRiadok) %*% t(unlist(novyRiadok))
  thetaNova <- theta + c(b(pravdepodobnost(beta, novyRiadok), as.numeric(MIS))) * unlist(novyRiadok)
  betaNova <- beta2(psiNova, thetaNova)

  beta <- betaNova
  psi <- psiNova
  theta <- thetaNova

}
ET = Sys.time()
D = ET - ST
print(D)

# ROC
TP <- 0
FP <- 0
FN <- 0
TN <- 0
for (i in 1:nrow(noveData)) {
  if(noveData$Rozhodnutie[i] == 1 && noveData$MIS_Status[i] == 1) {
    TP = TP + 1
  } else if (noveData$Rozhodnutie[i] == 1 && noveData$MIS_Status[i] == 0) {
    FP = FP + 1
  } else if (noveData$Rozhodnutie[i] == 0 && noveData$MIS_Status[i] == 1) {
    FN = FN + 1
  } else if (noveData$Rozhodnutie[i] == 0 && noveData$MIS_Status[i] == 0) {
    TN = TN + 1
  }
}

Senzitivita = TP / (TP + FN)
Specificita = TN / (TN + FP)
Presnost = (TP + TN) / (TP + TN + FP + FN)

J = Senzitivita - (1 - Specificita)

Presnost







# fitD3 <- glm(MIS_Status ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + Term + NoEmp + NewExist + SBA_Appv + GrAppv + UrbanRural, data=data3, family=binomial)
# betaD3 <- coefficients(fitD3)
# 
# tabulkaD3 <- zvektoruj(fitD3, data3)
# jednotky <- rep(1, nrow(tabulkaD3))
# tabulkaD3 <- cbind(jednotky, tabulkaD3)
# 
# PravdepodobnostD3 <- rep(0, nrow(data3))
# RozhodnutieD3 <- rep(-1, nrow(data3))
# data3 <- cbind(PravdepodobnostD3, data3)
# data3 <- cbind(RozhodnutieD3, data3)
# 
# for (i in 1:(nrow(data3))) {
#   data3$PravdepodobnostD3[i] = fi(pravdepodobnost(betaD3, tabulkaD3[i,]))
# }
# require(pROC)
# rocobj <- roc(data3$MIS_Status, data3$PravdepodobnostD3)
# CO3 <- coords(rocobj, x="best", input="threshold", best.method="youden")
# cutoff3 <- as.numeric(CO3[1])
# 
# for (i in 1:(nrow(data3))) {
#   if (data3$PravdepodobnostD3[i] <= cutoff3) {
#     data3$RozhodnutieD3[i] <- 0
#   } else {
#     data3$RozhodnutieD3[i] <- 1
#   }
# }
# 
# TP3 <- 0
# FP3 <- 0
# FN3 <- 0
# TN3 <- 0
# for (i in 1:nrow(data3)) {
#   if(data3$RozhodnutieD3[i] == 1 && data3$MIS_Status[i] == 1) {
#     TP3 = TP3 + 1
#   } else if (data3$RozhodnutieD3[i] == 1 && data3$MIS_Status[i] == 0) {
#     FP3 = FP3 + 1
#   } else if (data3$RozhodnutieD3[i] == 0 && data3$MIS_Status[i] == 1) {
#     FN3 = FN3 + 1
#   } else if (data3$RozhodnutieD3[i] == 0 && data3$MIS_Status[i] == 0) {
#     TN3 = TN3 + 1
#   }
# }
# 
# Senzitivita3 = TP3 / (TP3 + FN3)
# Specificita3 = TN3 / (TN3 + FP3)
# Presnost3 = (TP3 + TN3) / (TP3 + TN3 + FP3 + FN3)
# 
# J3 = Senzitivita3 - (1 - Specificita3)
# 
# Presnost3







testDataD2 <- data2[data2$EndDate < ymd("2010-01-01"),]
testDataD2 <- testDataD2[order(testDataD2$EndDate),]

fitD2 <- glm(MIS_Status ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + Term + NoEmp + NewExist + SBA_Appv + GrAppv + UrbanRural, data=testDataD2, family=binomial)
betaD2 <- coefficients(fitD2)

tabulkaD2 <- zvektoruj(fitD2, testDataD2)
jednotky <- rep(1, nrow(tabulkaD2))
tabulkaD2 <- cbind(jednotky, tabulkaD2)

noveDataD2 <- data2[data2$DisbursementDate >= ymd("2010-01-01"),]
noveDataD2 <- noveDataD2[order(noveDataD2$DisbursementDate),]
netestovacieDataD2 <- data2[data2$EndDate >= ymd("2010-01-01"),]
netestovacieDataD2 <- netestovacieDataD2[order(netestovacieDataD2$EndDate),]

tabulka2D2 <- zvektoruj(fitD2, noveDataD2)
jednotky <- rep(1, nrow(tabulka2D2))
tabulka2D2 <- cbind(jednotky, tabulka2D2)

tabulka4D2 <- zvektoruj(fitD2, netestovacieDataD2)
jednotky <- rep(1, nrow(tabulka4D2))
tabulka4D2 <- cbind(jednotky, tabulka4D2)

Pravdepodobnost2 <- rep(0, nrow(noveDataD2))
Rozhodnutie2 <- rep(-1, nrow(noveDataD2))
noveDataD2 <- cbind(Pravdepodobnost2, noveDataD2)
noveDataD2 <- cbind(Rozhodnutie2, noveDataD2)

Pravdepodobnost2 <- rep(0, nrow(netestovacieDataD2))
netestovacieDataD2 <- cbind(Pravdepodobnost2, netestovacieDataD2)

# Prvý cut-off
Pravdepodobnost2 <- rep(0, nrow(testDataD2))
testDataD2 <- cbind(Pravdepodobnost2, testDataD2)
Rozhodnutie2 <- rep(0, nrow(testDataD2))
testDataD2 <- cbind(Rozhodnutie2, testDataD2)
for (i in 1:(nrow(testDataD2))) {
  testDataD2$Pravdepodobnost2[i] = fi(pravdepodobnost(betaD2, tabulkaD2[i,]))
}
require(pROC)
rocobj <- roc(testDataD2$MIS_Status, testDataD2$Pravdepodobnost2)
CO2 <- coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff2 <- as.numeric(CO2[1])

ST2 = Sys.time()
j = 1
# Pridávanie riadkov, zakaždým sa model aktualizuje (necha ís zhruba 10 minút
for (i in 1:pocetNet) {
  if (j < nrow(noveDataD2)) {
    while (noveDataD2$DisbursementDate[j] <= netestovacieDataD2$EndDate[i]) {
      noveDataD2$Pravdepodobnost2[j] = fi(pravdepodobnost(betaD2, tabulka2D2[j,]))
      if (noveDataD2$Pravdepodobnost2[j] >= cutoff2) {
        noveDataD2$Rozhodnutie2[j] <- 1
      } else {
        noveDataD2$Rozhodnutie2[j] <- 0
      }
      novyRiadokDoTDD2 <- noveDataD2[j,]
      testDataD2 <- rbind(novyRiadokDoTDD2, testDataD2)
      j = j + 1
    }
  }

  fitD2 <- glm(MIS_Status ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + Term + NoEmp + NewExist + SBA_Appv + GrAppv + UrbanRural, data=testDataD2, family=binomial)
  betaD2 <- coefficients(fitD2)

}
ET2 = Sys.time()
D_2 = ET2 - ST2
print(D_2)

# ROC ruène
TP2 <- 0
FP2 <- 0
FN2 <- 0
TN2 <- 0
for (i in 1:nrow(noveDataD2)) {
  if(noveDataD2$Rozhodnutie2[i] == 1 && noveDataD2$MIS_Status[i] == 1) {
    TP2 = TP2 + 1
  } else if (noveDataD2$Rozhodnutie2[i] == 1 && noveDataD2$MIS_Status[i] == 0) {
    FP2 = FP2 + 1
  } else if (noveDataD2$Rozhodnutie2[i] == 0 && noveDataD2$MIS_Status[i] == 1) {
    FN2 = FN2 + 1
  } else if (noveDataD2$Rozhodnutie2[i] == 0 && noveDataD2$MIS_Status[i] == 0) {
    TN2 = TN2 + 1
  }
}

Senzitivita2 = TP2 / (TP2 + FN2)
Specificita2 = TN2 / (TN2 + FP2)
Presnost2 = (TP2 + TN2) / (TP2 + TN2 + FP2 + FN2)

J2 = Senzitivita2 - (1 - Specificita2)

Presnost2


P = 0
for(i in 1:nrow(noveData)) {
  if(noveData$Rozhodnutie[i] == 0 || noveData$Rozhodnutie[i] == 1) {
    P = P + 1
  }
}

LLZak = 0
LL1 = 0
LL2 = 0
for (i in 1:P) {
  LLZak = LLZak + (log(1 - noveData$PravdepodobnostPN[i]))
  LL1 = LL1 + (noveData$Rozhodnutie[i] * log(noveData$PravdepodobnostPN[i]) + (1 - noveData$Rozhodnutie[i])*log(1 - noveData$PravdepodobnostPN[i]))
  LL2 = LL2 + (noveDataD2$Rozhodnutie2[i]*log(noveDataD2$Pravdepodobnost2[i]) + (1 - noveDataD2$Rozhodnutie2[i])*log(1 - noveDataD2$Pravdepodobnost2[i]))
}

DA = (-2) * LL1
DR = (-2) * LL2
chiKvad1 = 2 * (LL1 - LLZak)
chiKvad2 = 2 * (LL2 - LLZak)
chiKvadN = 2 * (LL2 - LL1)

HL1 = ((-2)*LL1)/((-2)*LLZak)
CS1 = 1 - exp((2/P)*(LLZak - LL1))
N1 = CS1 / (1-exp((2*LL1)/(P)))

HL2 = ((-2)*LL2)/((-2)*LLZak)
CS2 = 1 - exp((2/P)*(LLZak - LL2))
N2 = CS2 / (1-exp((2*LL2)/(P)))

HLN = ((-2)*LL2)/((-2)*LL1)
CSN = 1 - exp((2/P)*(LL1 - LL2))
NN = CSN / (1-exp((2*LL2)/(P)))
