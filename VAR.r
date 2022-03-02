wig <- read.csv("~/Desktop/VAR2/wig.csv") #Polska - PLN 
cac <- read.csv("~/Desktop/VAR2/cac.csv") #Francja - EUR
fmib <- read.csv("~/Desktop/VAR2/fmib.csv") #Włochy - EUR
ose <- read.csv("~/Desktop/VAR2/ose.csv") #Norwegia - NOK
ibex <- read.csv("~/Desktop/VAR2/ibex.csv") #Hiszpania - EUR


#Wszystkie indeksy wyrazono w EUR

plneur <- read.csv("~/Desktop/VAR2/plneur.csv")
nokeur <- read.csv("~/Desktop/VAR2/nokeur.csv")

#Roznice w liczbie obserwacji dla pobranych zbiorow - przedstawiono ramke danych tylko z datami wspolnymi dla wszystkich zbiorow

library(dplyr)

dates <- data.frame(matrix(NA, ncol=8))

dates <- inner_join(wig,cac,by="Data")[,c(1,5,10)]
dates <- inner_join(dates,fmib,by="Data")[,c(1,2,3,7)]
dates <- inner_join(dates,ose,by="Data")[,c(1,2,3,4,8)]
dates <- inner_join(dates,ibex,by="Data")[,c(1,2,3,4,5,9)]

dates <- inner_join(dates,plneur,by="Data")[,c(1,2,3,4,5,6,10)]
dates <- inner_join(dates,nokeur,by="Data")[,c(1,2,3,4,5,6,7,11)]

colnames(dates) <- c("Data","wig","cac","fmib","ose","ibex","plneur","nokeur")

#Przewalutowano indeksy wyrazone w PLN i NOK na EUR

dates$wig <- dates$wig*dates$plneur
dates$ose <- dates$ose*dates$nokeur

#Ramka danych - logarytmiczne stopy zwrotu

data <- dates[,1:6]

returnrates <- data.frame(data[-1,1])
for (j in 2:ncol(data)){
  x <- c()
  for (i in 2:nrow(data)){
    x[i-1] <- log(data[i,j])/(data[i-1,j])
  }
  returnrates <- cbind(returnrates,x)
}
colnames(returnrates) <- colnames(data)

returnrates[1:10,]

#Badanie stacjonarnosci - test adf

library(aTSA)

adf_wig <- adf.test(returnrates$wig,nlag=5)
adf_cac <- adf.test(returnrates$cac,nlag=5)
adf_fmib <- adf.test(returnrates$fmib,nlag=5)
adf_ose <- adf.test(returnrates$ose,nlag=5)
adf_ibex <- adf.test(returnrates$ibex,nlag=5)

adf_wig$type1[,3] || adf_wig$type2[,3] || adf_wig$type3[,3]  < 0.05
adf_cac$type1[,3] || adf_cac$type2[,3] || adf_cac$type3[,3]  < 0.05
adf_fmib$type1[,3] || adf_fmib$type2[,3] || adf_fmib$type3[,3]  < 0.05
adf_ose$type1[,3] || adf_ose$type2[,3] || adf_ose$type3[,3]  < 0.05
adf_ibex$type1[,3] || adf_ibex$type2[,3] || adf_ibex$type3[,3]  < 0.05

#Wybór opoznienia z minimalna wartoscia kryterium informacyjnego

returnrates <- returnrates[,-1]

library(vars)

lags <- VARselect(returnrates,lag.max=5)
lags <- lags$criteria

lags <- data.frame(which.min(lags[1,]),which.min(lags[2,]),which.min(lags[3,]),which.min(lags[4,]))
colnames(lags) <- c("AIC","HQ","SC","FPE")
rownames(lags) <- "Lag"

#Rzad opoznienia rowny 2 lub 5 - budujemy dwa modele i sprawdzamy, ktory jest lepszy

model1 <- VAR(returnrates,p=2)
model2 <- VAR(returnrates,p=5)

residuals1 <- resid(model1)
residuals2 <- resid(model2)

#Weryfikacja modeli - za pomoca korelogramow sprawdzamy, czy istnieje autokorelacja reszt

library(corrgram)

correlogram_wig <- acf(residuals1[,1])
correlogram_cac <- acf(residuals1[,2])
correlogram_fmib <- acf(residuals1[,3])
correlogram_ose <- acf(residuals1[,4])
correlogram_ibex <- acf(residuals1[,5])

plot(correlogram_wig[1:10],ci=0.99, main="Korelogram dla reszt równania I - (wig)") 
plot(correlogram_cac[1:10],ci=0.99, main="Korelogram dla reszt równania II - (cac)") 
plot(correlogram_fmib[1:10],ci=0.99, main="Korelogram dla reszt równania III - (fmib)") 
plot(correlogram_ose[1:10],ci=0.99, main="Korelogram dla reszt równania IV - (ose)") 
plot(correlogram_ibex[1:10],ci=0.99, main="Korelogram dla reszt równania V - (ibex)") 

correlogram_wig <- acf(residuals2[,1])
correlogram_cac <- acf(residuals2[,2])
correlogram_fmib <- acf(residuals2[,3])
correlogram_ose <- acf(residuals2[,4])
correlogram_ibex <- acf(residuals2[,5])

plot(correlogram_wig[1:10],ci=0.99, main="Korelogram dla reszt równania I - (wig)") 
plot(correlogram_cac[1:10],ci=0.99, main="Korelogram dla reszt równania II - (cac)") 
plot(correlogram_fmib[1:10],ci=0.99, main="Korelogram dla reszt równania III - (fmib)") 
plot(correlogram_ose[1:10],ci=0.99, main="Korelogram dla reszt równania IV - (ose)") 
plot(correlogram_ibex[1:10],ci=0.99, main="Korelogram dla reszt równania V - (ibex)") 

#Zdecydowanie lepiej wypada model z opoznieniami rzedu 5, jednak wystepuje autokorelacja reszt rownania
#1 dla opoznienia rownego 9, jednak jest to bardzo niewielka wartosc, zatem uznano ten model za poprawny, 
#bedzie on stanowil przedmiot do dalszej analizy

#Istotnosc parametrow modelu

summary(model2)

#Badanie przyczynowosci w sensie Grangera

library(lmtest)

#Przyczynowosc badanej gieldy na wszystkie 4 pozostale gieldy
granger1 <- causality(model2, cause="wig")$Granger
granger2 <- causality(model2, cause="cac")$Granger
granger3 <- causality(model2, cause="fmib")$Granger
granger4 <- causality(model2, cause="ose")$Granger
granger5 <- causality(model2, cause="ibex")$Granger

granger <- data.frame(granger1$p.value,granger2$p.value,granger3$p.value,granger4$p.value,granger5$p.value)
colnames(granger) <- c("wig","cac","fmib","ose","ibex")
rownames(granger) <- "p-value"





