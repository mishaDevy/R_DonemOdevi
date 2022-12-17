#Veri yükleme
veri <- read.csv("AKSEN.IS.csv")
veri

#Kapanis fiyatinin tanimlanmasi
kapanisfiyat <- veri$Close
kapanisfiyat

#Ortalama hesaplama
mean(kapanisfiyat)

#Medyan bulunmasi
median(kapanisfiyat)

#Modun bulunmasi
modfiyat <- table(kapanisfiyat)
names(modfiyat)[which(modfiyat == max(modfiyat))]

#Çeyriklik ve yüzdeliklerin bulunmasi
quantile(kapanisfiyat)
quantile(kapanisfiyat, probs= c(10,25,75,90)/100)

#Varyans
var(kapanisfiyat)
#Standart Sapma
sd(kapanisfiyat)
#Standart Hata
std_hata <- sd(kapanisfiyat)/sqrt(length(kapanisfiyat))

#Çarpıklık Katsayısı
install.packages("moments")
library(moments)
skewness(kapanisfiyat)

#Basıklık Katsayısı
kurtosis(kapanisfiyat)

#Bowley'in Asimetri Ölçüsü - Pearson'ın asimetri ölçüsü
#Bowley
q1_fiyat <- 39
q2_fiyat <- 42.910
q3_fiyat <- 48.035
bow_fiyat <- (q3_fiyat+q1_fiyat-(2*q2_fiyat))/(q3_fiyat-q1_fiyat)
bow_fiyat
#Pearson
pear_fiyat <- 3*(mean(kapanisfiyat)-median(kapanisfiyat))/sd(kapanisfiyat)
pear_fiyat

#Histogram Grafiği
hist(kapanisfiyat, main="AKSEN Son 60 Günün Kapaniş Fiyatlarının Histogramı", xlab="Kapanis fiyati", ylab="Frekans")

#Frekans Tablosuna göre Birikimli Sıklık Grafiği
f <- c(4,6,4,8,10,10,12,6)
plot(cumsum(f),type="o",main="Birikimli Sıklık Grafiği",xlab="Kapanis fiyati",ylab="Birikimli Sıklık")

#Trend Grafiği
plot(ts(kapanisfiyat), type="o",main="AKSEN Son 60 Günün Kapaniş Fiyatlarının Trend Grafiği", xlab="Gün", ylab="Kapanis Fiyati")

#Boxplot Grafiği
boxplot(kapanisfiyat, col="pink")


