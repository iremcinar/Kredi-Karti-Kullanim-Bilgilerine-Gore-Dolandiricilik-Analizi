getwd()
library(readxl)
dataset <- read_excel("~/Desktop/FinalProje/veri.xlsx") #dataset ismiyle veriyi okuttuk.
View(dataset) #dataset'i görüntüledik.

colnames(dataset)<- c('custId','bakiye','bakiyeFrekansi','satinAlma','tekSeferlikAlim','taksitliAlim','nakitAvans',
                  'satinAlimFrekansi','tekSeferlikAlimFrekansi','taksitliAlimFrekansi','nakitAvansFrekansi','nakiAvansIslem',
                  'satinAlmaIslem','kKLimit','Odeme','MinOdeme','tamOdemeYuzdesi','kKHizmetSuresi','CLASS') #sütun isimlerini değiştirdik


#Histogram
ggplot(dataset, aes(x=CLASS)) + geom_histogram(binwidth = 0.2, colour="green") #CLASS verisinin histogramı.


ggplot(dataset, aes(x=bakiye)) + geom_histogram(binwidth = 0.5, colour="black")
ggplot(dataset, aes(x= kKLimit)) + geom_histogram(binwidth = 0.5, colour="black")

hist(dataset$kKLimit, col="blue", main="Kart Limitine Göre Satin Alma", xlab = "kKLimit", ylab = "satinAlmaIslem")
hist(dataset$kKLimit, col="purple", main="Kart Limitine Göre Bakiye", xlab = "kKLimit", ylab = "bakiye")

#Kredi kartı bakiyesine göre bakiye durumu
library(ggplot2)
ggplot(data = dataset) + 
  geom_point(mapping = aes(x = kKLimit, y = bakiye ), color = "orange")
#Kredi kartı limitine göre tam ödeme yüzdeleri
ggplot(data = dataset) + geom_smooth(mapping = aes(x = tamOdemeYuzdesi, y = kKLimit)) 
#Kredi kartı limitine göre satın alma 
ggplot(data = dataset) + geom_smooth(mapping = aes(x = kKLimit, y = satinAlma )) 
ggplot(data = dataset, mapping = aes(x = kKLimit, y = bakiye)) + geom_point()

#Veri Önişlemler----
head(dataset)
summary(dataset)
dataset <- dataset[!is.na(dataset$MINIMUM_PAYMENTS),] #summary de çıkan boş verileri sildik.
dataset <- dataset[!is.na(dataset$CREDIT_LIMIT),] #summary de çıkan boş verileri sildik.
#install.packages("Amelia")
library(Amelia)
missmap(dataset)#boş veri var mı görsel ile kontrol ettik.
summary(veriseti)
dataset <- unique(dataset, incomparables = FALSE) #yenilenen satılar kaldırıldı.
duplicated(dataset) #tekrar eden verileri kontrol ettik
duplicatedDataSet <- dataset[7635:8636,] #max değere ulaşıldığı için kalan verileri duplicatedDataSet değişkenine atayarak kontrol ettik.
duplicated(duplicatedDataSet)
dataset <- dataset[2:19] #verisetinden 
veriseti <- veriseti[2:19]

#Normalizasyon----
min_max <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

data_norm <- as.data.frame(apply(dataset[, 2:18], 2, function(x) (x - min(x))/(max(x)-min(x))))
str(data_norm)
summary(data_norm)#normalize verileri inceledik.

#Görselleştirme----
#install.packages("ggplot2")
#install.packages("psych")
library(ggplot2)
library(psych)
library(ggplot2)
describe(data_norm) #Verilerin açıklayıcı istatistiklerine ulaştık(sd,vars,mean,median,min,max,vs)

#Pasta Grafiği- Fraud İşlemlerin Yasal İşlemlere Oranı
labels <- c("YASAL", "FRAUD")
labels <- paste(labels, round(100*prop.table(table(dataset$CLASS)), 2)) #CLASS verisini yüzdelik olarak ifade edeşlim.
labels
pie(table(dataset$CLASS), labels,col= c("yellow", "grey"),
    main = "Fraud İşlemlerin Yasal İşlemlere Oranı" ) 
#Histogram
ggplot(dataset, aes(x=CLASS)) + geom_histogram(binwidth = 0.2, colour="green") #CLASS verisinin histogramı.


ggplot(dataset, aes(x=bakiye)) + geom_histogram(binwidth = 0.5, colour="black")
ggplot(dataset, aes(x= kKLimit)) + geom_histogram(binwidth = 0.5, colour="black")

hist(dataset$kKLimit, col="blue", main="Kart Limitine Göre Satin Alma", xlab = "kKLimit", ylab = "satinAlmaIslem")
hist(dataset$kKLimit, col="purple", main="Kart Limitine Göre Bakiye", xlab = "kKLimit", ylab = "bakiye")

#Kredi kartı bakiyesine göre bakiye durumu
ggplot(data = dataset) + 
  geom_point(mapping = aes(x = kKLimit, y = bakiye ), color = "orange")
#Kredi kartı limitine göre tam ödeme yüzdeleri
ggplot(data = dataset) + geom_smooth(mapping = aes(x = tamOdemeYuzdesi, y = kKLimit)) 
#Kredi kartı limitine göre satın alma 
ggplot(data = dataset) + geom_smooth(mapping = aes(x = kKLimit, y = satinAlma )) 
ggplot(data = dataset, mapping = aes(x = kKLimit, y = bakiye)) + geom_point()


#Korelasyon----
#install.packages("corrplot")
library(corrplot)
korelasyon <- cor(dataset[,c(1,3,4,5,6,11,12,13,14,15)])
corrplot(korelasyon, method = "circle", bg = "gray",title = "korelasyon") #ilişki yoğunluklarını gösterir.

# Nitelikler arasindaki iliskilerin incelenmesi
pairs(~CLASS+kKLimit+satinAlmaIslem+tekSeferlikAlim+taksitliAlim+nakitAvans, data=dataset)
plot(dataset[,2:5])

#Regresyon----
library(nnet)
mymodel <- multinom(CLASS ~., data=veriseti)
p <- predict(mymodel,veriseti)
tab <- table(p, veriseti$CLASS)
tab
table(veriseti$CLASS)
sum(diag(tab))/sum(tab) #doğrusınıflandırmaoranınıbulduk
1-sum(diag(tab))/sum(tab)#yanlışsınıflandırmaoranınıbulduk

#SONUÇ 0.51 oranında doğru bir sınıflandırma gerçekleşti

#Model Performane E.
install.packages("ROCR")
library(ROCR)
pred <- predict(mymodel, veriseti, type='prob')
#Veri setimizdeki ilk 6 verinin tahminini gördük
head(pred) 

#Olasılıklar eğer 0.5 in üzerindeyse tahmin 1(YASADISI ISLEM)dir.
pred <- prediction(pred,veriseti$CLASS)
eval <- performance(pred, "acc")

#ACCURACY Oranını Grafikte görmek istersek;
acc <- performance(pred,"acc")
par(mar = rep(2, 4)) 
plot(acc)
abline(h=0.518, v=0.50) #(doğruluk) değerinin en yüksek olduğu (tepe) noktayı gösterdik
#Lojistik Regresyon----
# Bağımlı değişkenimiz kategorik olduğu için lojistik regresyon tercih ettik
#install.packages("caret")
regresyon_data <- dataset
regresyon_data$tekSeferlikAlimFrekansi <- as.integer(regresyon_data$tekSeferlikAlimFrekansi)
regresyon_data$taksitliAlimFrekansi <- as.integer(regresyon_data$taksitliAlimFrekansi)
regresyon_data$nakitAvansFrekansi <- as.integer(regresyon_data$nakitAvansFrekansi)
regresyon_data$nakiAvansIslem <- as.integer(regresyon_data$nakiAvansIslem)
regresyon_data$satinAlmaIslem <- as.integer(regresyon_data$satinAlmaIslem)
regresyon_data$tamOdemeYuzdesi <- as.integer(regresyon_data$tamOdemeYuzdesi)
regresyon_data$kKHizmetSuresi <- as.factor(regresyon_data$kKHizmetSuresi)
regresyon_data$bakiye <- as.integer(regresyon_data$bakiye)
regresyon_data$bakiyeFrekansi <- as.integer(regresyon_data$bakiyeFrekansi)
regresyon_data$satinAlma <- as.integer(regresyon_data$satinAlma)
regresyon_data$tekSeferlikAlim <- as.integer(regresyon_data$tekSeferlikAlim)
regresyon_data$taksitliAlim <- as.integer(regresyon_data$taksitliAlim)
regresyon_data$nakitAvans <- as.integer(regresyon_data$nakitAvans)
regresyon_data$satinAlimFrekansi <- as.integer(regresyon_data$satinAlimFrekansi)
regresyon_data$CLASS<- as.factor(regresyon_data$CLASS)
#Eğitim ve test verilerini oluşturalım
library(caret)
set.seed(1)
egitimIndisleri <- createDataPartition(y=regresyon_data$CLASS, p = 0.70, list = FALSE) 
egitim_regresyon<- regresyon_data[egitimIndisleri,]
test_regresyon <- regresyon_data[-egitimIndisleri,]
test_hedef_regresyon <- test_regresyon$CLASS
View(egitim_regresyon)

model_regresyon <- glm(CLASS~., data = egitim_regresyon, family = binomial) #modeli oluşturduk
summary(model_regresyon) #modelin özetini aldık. P değeri 0.05 den küçük olanlar, istatistiksel olarak anlamlıdır.
model
#Performansı nasıl? Doğruluk oranını kontrol edelim.
olasilik_regresyon <- predict(model_regresyon, newdata = test_regresyon, type = "response") 
head(olasilik_regresyon) #ilk 6 değişkenin modele göre olasılıkları

#kurduğumuz modeli kullanarak  test verisindeki bağımlı 
#değişkeni(CLASS) tahmin edeceğiz ve gerçek değerlerle karşılaştıracağız.
tahmin_regresyon <- ifelse(olasilik_regresyon>0.5, "Fraud", "Yasal")
head(tahmin_regresyon)

#olasılık değerlerine göre bağımlı değişkeni tahmin edelim.
head(tahmin_regresyon)
mean(tahmin_regresyon==test_regresyon$CLASS)
# Test
olasilik_regresyon_egitim <- predict(model_regresyon, newdata = egitim_regresyon, type = "response")
head(olasilik_regresyon)

tahmin_regresyon_egitim <- ifelse(olasilik_regresyon_egitim>0.5, "Fraud", "Yasal")
head(tahmin_regresyon)
mean(tahmin_regresyon_egitim==egitim_regresyon$CLASS)

test_hedef_regresyon

(tablom_regresyon <- table (tahmin_regresyon, test_regresyon$CLASS, dnn = c("Tahmin",
                                                                            "Gercek ")))
(tp_regresyon <- tablom_regresyon[1])
(fp_regresyon  <- tablom_regresyon[3])
(fn_regresyon  <- tablom_regresyon[2])
(tn_regresyon <- tablom_regresyon[4])
paste0 ("Dogruluk = ",(dogruluk_regresyon  <- (tp_regresyon +tn_regresyon)/sum(tablom_regresyon)))
paste0("Hata = ",(hata_regresyon<- 1-dogruluk_regresyon ))
paste0("TPR = ",(TPR_regresyon<- tp_regresyon / (tp_regresyon +fn_regresyon)))
paste0("SPC = ",(SPC_regresyon<- tn_regresyon / (fp_regresyon +tn_regresyon)))
paste0("PPV = ",(PPV_regresyon<- tp_regresyon / (tp_regresyon +fp_regresyon)))
paste0("NPV = ",(NPV_regresyon<- tn_regresyon / (tn_regresyon +fn_regresyon)))
paste0("FPR = ",(FPR_regresyon<- fp_regresyon / sum(tablom)))
paste0("FNR = ",(FNR_regresyon<- fn_regresyon / (fn_regresyon +tp_regresyon)))
paste0("LR_p = ",(LR_p_regresyon<- TPR_regresyon / FPR_regresyon))
paste0("LR_n = ",(LR_n_regresyon<- FNR_regresyon / SPC_regresyon))
paste0("DOR = ",(DOR_regresyon<- tp_regresyon / LR_p_regresyon/LR_n_regresyon))
paste0("F_Measure = ",(F_Measure_regresyon<- (2*PPV_regresyon*TPR_regresyon)/(PPV_regresyon+TPR_regresyon )))




#NAVIEBAYES Algoritması----
library(readxl)
veriseti <- data.frame(read_excel("~/Desktop/FinalProje/veri.xlsx"))

veriseti <- veriseti[!is.na(veriseti$MINIMUM_PAYMENTS),] #summary de çıkan boş verileri sildik.
veriseti <- veriseti[!is.na(veriseti$CREDIT_LIMIT),] #summary de çıkan boş verileri sildik.
#Sütun isimleri değiştirildi
colnames(veriseti)<- c('custId','bakiye','bakiyeFrekansi','satinAlma','tekSeferlikAlim',
                       'taksitliAlim','nakitAvans','satinAlimFrekansi','tekSeferlikAlimFrekansi',
                       'taksitliAlimFrekansi','nakitAvansFrekansi','nakiAvansIslem','satinAlmaIslem',
                       'kKLimit','Odeme','MinOdeme','tamOdemeYuzdesi','kKHizmetSuresi','CLASS')
View(veriseti)
#CLASS (0-1) değişkeni için karakter ataması yapıldı.
veriseti$CLASS <- ifelse(veriseti$CLASS>0.5,"Fraud","Yasal")
View(veriseti)

#CLASS değişkeni faktöre çevrildi,diğer değişkenler nümerik olarak kaldı.
numeric_columns <- c(2:18)
for(i in 1:ncol(veriseti)){
  if(i %in% numeric_columns)
    veriseti[,i] <- as.numeric(veriseti[,i])
  else
    veriseti[,i] <- as.factor(veriseti[,i])
}

summary(veriseti)
#Kategorik değişken dışındaki veriler(CUST_ID) verisetine atandı.
veriseti <- veriseti[,c(2:19)]


#Verilerimizletahmingerçekleştirebilmekiçinverisetinieğitimvetestdatasıolarakbölüyoruz
#install.packages("caret")
library(caret)
set.seed(1)
my_indexes <- createDataPartition(y = veriseti$CLASS, p = .70, list = FALSE)
training <- as.data.frame(veriseti[my_indexes,])
test <- as.data.frame(veriseti[-my_indexes,])

table(veriseti$CLASS) #Gerçek verisetimideki CLASS değerler
table(training$CLASS) #Eğitim verisetimideki CLASS değerler
table(test$CLASS)     #Test verisetimideki CLASS değerler


#NAIVEBAYES ALGORITMASI için "e1071" paketini yüklüyoruz
#install.packages("e1071")
library(e1071)
naiveB_model <- naiveBayes(training[,1:17], training[[18]])
naiveB_model 
#Her bir değişkeni kendi içerisinde 2 alt kategoriye bölerek YASAL ve YASADISI işlemlerin bağımsız değişkenler üzerineki tahmini gerçekleştirildi


# ModelTahminleriniOluşturalım;
(nb_predictions <- predict(naiveB_model, test[,1:17]))
(nb_probs <- predict(naiveB_model, test[,1:17], "raw", ))

#TahminOlasılıklarınıyüzdelikolarakgörmekistersek;
paste(round(100*nb_probs,2), "%")

#TahminOlasılıklarınıGrafikteGörmekİstersek;
barplot(as.matrix(nb_probs), xlab = ("CLASS"), main = "Barplot comparison of Transactions", ylab = "Number of Transactions")
barplot(apply(nb_probs, 2, FUN = sum), col = c("red", "blue"),xlab = "CLASS", ylab = "Number of Transactions", main = "Barplot comparison of Transactions", cex.main = 1)

#GERÇEK DEĞERLER,TAHMİN DEĞERLERİ VE OLASILIKLARI BİR ARADA GÖRMEK İSTERSEK;
results <- data.frame(test[[18]], nb_predictions, nb_probs)

#Naive Bayes CLASS tahmini ve gerçek CLASS değerlerini bir arada gördük
(my_table <- table(nb_predictions, test[[18]], dnn = c("Predictions", "Actual/Reference")))

#PEKİ MODELİMİZİN BAŞARISI NEDİR?
#MODEL PERFORMANCE EV.
# (tp <- ) = 1169
# (fp <- ) = 1129
# (fn <- ) = 140
# (tn <- ) = 152

#acc  <- 1321/2590=0.51 
 doğruluk  <- 0.51 
#1-ACC=0,49(ERR(HATAORANI))
hata_navi<- 0.49
# sens <- 1169/1309=0,89
# spec <- 152/1281=0,12
# prec-PPV <- 1169/2298=0,51
# npv  <- 152/292=0,52
#f-measure <- (2*0,51*0,89)/(0,51+0,89)=0,65

#VERİSETİMİZDEKİ İLK 6 GÖZLEM İLE MODELPERFORMANSÖLÇÜMÜ
head(veriseti)

#KNN Algoritması----
summary(veriseti)
#install.packages("ggplot2")
library(ggplot2)
plot(cars)
par(mfrow=c(2,2))
plot(cars)
dev.off()

boxplot(veriseti$CLASS ~ veriseti$bakiye,col="blue",main="bakiye ve dolandırıcılık",
        xlab="bakiye", ylab="CLASS")
hist(veriseti$CLASS, col="red", main="CLASS")

#KrediKartıLimiti, TekSeferlikAlımFrekasnı ve Satın Alım'ın karşılaştırılması
pairs(~ kKLimit + tekSeferlikAlimFrekansi + satinAlma, data=veriseti )

#Veri setimizde yer alan bütün değişkenleri bir arada görelim
#install.packages("funModeling")
library(funModeling)
plot_num(veriseti)


#PERFORMANS DEĞERLENDİRME olarak Hold-out kullanılmıştır
#install.packages("caret")
library(caret)
set.seed(1)
#Verimizin %80'nini eğitim amaçlı ayırdık
egitimIndisleri <- createDataPartition(y =  veriseti$CLASS, p = .80, list = FALSE)


#VERİ SETİ EGİTİM VE TEST VERİ SETİ OLMAK ÜZERE İKİYE AYRILIR

egitim <- veriseti[egitimIndisleri,]
test <- veriseti[-egitimIndisleri,]

#Eğitim ve Test veri setlerinde tahminde kullanılacak nitelikler ve hedef nitelikler ayrı nesnelere atanrı
#Veri setini 18.sütunu hedef nitelik olan ClASS sütunudur.

testNitelikleri <- test[, -18]
testHedefNitelik <- test[[18]]
egitimNitelikleri <- egitim [, -18]
egitimHedefNitelik <- egitim [[18]]

#rastgele 3 olarak seçilmiştir.
k_degeri=3

#KNN icin "class" paketini yüklüyoruz
#install.packages("class")
library(class)
set.seed(1)
#knn için Öklid uzaklığı kullanılmıştır
(tahminiSiniflar=knn(egitimNitelikleri, testNitelikleri,egitimHedefNitelik,
                     k= k_degeri)) #Test veri seti olarak ayrılan örnekler için elde edilen tahmini sınıf değerleri 


(tablom <- table (tahminiSiniflar,
                  testHedefNitelik, dnn = c("Tahmini Siniflar",
                                            "Gercek Siniflar")))

#Model performansını değerlendirdik

(tp <- tablom[1])
(fp <- tablom[3])
(fn <- tablom[2])
(tn <- tablom[4])


paste0 ("Dogruluk = ",(dogruluk <- (tp+tn)/sum(tablom)))
paste0("Hata = ",(hata <- 1-dogruluk))
paste0("TPR = ",(TPR <- tp/(tp+fn)))
paste0("SPC = ",(SPC <- tn/(fp+tn)))
paste0("PPV = ",(PPV <- tp/(tp+fp)))
paste0("NPV = ",(NPV <- tn/(tn+fn)))
paste0("FPR = ",(FPR <- fp/sum(tablom)))
paste0("FNR = ",(FNR <- fn/(fn+tp)))
paste0("LR_p = ",(LR_p <- TPR/FPR))
paste0("LR_n = ",(LR_n <- FNR/SPC))
paste0("DOR = ",(DOR <- tp/LR_p/LR_n))
paste0("F_Measure = ",(F_Measure <- (2*PPV*TPR)/(PPV+TPR)))

#C4.5 algoritması----
#install.packages("RWeka")
#install.packages("FSelector")
#install.packages("party")
#install.packages("partykit")
library(caret)
set.seed(1)
egitimIndisc45 <- createDataPartition(y =regresyon_data$CLASS, p = .70, list = FALSE) 
egitim_c45 <- regresyon_data[egitimIndisc45,]
test_c45 <- regresyon_data[-egitimIndisc45,]
test_hedef_c45 <- test_c45$CLASS

library(RWeka)
str(egitim_c45)

C45_modeli <- J48(CLASS ~. ,  data=egitim_c45)
C45_modeli <- J48(CLASS ~., data=egitim_c45, control = Weka_control(R = F, M = 6))
print(summary(C45_modeli))
print(C45_modeli)#ağaçları inceleyelim

# Attribute Importance
InfoGainAttributeEval(CLASS ~., data=egitim_c45)
GainRatioAttributeEval(CLASS ~., data=egitim_c45)
library(FSelector)
information.gain(CLASS ~., data=egitim_c45)
gain.ratio(CLASS ~., data=egitim_c45)



library("party")
library("partykit")
plot(C45_modeli)#c45 görseli


# Finding Predictions of The Model
C45_predictions <- predict(C45_modeli, newdata = test_c45[,-18])

# Performance Evaluation
confusionMatrix(data = C45_predictions, reference = test_c45[[18]], dnn = c("Predictions", "Actual/Reference"), mode = "everything")



# prediction for new data, assume that newdata is selected from training data set instead of giving new data really
i <- 5
(newdata_predictions <- predict(C45_modeli, newdata = regresyon_data[i,18]))
data[i,7]


(tablom_c45 <- table (C45_predictions, test_c45$CLASS, dnn = c("Tahmin","Gercek ")))
(tp_c45 <- tablom_c45[1])
(fp_c45  <- tablom_c45[3])
(fn_c45  <- tablom_c45[2])
(tn_c45 <- tablom_c45[4])
paste0 ("Dogruluk = ",(dogruluk_c45  <- (tp_c45 +tn_c45)/sum(tablom_c45)))
paste0("Hata = ",(hata_c45<- 1-dogruluk_c45 ))
paste0("TPR = ",(TPR_c45<- tp_c45 / (tp_c45 +fn_c45)))
paste0("SPC = ",(SPC_c45<- tn_c45 / (fp_c45 +tn_c45)))
paste0("PPV = ",(PPV_c45<- tp_c45 / (tp_c45 +fp_c45)))
paste0("NPV = ",(NPV_c45<- tn_c45 / (tn_c45 +fn_c45)))
paste0("FPR = ",(FPR_c45<- fp_c45 / sum(tablom)))
paste0("FNR = ",(FNR_c45<- fn_c45 / (fn_c45 +tp_c45)))
paste0("LR_p = ",(LR_p_c45<- TPR_c45 / FPR_c45))
paste0("LR_n = ",(LR_n_c45<- FNR_c45 / SPC_c45))
paste0("DOR = ",(DOR_c45<- tp_c45 / LR_p_c45/LR_n_c45))
paste0("F_Measure = ",(F_Measure_c45<- (2*PPV_c45*TPR_c45)/(PPV_c45+TPR_c45 )))

