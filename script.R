require(class)
#http://archive.ics.uci.edu/ml/datasets/ILPD+%28Indian+Liver+Patient+Dataset%29#
#wczytywanie danych z pliku csv
data = read.csv("Documents/ziwm/Indian Liver Patient Dataset (ILPD).csv")

#listowanie podstawowych informacji o danych
summary(data)

#usuwanie niepelnych danych
data <- na.omit(data)

#zmiana nazw kolumn
#1. Age - Age of the patient
#2. Gender - Gender of the patient
#3. TB - Total Bilirubin
#4. DB - Direct Bilirubin
#5. Alkphos - Alkaline Phosphotase
#6. Sgpt - Alamine Aminotransferase
#7. Sgot - Aspartate Aminotransferase
#8. TP - Total Protiens
#9. ALB - Albumin
#10. A/G Ratio - Albumin and Globulin Ratio
#11. Selector - field used to split the data into two sets (labeled by the experts) 
names(data)<-c("Age","Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "ALB", "A/G Ratio", "Selector")

#funkcja normalizujaca wartosci w zbiorze danych 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x) ) )
}

set.seed(1234)

for(i in 1:5){
  
  rnd <- runif(nrow(data))
  data <- data[order(rnd),]

  #normalizacja danych
  data_n <- as.data.frame(lapply(data[,c(1,3,4,5,6,7,8,9,10)], normalize))

  #zbior trenujacy
  data_n_train <- data_n[1:515, ]
  #zbior testujacy
  data_n_test <- data_n[516:578, ]
  #klasyfikacja zbioru trenujacego
  data_train_target <- data[1:515, 11]
  #klasyfikacja zbioru testujacego
  data_test_target <- data[516:578, 11]
  
  #zbior trenujacy bez normalizacji
  data_train <- data[1:515, c(1,3,4,5,6,7,8,9,10)]
  #zbior testujacy bez normalizacji
  data_test <- data[516:578, c(1,3,4,5,6,7,8,9,10)]
  
  
  #---------------knn-------------------
  
  
  #algorytm knn dla k=3 normalizacja
  m1 <- knn(train = data_n_train, test = data_n_test, cl = data_train_target, k = 25)
  
  #algorytm knn dla k=3 bez normalizacji
  m2 <- knn(train = data_train, test = data_test, cl = data_train_target, k = 25)
  
  #sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
  #(1,2) i (2,1) = bledna klasyfikacja
  print(table(data_test_target, m1))
  
  print(table(data_test_target, m2))
  
}
#knn rozne miary odleglosci
require(knnGarden)

#Euclidean
m5 <- knnVCN(TrnX = data_n_train, OrigTrnG = data_train_target, TstX = data_n_test, K = 25, ShowObs = FALSE, method = "euclidean", p = 2)

m5 <- m5[,1]
table(data_test_target, m5)

#Manhattan
m6 <- knnVCN(TrnX = data_n_train, OrigTrnG = data_train_target, TstX = data_n_test, K = 25, ShowObs = FALSE, method = "manhattan", p = 2)

m6 <- m6[,1]
table(data_test_target, m6)

#---------------nm----------------

require(klaR)
#bez normalizacji
m3 <- nm(Selector ~ ., data = data[, c(1,3,4,5,6,7,8,9,10,11)])

m3$grouping <- factor(1:2)

m3$lev <- as.character((1:2))

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledna klasyfikacja
table(data$Selector,predict(m3)$class)

#z normalizacja
data_n$Selector <- data[,"Selector"]

m4 <- nm(Selector ~ ., data = data_n)

m4$grouping <- factor(1:2)

m4$lev <- as.character((1:2))

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledna klasyfikacja
table(data$Selector,predict(m4)$class)

