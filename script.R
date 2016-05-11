
#wczytywanie danych z pliku csv
data = read.csv("Documents/ziwm/Indian Liver Patient Dataset (ILPD).csv")

#listowanie podstawowych informacji o danych
summary(data)

#usuwanie niepelnych danych
data <- na.omit(data)


#funkcja normalizujaca wartosci w zbiorze danych 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x) ) )
}

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

require(class)
#algorytm knn dla k=3 normalizacja
m1 <- knn(train = data_n_train, test = data_n_test, cl = data_train_target, k = 25)

#algorytm knn dla k=3 bez normalizacji
m2 <- knn(train = data_train, test = data_test, cl = data_train_target, k = 25)

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledna klasyfikacja
table(data_test_target, m1)

table(data_test_target, m2)


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
m3 <- nm(X1 ~ ., data = data[, c(1,3,4,5,6,7,8,9,10,11)])

m3$grouping <- factor(1:2)

m3$lev <- as.character((1:2))

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledna klasyfikacja
table(data$X1,predict(m3)$class)

#z normalizacja
data_n$X1 <- data[,"X1"]

m4 <- nm(X1 ~ ., data = data_n)

m4$grouping <- factor(1:2)

m4$lev <- as.character((1:2))

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledna klasyfikacja
table(data$X1,predict(m4)$class)

