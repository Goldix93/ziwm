
#wczytywanie danych z pliku csv
data = read.csv("Documents/ziwm/Indian Liver Patient Dataset (ILPD).csv")

#listowanie podstawowych informacji o danych
summary(data)

#funkcja normalizujaca wartosci w zbiorze danych 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x) ) )
}

#normalizacja danych
data_n <- as.data.frame(lapply(data[,c(1,3,4,5,6,7,8,9)], normalize))

#zbior trenujacy
data_train <- data_n[1:520, ]
#zbior testujacy
data_test <- data_n[521:582, ]
#klasyfikacja zbioru trenujacego
data_train_target <- data[1:520, 11]
#klasyfikacja zbioru testujacego
data_test_target <- data[521:582, 11]

require(class)
#algorytm knn dla k=3
m1 <- knn(train = data_train, test = data_test, cl = data_train_target, k = 3)

#sprawdzenie poprawnosci dzialania algorytmu, x,y = (1,1) i (2,2) = poprawne,
#(1,2) i (2,1) = bledne oszacowania klas
table(data_test_target, m1)





