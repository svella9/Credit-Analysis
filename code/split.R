data <- read.csv('cleaned_data.csv',TRUE,sep = ',')
indexes <- sample(1:nrow(data), size = 0.5 * nrow(data))
test <- data[indexes,]
train <- data[-indexes,]

write.csv(test, file = 'Test500.csv', row.names = FALSE)
write.csv(train, file = 'Train500.csv', row.names = FALSE)


