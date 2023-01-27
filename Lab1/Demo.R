days <- c('Mon', 'Tues', 'Wed', 'Thurs','Fri','Sat','Sun') #days
temp <- c(28,30.5, 32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','T')

help("data.frame")
RPI_Weather_Week <- data.frame(days, temp,snowed)
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
