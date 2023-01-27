data_2010EPI <-read.csv(file.choose(),header = T)
names(data_2010EPI) <- as.matrix(data_2010EPI[1, ])
data_2010EPI <- data_2010EPI[-1, ]
data_2010EPI[] <- lapply(data_2010EPI, function(x) 
  type.convert(as.character(x)))
data_2010EPI
View(data_2010EPI)
shapiro.test(data_2010EPI$EPI)
EPI <- data_2010EPI$EPI
summary(EPI)
boxplot(EPI)
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95., 1.0), prob=TRUE)
lines (density(EPI,na.rm=TRUE,bw=1.))
rug(EPI) #histograms are unsatisfying
DALY <- data_2010EPI$DALY
scatter(EPI)
Population07 <- data_2010EPI$Population07
plot(Population07,EPI)