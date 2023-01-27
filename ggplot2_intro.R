library(ggplot2)
wt <- mtcars$wt
mpg <- mtcars$mpg
plot(wt,mpg)
qplot(wt,mpg)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

#PLOTS GG
lines(pressure$temperature,pressure$pressure/2, col = "red")
lines(pressure$temperature,pressure$pressure/2, col = "blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature,y=pressure)) + geom_line() + geom_point() #line with points

#BAR GG
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
#BAR GRAPH OF COUNTS
qplot(factor(cyl),data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar() #gg 

#HIST GG
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)

#BOXPLOT GG
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
gplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot() #equivalent to above

ggplot(ToothGrowth, aes(x = interaction(supp,dose), y = len)) + geom_boxplot() #interaction to compare 3 vectors
