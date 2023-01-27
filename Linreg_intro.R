library(magrittr)

multivariate <- read.csv("C:/Users/xiaoj6/Documents/GitHub/data-analytics/multivariate.csv")
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm
head(multivariate)
summary(mm)$coef
plot(Homeowners~Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)

newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients