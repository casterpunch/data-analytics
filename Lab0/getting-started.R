install.packages("MASS") #installing the MASS package
library(MASS) #load the library MASS
attach(Boston) #attaching the dataset
help("Boston")
head(Boston) #show the head of the dataset (First 6 rows or can specify)
dim(Boston) #dimensions of the dataset Rows, Cols
names(Boston) #column names
str(Boston) #str function shows the structure of the dataframe
nrow(Boston) #num rows
ncol(Boston) #num cols
summary(Boston) #shows summary statistics
summary(Boston$crim)
print(Boston$crim)


        