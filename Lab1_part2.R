### PACKAGES ###

install.packages("dplyr")
install.packages("magrittr")
library('magrittr')
library("dplyr")
library('ggplot2')

### MAIN ###


multivariate <- read.csv(file.choose(), header = TRUE)
head(multivariate)
attach(multivariate)

mm <- lm(Homeowners~Immigrant)
mm

summary(mm)$coef

plot(Homeowners~Immigrant)
abline(mm)
abline(mm, col = 2, lwd = 3)

newimmigrantdata <- data.frame(Immigrant = c(0,20))
mm %>% predict(newimmigrantdata)

abline(mm)
abline(mm, col = 3, lwd = 3) # col = color, lwd = line width

attributes(mm)
mm$coefficients

### GGPLOT ###
plot(mtcars$wt, mtcars$mpg)

qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)

ggplot(mtcars, aes(x=wt, y=mpg))+ geom_point()

plot(pressure$temperature, pressure$pressure, type = 'l')
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")

qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")

ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()

### BAR GRAPHS ###
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

### HISTOGRAMS ###
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)

### BOXPLOTS ###
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

