### PACKAGES ###
install.packages('readxl')
library('readxl')



### MAIN ###

days <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
help("data.frame")

RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5, c('days', 'temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset = snowed == TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
empty.dataframe <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
v2
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

write.csv(df, file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2
######## USE EXCEL

psth <- '/Users/jackmoncur23/Downloads/EPI_data.xls'
sheetn <- 'EPI2010_onlyEPIcountries'

EPI_data <- read_excel(psth, sheet = sheetn, col_names = TRUE)
colnames(EPI_data) <- EPI_data[1,]
EPI_data <- EPI_data[-1,]
View(EPI_data)

attach(EPI_data)
EPI_data$EPI <- as.numeric(EPI_data$EPI)
EPI <- EPI_data$EPI

tf <- is.na(EPI)
E <- EPI[!tf]

summary(EPI)
fivenum(EPI, na.rm = TRUE)  
stem(EPI)  
hist(EPI)  
hist(EPI, seq(30., 95., 1.0), prob = TRUE)  
lines(density(EPI, na.rm = TRUE, bw = "SJ"))  
rug(EPI)  

plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)  
par(pty = "s")  
qqnorm(EPI);qqline(EPI)  
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5),x, xlab = 'Q-Q Plot for T dsn')
qqline(x)

#EXERCISE 1
EPI_data$DALY <- as.numeric(EPI_data$DALY)
daly <- EPI_data$DALY

summary(daly)
fivenum(daly, na.rm = TRUE)
stem(daly)
hist(daly)
rug(daly)

# Fitting a distribution beyond histograms
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
par(pty = 's')
qqnorm(EPI); qqline(EPI)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(daly), do.points = FALSE, verticals = TRUE)
par(pty= 's')
qqnorm(daly);qqline(daly)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for Daly")
qqline(x)

plot(ecdf(WATER_H), do.points = FALSE, verticals = TRUE)
par(pty='s')
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for WATER_H")

boxplot(EPI, daly)
qqplot(EPI, daly)


boxplot(EPI_data[,c("EPI", "DALY", "ENVHEALTH", "ECOSYSTEM", "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIVERSITY")])

#Exercise 2
help("distributions")

EPILand <- EPI[!Landlock,]
Eland <- EPILand[!is.na(EPILand)]



















