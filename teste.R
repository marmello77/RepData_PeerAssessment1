#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Remove previous objects
rm(list= ls())


## Loading and preprocessing the data

dados <- read.csv("activity.csv", header = T, na.strings = "NA")
dados$date <- as.Date(dados$date)
head(dados)
str(dados)
summary(dados$steps)


## What is mean total number of steps taken per day?

dia <- aggregate(steps ~ date, dados, sum)
dia

hist(dia$steps,
     main = "Total number of steps taken each day", 
     xlab = "Number of steps", 
     ylab = "Frequency",
     col = "grey", border = "white",
     cex.axis = 1.5, 
     cex.lab = 2, 
     cex.main = 3,
     xaxt="n")
axis(side=1, at=axTicks(1),
     cex.axis = 1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

mean(dia$steps, na.rm = T)
median(dia$steps, na.rm = T)



## What is the average daily activity pattern?

intervalo <- aggregate(steps ~ interval, dados, mean)
head(intervalo)
sum(is.na(intervalo))

plot(intervalo$steps ~ intervalo$interval,
     type = "l",
     lwd = 2,
     main = "Average number of steps taken each interval", 
     xlab = "5-min interval", 
     xlim = c(0,2500),
     ylab = "Average number of steps/day",
     col = "darkgrey",
     cex.axis = 1.5, 
     cex.lab = 2, 
     cex.main = 3,
     xaxt="n")
axis(side=1, at=axTicks(1),
     cex.axis = 1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

intervalo$interval[which.max(intervalo$steps)]


       
## Imputing missing values

sum(is.na(dados))

head(dados)
head(intervalo)

dados2 <- merge(dados, intervalo, by = "interval")
head(dados2)
dados2$steps <- ifelse(is.na(dados2$steps.x), dados2$steps.y, dados2$steps.x)
head(dados2)
dados3 <- subset(dados2, select = c(interval, date, steps))
head(dados3)
sum(is.na(dados3))
dados3$steps <- round(dados3$steps, 0)

dia2 <- aggregate(steps ~ date, dados3, sum)
head(dia2)

hist(dia2$steps,
     main = "Total number of steps taken each day", 
     xlab = "Number of steps", 
     ylab = "Frequency",
     col = "grey", border = "white",
     cex.axis = 1.5, 
     cex.lab = 2, 
     cex.main = 3,
     xaxt="n")
axis(side=1, at=axTicks(1),
     cex.axis = 1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

mean(dia2$steps, na.rm = T)
median(dia2$steps, na.rm = T)



## Are there differences in activity patterns between weekdays and weekends?

weekdays(dados3$date)
dados3$weekdays <- ifelse(weekdays(dados3$date) == c("Domingo", "Sábado"),
                        "weekend", "weekday")
table(dados3$weekdays)

dados3weekday <- subset(dados3, weekdays == "weekday")
head(dados3weekday)

dados3weekend <- subset(dados3, weekdays == "weekend")
head(dados3weekend)

intervalo2weekday <- aggregate(steps ~ interval, dados3weekday, mean)
head(intervalo2weekday)
sum(is.na(intervalo2weekday))

intervalo2weekend <- aggregate(steps ~ interval, dados3weekend, mean)
head(intervalo2weekend)
sum(is.na(intervalo2weekend))

par(mfrow = c(2, 1))

plot(intervalo2weekday$steps ~ intervalo2weekday$interval,
     type = "l",
     lwd = 2,
     main = "Weekdays", 
     xlab = "5-min interval", 
     xlim = c(0,2500),
     ylab = "Average number of steps/day",
     col = "darkgrey",
     cex.axis = 1.5, 
     cex.lab = 2, 
     cex.main = 3,
     xaxt="n")
axis(side=1, at=axTicks(1),
     cex.axis = 1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

plot(intervalo2weekend$steps ~ intervalo2weekend$interval,
     type = "l",
     lwd = 2,
     main = "Weekend", 
     xlab = "5-min interval", 
     xlim = c(0,2500),
     ylab = "Average number of steps/day",
     col = "darkgrey",
     cex.axis = 1.5, 
     cex.lab = 2, 
     cex.main = 3,
     xaxt="n")
axis(side=1, at=axTicks(1),
     cex.axis = 1.5,
     labels=formatC(axTicks(1), format="d", big.mark=','))

par(mfrow=c(1,1))
