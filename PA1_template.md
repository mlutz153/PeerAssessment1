---
title: "PeerAssessment1"
author: "Matt Lutz"
date: "Tuesday, March 10, 2015"
output: html_document
---


```{r firstchunk, echo = TRUE}
require(lubridate)
require(plyr)
require(dplyr)
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "repdata-data-activity.zip", mode="wb")
data <- unzip("repdata-data-activity.zip")
data <- read.csv(data, colClasses = c("numeric", "POSIXct", "numeric"))
data <- mutate(data, hour = interval %/% 100, minute = interval %% 100)  


a <- filter(data, hour < 10)
a[,4] <- paste0("0", a[,4])
b <- filter(data, hour > 9)
g <- rbind(a,b)
e <- filter(g, minute < 10)
e[,5] <-paste0("0", e[,5])
d <- filter(g, minute > 5)

c <- rbind(e,d)



c <- mutate(c, seconds = 00)
c <- mutate(c, time = paste(hour,minute, seconds, sep = ":"))
c <- mutate(c, dateandtime = paste(date, time, sep = " "))
c[,8] <- ymd_hms(c[,8])
data <- c 
stepsum <- aggregate(steps ~ date, data = data, sum)
```

##Histogram of total steps per day:
```{r HISTOGRAM, fig.height = 4, echo = TRUE}
hist(stepsum$steps, main = "Freq of Steps",xlab = "steps")

```

##Average steps per day:
```{r avgstepsperdayv}
stepmean <- aggregate(steps ~ date, data = data, mean)
head(stepmean,53)
```
##Median steps per day:
```{r medianstepsperday}

stepmedian <- aggregate(steps ~ date, data = data, median)
head(stepmedian,53)
```

```{r secondchunk, echo = FALSE, fig.height = 4, echo = TRUE}
stepintervalmean <- aggregate(steps ~ minute, data = data, mean)
colnames(stepintervalmean)[2]<- c("avgSteps")
plot(stepintervalmean$minute, stepintervalmean$avgSteps, main = "Avg Steps by Time Interval", xlab = "time interval(min)", ylab = "avgSteps", type = "l" )
stepintervalmean <- arrange(stepintervalmean, desc(avgSteps))
head(stepintervalmean,1)

```
##Number of missing values:
```{r numberofnas, echo = TRUE}
sum(is.na(data$steps))
```
##Missing Values histogram
```{r missingvalues, echo = TRUE}
combo <- merge(data, stepintervalmean)
combo$steps[is.na(combo$steps)] <- combo$avgSteps[is.na(combo$steps)]
combostepsum <- aggregate(steps ~ date, data = combo, sum)
colnames(combostepsum) <- c("date", "totalsteps")
hist(combostepsum$totalsteps, main = "Freq of Steps (NA's AVG'd per interval)", xlab = "steps per day (NAs AVG'd per interval)")
```
##Mean and median difference with NA's avg'd per interval:
```{r meanNasAvgd, echo = TRUE}
combostepmean <- aggregate(steps ~ date, data = combo, mean)
colnames(combostepmean) <- c("date", "avgStepsNoNAs")
stepmean <- aggregate(steps ~ date, data = data, mean, na.action = na.pass)
stepmean[is.na(stepmean)] <- 0
colnames(stepmean) <- c("date", "avgSteps")
combo2 <- merge(combostepmean,stepmean, by = "date")
combo2 <- mutate(combo2, "meandifference" = abs(avgStepsNoNAs- avgSteps))


combostepmedian <- aggregate(steps ~ date, data = combo, median)
colnames(combostepmedian) <- c("date", "medianStepsNoNAs")
stepmedian <- aggregate(steps ~ date, data = data, median, na.action = na.pass)
stepmedian[is.na(stepmedian)] <- 0
colnames(stepmedian) <- c("date", "medianSteps")
combo3 <- merge(combostepmedian, stepmedian)
combo3 <- mutate(combo3, "mediandifference" = abs(medianStepsNoNAs- medianSteps))

combo4 <- merge(combo2, combo3)

head(combo4, 53)

```
## WEEKEND VS WEEKDAY
```{r weekdays, fig.height = 4, echo = TRUE}
require(lubridate)
combo3 <- mutate(combo, "weekdays" = weekdays(ymd(date)))
require(data.table)
combo3 <- data.table(combo3)
wkdays <- filter(combo3, weekdays != "Saturday")
wkdays <- filter(wkdays, weekdays != "Sunday")
wkdays <- aggregate(steps ~ minute, data = wkdays, mean)
wkdays <- mutate(wkdays, Day = "Weekday")
wkends <- filter(combo3, weekdays != "Monday")
wkends <- filter(wkends, weekdays != "Tuesday")
wkends <- filter(wkends, weekdays != "Wednesday")
wkends <- filter(wkends, weekdays != "Thursday")
wkends <- filter(wkends, weekdays != "Friday")
wkends <- aggregate(steps ~ minute, data = wkends, mean)
wkends <- mutate(wkends, Day = "Weekend")
DOW <- rbind(wkdays,wkends)
require(ggplot2)
g <-ggplot(DOW, aes(x=minute, y=steps, group = Day,colour = Day, main = "Weekday vs Weekend", label = Day))
g <- g+geom_line()
g <- g+ylim(30,60)
g



```
