#libraries 
library(ggplot2)

#read in the data frame
#this one is just the individual game values:
df <- read.csv('C:\\Users\\joshr\\Downloads\\RawVals.csv', stringsAsFactors = TRUE)

#this next df is the average values and the dates: 
dfBig <- read.csv('C:\\Users\\joshr\\Downloads\\Blitz.csv', stringsAsFactors = TRUE)

#the date is being brought in as a factor and NOT a date, which will become important later
dfBig$Date <- as.Date(dfBig$Date, format = "%m/%d/%y")

#df[order(dfBig$Date), ]

#get some summary statistics
summary(df)

#store the values in a variable, yolo: 
x <- df$data

#store the values of mean and median, why not? 
dfMean <- mean(x)
dfMedian <- median(x)

#let's take a look at some visuals
boxplot(df$data)

#histogram stuff
hist <- hist(df$data, col = "red", xlab = "Average Centipawn Loss", main = "Average Centipawn Loss")
xfit <- seq(min(x), max(x), length = 87)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(hist$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)

#Kernel Density stuff
d <- density(x)
plot(d, main = "Kernel Density Plot of Average Centipawn Loss")
polygon(d, col = "red", border = "blue")

#correlation stuff 
#let's try to find a correlation between rating and average/median centipawn loss: 
#see if there's any relationship between rating and average centipawn loss (mean/Median)
cor.test(dfBig$Rating, dfBig$ACL.Mean)
cor.test(dfBig$Rating, dfBig$ACL.Median)


cor.test(dfBig$Rating, dfBig$Win.)



#let's plot some of the information that we have
plot(dfBig$Rating, dfBig$ACL.Mean, xlab = 'Rating', ylab = 'Average Centipawn Loss', main = 'Rating Vs Avg Centipawn Loss')

plot(dfBig$Rating, dfBig$ACL.Median, xlab = 'Rating', ylab = 'Median Centipawn Loss', main = 'Rating Vs Median Centipawn Loss')

#Let's do a timeseries with the Rating
#timeseries = ts(dfBig, by = day, start = as.Date('7/6/22', format="%m/%d/%y"), end = as.Date('7/28/22', format="%m/%d/%y"))
# daySeq = seq(as.Date("2022-07-06"), as.Date("2022-07-28"), by = "day")
# timeseries = ts(dfBig$Rating, frequency = 365)
# plot.ts(timeseries)

plot.ts(dfBig$Date, dfBig$Rating)

ggplot(dfBig, aes(Date, Rating)) + geom_point()
