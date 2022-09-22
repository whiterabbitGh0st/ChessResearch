#libraries 
install.packages("ggplot2")
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
abline(v = 57.57) #mean
abline(v = 54) #median

#Kernel Density stuff
d <- density(x)
plot(d, main = "Kernel Density Plot of Average Centipawn Loss")
polygon(d, col = "red", border = "blue")
#just want to show the means and medians on this density plot:
abline(v = 57.57) #mean
abline(v = 54) #median

#correlation stuff 
#let's try to find a correlation between rating and average/median centipawn loss: 
#see if there's any relationship between rating and average centipawn loss (mean/Median)
cor.test(dfBig$Rating, dfBig$ACL.Mean)
#9.22.22 - no correlation, although there aren't enough samples just yet.

cor.test(dfBig$Rating, dfBig$ACL.Median)
#9.22.22 - no correlation, although there aren't enough samples just yet.

#determine if there is any correlation with rating and win percentage
cor.test(dfBig$Rating, dfBig$Win.)
#9.22.22 - Decent correlation (.48), but there aren't enough samples yet.

#test to see if there is any correlation with my play and my win Percentages: 
cor.test(dfBig$Win., dfBig$ACL.Mean)
#9.22.22 - slight negative correlation (.36) indicating that the lower the ACL mean, the higher Win %, which is logical.

cor.test(dfBig$Win., dfBig$ACL.Median)
#9.22.22 - Slight negative correlation (.32) (see above)

#let's plot some of the information that we have
plot(dfBig$Rating, dfBig$ACL.Mean, xlab = 'Rating', ylab = 'Average Centipawn Loss', main = 'Rating Vs Avg Centipawn Loss')

plot(dfBig$Rating, dfBig$ACL.Median, xlab = 'Rating', ylab = 'Median Centipawn Loss', main = 'Rating Vs Median Centipawn Loss')

#Let's do a timeseries with the Rating
#timeseries = ts(dfBig, by = day, start = as.Date('7/6/22', format="%m/%d/%y"), end = as.Date('7/28/22', format="%m/%d/%y"))
# daySeq = seq(as.Date("2022-07-06"), as.Date("2022-07-28"), by = "day")
# timeseries = ts(dfBig$Rating, frequency = 365)
# plot.ts(timeseries)

plot.ts(dfBig$Date, dfBig$Rating)

#let's use ggplot to plot the rating changes over time: 
#first let's change the theme so we aren't using that gross greyscale shit: 
theme_set(theme_bw())
ggplot(dfBig, aes(Date, Rating, color = Rating)) + 
  geom_point() + 
  labs(x = "Date", y = "Rating (Δ)") + 
  ggtitle("Rating Change Over Time") + 
  #scale_color_gradient2(midpoint = 0)
  scale_color_gradient2(midpoint = 0, low = "blue", high = "red")


RColorBrewer::display.brewer.all()