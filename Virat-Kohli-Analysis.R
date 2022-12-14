# read the dataset
runs <- read.csv("~/Downloads/VKruns.csv")


# perform one way anova
result <- aov(Runs~Year, data = runs)
ans <- summary(res)
print(ans)
print(result)

# Overall statistics for the 4 year period
sprintf("No of innings - %d",length(runs$Runs))
sprintf("Total Runs - %d",sum(runs$Runs))
sprintf("Average - %f",mean(runs$Runs))

# Now do the year by year analysis

# store data of 2016 by taking the subset
# calculate avg and sd for that year
runs2016 <- subset(runs,Year == 2016, select = c("Runs","Year"))
avg2016 <- mean(runs2016$Runs)
sd2016 <- sd(runs2016$Runs)

# store data of 2017 by taking the subset
# calculate avg and sd for that year
runs2017 <- subset(runs,Year == 2017, select = c("Runs","Year"))
avg2017 <- mean(runs2017$Runs)
sd2017 <- sd(runs2017$Runs)

# store data of 2018 by taking the subset
# calculate avg and sd for that year
runs2018 <- subset(runs,Year == 2018, select = c("Runs","Year"))
avg2018 <- mean(runs2018$Runs)
sd2018 <- sd(runs2018$Runs)

# store data of 2019 by taking the subset
# calculate avg and sd for that year
runs2019 <- subset(runs,Year == 2019, select = c("Runs","Year"))
avg2019 <- mean(runs2019$Runs)
sd2019 <- sd(runs2019$Runs)

# tabulate the information with the help of a data frame
# columns are - year, no. of innings, no. of runs, average and the standard deviation
data <- data.frame(c("2016","2017","2018","2019"),
                   c(length(runs2016$Runs),length(runs2017$Runs),length(runs2018$Runs),length(runs2019$Runs)),
                   c(sum(runs2016$Runs),sum(runs2017$Runs),sum(runs2018$Runs),sum(runs2019$Runs)),
                   c(avg2016,avg2017,avg2018,avg2019),
                   c(sd2016,sd2017,sd2018,sd2019))

# name the columns of the data frame
colnames(data) <- c("Year","Innings","Runs","Avg","Std Dev")
print(data)

# construct a boxplot for each year
boxplot(runs2016$Runs,runs2017$Runs,runs2018$Runs,runs2019$Runs,col = "Blue",xlab = "Year"
        ,ylab = "Runs",names = unique(runs$Year),pch = 16)

# plot a side by side bar plot using the ggplot library
library(ggplot2)

# prepare the right dataframe
avg <- c(avg2016,avg2017,avg2018,avg2019)
sd <- c(sd2016,sd2017,sd2018,sd2019)


measures <- rep(c("Average","Standard Deviation"),4)
year<- c(2016,2016,2017,2017,2018,2018,2019,2019)
values <- c(avg2016,sd2016,avg2017,sd2017,avg2018,sd2018,avg2019,sd2019)
comp <- data.frame(year,measures,values)
print(comp)


# plot the bar chart using the ggplot function
# if you explicitly say stat = "identity" in geom_bar() 
# you're telling ggplot2 to skip the aggregation and that you'll provide the y values. 

ggplot(comp,aes(fill = measures,y = values,x = year)) +
geom_bar(position="dodge",stat="identity") +
ggtitle("Comparison of Average and Standard Deviation") 


# innings by innings bar plots for each year

# 2016 
barplot(runs2016$Runs,yaxp=c(0, 250, 10),ylim = c(0,250),
        main = "Virat Kohli : Innings by Innings - 2016",ylab = "Runs",col = "Dark Blue")

# 2017
barplot(runs2017$Runs,yaxp=c(0, 250, 10),ylim = c(0,250),
        main = "Virat Kohli : Innings by Innings - 2017",ylab = "Runs",col = "Dark Blue")

# 2018
barplot(runs2018$Runs,yaxp=c(0, 250, 10),ylim = c(0,250),
        main = "Virat Kohli : Innings by Innings - 2018",ylab = "Runs",col = "Dark Blue")

# 2019
barplot(runs2019$Runs,yaxp=c(0, 250, 10),ylim = c(0,250),
        main = "Virat Kohli : Innings by Innings - 2019",ylab = "Runs",col = "Dark Blue")