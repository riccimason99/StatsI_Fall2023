setwd("/Users/riccimason99/GitHub/StatsI_Fall2023/problemSets/PS01/template")



###QUESTION ONE###

#data
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

###PART 1###

# First step, look at data
View(y)
head(y)
str(y)

#To find the CI one must have the sample mean +/- t-distribution * standard error
#Mean = the sum of all data points/number of values. 98.44
mean(y)

#Standard error- to find standard error we first need to find the variance and then standard deviation
#To get the Variance we must find the sum of all squared differences (ie.the difference of 
#each data point from the mean squared and added together) Divide by n-1. In this case it is 171.4233
var(y)

var_y <- var(y)

#Then, to get the Standard Deviation one must sqrt(variance)  13.09287
sd_y<-sd(y)

sd_y_n <-sqrt(var_y)

#Now that we have Standard Deviation we can calculate Standard Error with the formula  
#SE=sd/sqrt(n) it comes out to 2.618575
se_y<-sd(y)/sqrt(length(y))
se_y
#Fnially we must find the t-statistc, we use t instead of z/normal 
#because we have a small sample size (<30) 
#First we must find degrees of freedom (n-1=24) 
#We used .95 because it is two tailed.
qt(.95,df=(length(y)-1))

#Now to find the confidence interval. The formula is mean(y) plus/minus t_score*se 
#(98.44 +/- 1.317 * 2.618)
lower_ci <- mean(y) - qt(.95, df = (length(y) - 1)) * se_y
upper_ci <- mean(y) + qt(.95, df = (length(y) - 1)) * se_y
                
lower_ci
upper_ci

#or use 
t.test(y, conf.level = 0.95, alternative = "two.sided")

#Confidence Interval is 93.95-102.92
#We can say with 90% certinity that the average IQ of students as school y lies between these two values




####PAART 2###
#The Alternate Hypotheses is: Students from school y will on average have IQ scores > to the average
#IQ score of students from all other schools in the county.

#The Null Hypotheses is: Students from school y will on average have IQ scores < or = to the average 
#IQ score of students from all other schools in the county.

#In this part we can use the same mean, standard deviation,
#considering this test will be preformed with 95% confidence (alpha-1) we will have a different t-stat.

qt(.975,df=(length(y)-1))
#t-stat is 2.063899
#We used .975 because a=.05
qt(.025,df=(length(y)-1))

#T-Stat is mean of sample-population mean/standard error
t_stat<-(mean(y)-100)/se_y

t_stat
#Since the t-stat is less than the critical value we fail to reject the Null that the IQ of students
#at school y is <=

t_value<-t.test(y, conf.level = 0.95, alternative = "greater")

sd(y)
t_value
p-value<-pt(t_value, df=n-1, "lower.tail" = FALSE)

######QUESTION 2#######

#Step 1 Look at data
View(exp)
head(exp)
str(exp) 
exp <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
typeof(expenditure)
str(expenditure)
View(expenditure)

###PART ONE###plot Data

plot(exp$X1,exp$Y,
     main="Income vs Ammount spent on Homelessness ",
     xlab="Income per Capita",
     ylab="Ammount Spent per Capita")
plot(exp$X2,exp$Y,
     main="Financially Insecure Residents vs Ammount spent on Homelessness",
     xlab="Financially Insecure Residents, per 100,000",
     ylab="Ammount Spent per Capita")
plot(exp$X3,exp$Y,
     main="Pop Living in Urban Areas vs Ammount spent on Homelessness ",
     xlab="Pop Living in Urban Areas per 1,000",
     ylab="Ammount Spent per Capita")
### This is where I left off
plot(exp$X1,exp$X2,
     main="Income vs Financially Insecure Residents ",
     xlab="Income per Capita",
     ylab="Financially Insecure Residents, per 100,000")
plot(exp$X1,exp$X3,
     main="Income vs Pop Living in Urban Areas",
     xlab="Income per Capita",
     ylab="Pop Living in Urban Areas per 1,000")
plot(exp$X2,exp$X3,
     main="Financially Insecure Residents vs Pop Living in Urban Areas",
     xlab="Financially Insecure Residents, per 100,000",
     ylab="Pop Living in Urban Areas per 1,000")

###PART 2###
pdf(file="plot.pdf")
plot(exp$Region,exp$Y, 
     main="Financially Insecure Residents vs Pop Living in Urban Areas",
     xlab="Region",
     ylab="Ammount Spent per Capita")
dev.off()
###PART 3###

