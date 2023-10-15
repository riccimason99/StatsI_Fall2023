##QUESTION 1##


#PART A

# This is the quick way to do it in R, I will use this to check my answers
# after I calculate it by hand. 
mat <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3)
chi_mat <- chisq.test(mat)
chi_mat

#The total sum is 42, this will be the used in every formula

#first I need to find the expected value for each observation 
#if the variables are independent.
(27/42)*21. 
#then I have to plug the values into the formula which is 
#the sum of all (observed-expected)^2/expected. 
(14-13.5)^2/13.5
#0.01851852
#I will find each value then add them all together at the end

(15/42)*21
(7-7.5)^2/7.5
#0.03333333

(27/42)*13
(6-8.36)^2/8.36
# 0.6662201

(15/42)*13
(7-4.64)^2/4.64
# 1.200345

(27/42)*8
(7-5.14)^2/5.14
#0.6730739

(15/42)*8
(1-2.86)^2/2.86
#1.20965

#Now to add them all together and get the Chi Squared statistic 
0.01851852 + 0.03333333 +  0.6662201 + 1.200345 + 0.6730739 + 1.20965
#3.80


#PART B

#Now to calculate the P-Value
#Degrees of Freedom = (number of rows -1)(number of columns - 1)
#Whenever we conduct a Chi Squared Test we will only look at the upper tail.
pchisq(3.7912, df = 2, lower.tail = FALSE)
# P-Value = 0.1495

# By using this function I can find the accurate Chi Squared Value (3.7912) 
# and P-Value (0.1502). When preforming the equation by hand I rounded the values and 
# there for my results were not exact.
chimat <- chisq.test(mat)
print(chi_mat)

#If alpha =.01 We fail to reject the NULL that the two variables are
#independent considering that our P-Value (0.1502) > alpha (0.01)

#In other words, we do not find evidence to support the alternate hypothesis that there 
# is a coloration between driver class and solicitation for a bribe. 

# We can also make inferences by comparing our chi squared statistic to the critical value

#This is the critical value
qchisq(0.1, df=2, lower.tail = FALSE)
#4.60517

#Since the chi squared statistic (3.80) is less than the critical value
# (4.60517) we fail to reject the Null.

#PART C
chi_mat <- chisq.test(mat)
print(chi_mat)

# This is the formula for standard residuals
# (observed - expected) / sqrt(expected)

#These functions show residuals and standard residuals respectively. 
chi_mat$residuals
standard_residuals <- round(chimat$stdres,3)
print(standard_residuals)



##PART D

# You can use the residuals to look at the variable that are further from being independent
# smaller values are closer to being independent.

# You can also see by if your observed value is greater than or less than the 
# what we would expect 

# In this case the standard residuals seem to mirror each other with the first column being the same
# as the lower but with the opposite sign (negative or positive). As the category of one goes up the other goes down
# this suggests a negitive association between the variables.
plot(standard_residuals)




#################################################################################################

#Question 2




# PART B

#Load the data into R
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#Take an initial look at the data and plot the variables of interest
View(data)
plot(data$reserved,data$water)

#I subset the two variables we are interested in.
relevant_data <- data[c("reserved","water")]
print(relevant_data)

#This function gives a litany of useful information such as 
# P-Value and coefficient estimate
sum <- summary(lm(data$water~data$reserved))
print(sum)
sum$coefficients


#Test Statistic and critical value
cor.test(data$water, data$reserved)
critical_value <- qt(1-.05/2,df=320)
critical_value


#This is the corrolation coeficcent it will tell you the strength of corrolation 
confint(sum, conf.level = 0.95)
