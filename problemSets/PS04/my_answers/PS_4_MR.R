install.packages("car")
library("car")
data(Prestige)
help(Prestige)
View(Prestige)

####### QUESTION 1 #########

                                  # A)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
View(Prestige)
## Drop any rows with NA although it doesn't really matter.
Prestige_no_na <- na.omit(Prestige)
View(Prestige_no_na)


                                  # B)
model_B <- lm(prestige ~ income + professional + income:professional , data = Prestige_no_na)
summary(model_B)



                                  # C)
# Y = 21.1422589 + 0.0031709*x1 + 37.7812800*x2 +  * -0.0023257(x1 * x2)
# Y = Predicted Value of Prestige, x1 = Income amount in dollars, for x2 1 = professional 0 = unprofessional



                                  # D)
# For non-professionals the estimated expected effect of a one dollar increase
# of "income" on prestige is 0.0031709.

# For professionals the estimated expected effect of a one dollar increase
# of "income" on prestige is 0.0008452. 
# 0.0031709(coefficient for income) - 0.0023257(interaction term) = 0.0008452

# There is a positive relationship between income and prestige.


                                  # E)
# When income is held constant, the estimated expected effect being "professional"
# compared to being "non-professional" is an increase of 37.7812800 in prestige score.


                                  # F)
21.1422589 + 0.0031709 * 1000 + 37.7812800 * 1 + -0.0023257 * (1000*1)
21.1422589 + 0.0031709 * 2000 + 37.7812800 * 1 + -0.0023257 * (2000*1) # $1,000 increase in income 
60.61394-59.76874 # = 0.8452

21.1422589 + 0.0031709 * 1001000 + 37.7812800 * 1 + -0.0023257 * (1001000*1)
21.1422589 + 0.0031709 * 1002000 + 37.7812800 * 1 + -0.0023257 * (1002000*1)
905.8139-904.9687 == 0.8452
  
0.0031709 * 1000 -0.0023257 * (1000*1)
# = 0.8452

21.1422589 + 0.0031709 * 0 + 37.7812800 * 1 + -0.0023257 * (0*1)
21.1422589 + 0.0031709 * 1000 + 37.7812800 * 1 + -0.0023257 * (1000*1) # $1,000 increase in income 
59.76874-58.92354 #0.8452


# For professionals, the estimated marginal affect of prestige when income 
# increases by $1,000 is an increase of 0.8452 in prestige score. 
# The above equation displays an increase from $1,000 to $2,000 but the affect on prestige would
# be the same for any increase of $1000 ex.$0 to $1000 or $1,500 to $2,500 ect



                                    # G)
21.1422589 + 0.0031709 * 6000 + 37.7812800 * 0  + -0.0023257 * (6000*0) # non-professional 
21.1422589 + 0.0031709 * 6000 + 37.7812800 * 1  + -0.0023257 * (6000*1) # Professional 
63.99474 - 40.16766
# 23.82708
# The estimated marginal of prestige between non-professional and professional
# when income held constant at $6,000 is an increase of 23.82708 in prestige score.



###################################################################################################


####### QUESTION 2 #########

                                    # A)

    # NULL and ALTERNATE HYPOTHESIS
# Ho: beta1 = 0 (slope equals zero)
# The estimated affect of having yard signs in the precinct on vote share = 0
# Ha: beta1 != 0 (slope does not equals zero)
# The estimated affect of having yard signs in the precinct on vote share does not = 0


t_stat_a <- 0.042/0.016 # # Test Statistic: coefficient/sd
df <- 128 # n - k - 1 (number of observations - number of variables - 1)
two_a <- 2 * pt(t_stat_a, 128, lower.tail = FALSE) 
# We want to know if yard signs affect vote share in any way (positive or negative) hence, lower.tail = FALSE
two_a
# p- value = 0.00972002

#   INTERPRETATION
# Since the p value of 0.00972002 is smaller than the significance level of alpha = .05 we
# can reject the null hypothesis that the estimated affect of having yard signs in the precinct
# on vote share = 0. We find evidence to support the alternate 
# hypothesis that the estimated affect of having yard signs in the precinct on vote share does not = 0


#crit_val <- qt(.95/2,df)
#print(crit_val)
?qt()


                                    # B)

  # NULL and ALTERNATE HYPOTHESIS
# Ho: beta1 = 0 (slope = 0)
# Ho: The estimated affect of vote share for a precinct adjacent to a precinct which displayed yard
# signs is 0.
# Ha: beta1 != 0 (slope does not = 0)
# Ha: The estimated affect of vote share for a precinct adjacent to a precinct which displayed yard 
# signs does not equal 0.

t_stat_b <- 0.042/0.013 # Test Statistic 
two_b <- 2 * pt(t_stat_b, df, lower.tail = FALSE )
# p-value = 0.00156946

#   INTERPRETATION
# Since the p-value of 0.00156946 is lower than the significance level, alpha = .05, we can reject
# the Null Hypothesis that the estimated effect of vote share 
# in a precinct adjacent to a precinct which displayed yard signs is 0.
# We find evidence to support the alternate hypothesis that the the estimated effect of vote 
# share of a precinct adjacent to a precinct which displayed yard signs does not equal 0.

                                      # C)
# The coefficient for the constant term 0.302 is the estimated vote share for a precinct where no 
# lawn signs were displayed and no adjacent precincts displayed lawn signs. 


                                      # D)
# 9.4% of the variability of voteshare can be explained by the model. There is much unexplained 
# variability indicating that there may be covariates which were not included. The model could fit better
# if more relevant variables were included.

##### Kellstead and Whitten 190-194
