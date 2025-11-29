library(HSAUR3)

data("CHFLS")

head(CHFLS)
str(CHFLS)

#============================================================================================================
#(a) Creat a new data frame called df1 that only includes R_age, R_happy and R_region. Check whether
#the new data frame has missing data. Explore the distribution of R_age for the different regions. Do
#boxplots of age as a function of region and comment on what you observe. Calculate mean, standard
#deviation, median and interquartile range for R_age for each of the six regions and comment.

df1 <- CHFLS[, c("R_age", "R_happy", "R_region")] #extract only the relevant columns
str(df1)
sum(is.na(df1))  #check if there are any mising values, is.na returns a same sized matrix with 1 if empty and 0 if not. sum sums it up


par(mar = c(8, 4, 4, 2))
boxplot(R_age ~ R_region, data = df1,
        main = "Distribution of Age by Region",
        xlab = "Region", ylab = "Age",
        col = "lightblue",
        las = 2)


mean <- tapply(df1$R_age, df1$R_region, mean)
sd <- tapply(df1$R_age, df1$R_region, sd)
median <- tapply(df1$R_age, df1$R_region, median)
IQR <- tapply(df1$R_age, df1$R_region, IQR)
head(mean)
head(sd)
head(median)
head(IQR)

#Looking at the boxplot we see that all regions have a relatively similar range of ages between the minmum nd maximum.
#We also see that the the coastal east has an older average of ages relative to coastal south.

#and upon further investigation using the mean, sd, median, and IQR we see that the values are pretty similar with a median range of 35 to 41 and a mean range of 36,05 to 41.06
#the standard deviation and IQR values are  also pretty close.



#============================================================================================================
#(b) Create a new ordered factor in df1 named R_a by dividing R_age into five groups having approximately
#the same number of subjects. Name the levels a1, a2, a3, a4, and a5.

q <- quantile(df1$R_age, probs = seq(0, 1, length.out = 6))
q


df1$R_a <- cut(df1$R_age,
               breaks = q,
               labels = c("a1", "a2", "a3", "a4", "a5"),
               ordered = TRUE)

table(df1$R_a)

#I split the ages using the quantile points to have a roughly equal amount of entires in each "aN"
#they are not equal because some ages are much larger and all entries of that age add to one bin.

#============================================================================================================
#(c) Produce a table of R_region against R_a (age should be in the columns of the table). Graph a mosaic
#plot of this table using different colors for the rectangles. Comment on what you observe. Produce a
#second table with proportions relative to the regions. Comment.

table1 <- table(df1$R_region, df1$R_a)
table1


mosaicplot(table1,
           main = "Age Group Distribution by Region",
           xlab = "Region", ylab = "Age Group (R_a)",
           color = c("lightblue", "lightgreen", "pink", "orange", "violet"))


#from the mosaic plot we see that some regions have much less entires (width of the rectangles)
#also we see that the each region has a different age distribution, for example coastal south has about 50% of its population on age groups a1 and a2.
#while coastal east has about the same percentage but lying in groups a4 and a5
table2 <- prop.table(table1, 1)
table2


#the proportion table shows the same observations I talked about in the mosaic plot interpretation, but the proportions are shown much clearer with exact floats.


#============================================================================================================
#(d)You want to determine whether the age groups have a homogeneous distribution across regions. Which
#test (or tests) do you know that can be used for this? What are the underlying assumptions? Are
#they satisfied in this case? Carry out all the tests that apply and discuss the results. What are your
#conclusions?


#since we want to essentially check categorical independence, i believe the Chi-squared test is most appropriate
#Chi-squared is more appropriate than fisher's test since we have a very large amount of data.

#Assumptions: the data are categorical, the observations are independent, the sample is randomly selected, and there is an 
#adequate sample size with expected cell counts not being too small (at least more than 5 is enough)

#H0: distributuion of age groups is the same across all regions
#H1: age distribution differs depending on region.

chi_result <- chisq.test(table1)
chi_result

#given that the p-value is < 0.02 we reject the null hypothesis and find that the age groups don't have a homogenous distribution.


#we also check that the expected values for all entries are at least 5
chi_result$expected


#we can then say that the age distributions are not independant.




#(e)To explore the relation between age and happiness, build a contingency table for R_happy against R_a.
#Use the Chi-square test on this table. Are the conditions for the test satisfied? Why or why not?

table3 <- table(df1$R_happy, df1$R_a)
table3

chi_age_happy <- chisq.test(table3)
chi_age_happy

#with a p-value of 0.6124 we fail to reject the null hypothesis and the test shows evidence that the happiness and age groups are independant 


#we then check the expected values of the chi squared.
chi_age_happy$expected
sum(chi_age_happy$expected < 5)

#we see that we have 5 cells with lower than expected value of 5. which indicates that the chi squared test here may be unreliable.
#given that the amount of expected cell vaalues < 5 is 25% which is slightly higher than the accepted 20%.
#therefore we can say that the evidence could be interpeted that the two categories are apprximately homogenous, although its not fully reliable.



#============================================================================================================
#(f) Create a new ordered factor called R_h in dt1 by joining the two lower levels of R_happy, i.e., R_h will
#have three level named unhappy, Somewhat happy, and very happy. The values for unhappy come
#from re-naming the levels Very unhappy and Not too happy as unhappy. One easy way to do this is
#to use the labels argument. Look at the help of the factor function to see how this is done.


df1$R_h <- factor(df1$R_happy,
                  levels = c("Very unhappy", "Not too happy", "Somewhat happy", "Very happy"),
                  labels = c("unhappy", "unhappy", "Somewhat happy", "Very happy"),
                  ordered = TRUE)

head(df1$R_h)
table(df1$R_h)



#============================================================================================================
#(g)Build a contingency table for R_h against R_a. Graph a mosaic plot of this table using different colors
#for the rectangles. Comment on what you observe. Use the Chi-square test on this table. Are the
#conditions for the test satisfied? Why or why not? What is your conclusion after using the test?

(table4 <- table(df1$R_h, df1$R_a))


mosaicplot(table4,
           main = "Happiness vs Age Group",
           xlab = "Age Group (R_a)",
           ylab = "Happiness (R_h)",
           color = c("lightblue", "lightgreen", "pink", "orange", "violet"))



#there seems to be a very equal distribution of age the age groups' happiness level.


chi_happy_age <- chisq.test(table4)
chi_happy_age

#with a p-value of 0.83936 we fail to reject the null hypothesis and we can say that happiness and age group are independent.
#we check cell expected count
chi_happy_age$expected

#since all cell counts are > 5, we can say that hte test conditions are satisfied and it's results are reliable











#============================================================================================================
#============================================================================================================
#============================================================================================================

data(women)
head(women)



#(a) Fit a simple linear regression model for weight as a function of height. Produce a scatterplot and add
#the regression line.

model <- lm(weight ~ height, data = women)

plot(women$height, women$weight,
     main = "Weight vs Height of American Women (30–39)",
     xlab = "Height (inches)",
     ylab = "Weight (pounds)",
     pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)



#(b) Print a summary table for the model and interpret the results. Write an equation for the model and
#interpret the coefficients. What is the R2 for this model?
summary(model)


#from the coefficients we get the following function:

#Weight = -87.52 + 3.45 * Height

#we can see that this result is only local to the area of interest since if we have a height of 10 we will get negative weight which is unreasonable
#we can also see that the weight increases linearly with height.

#R2 for this model is found to be 0.991


#  (c) Predict the weight for a woman of 65 inches including a confidence interval.

predict(model, newdata = data.frame(height = 65), interval = "confidence", level = 0.98)

#based on the fitted model, a 65-inch tall woman is predicted to weigh about 136.733 pounds.
#with an interval of 135.69 to 137.777 with 98% confidence.



#(d) State explicitly the assumptions on which the model is based and using plots and tests verify whether
##they are satisfied.

#A simple linear regression model has 4 assumptions.
#Linearity, Independence, Constant variance, normality of residuals.
#we test these next.
par(mfrow = c(2, 2))
plot(model)


#from the first plot we see a u-shape in th residuals vs fitted which indicates the relationship isn't entirely linear between height and weight
#from Q-Q residuals we see the points lying roughly at the line with deviations at the tails, this indicates normality.
#from scale-location the line is somewhat flat but the points aren't spread consistently indicating no constant variance.
#from the residuals vs leverage , no data points show high leverage which indicates independence and no influential points.

#we futher test for these assumptiopns
shapiro.test(residuals(model))
#with p-value > 0.02, we fail to reject H0 and we can say the distribution is normal


library(car)
ncvTest(model)
#with pvalue of >0.02, we fail tor eject H0 and we say the data has constant variance.





#(e) Use the function residualPlots in the car library. The argument of the function is the name of your
#model and the output is a couple of residual plots plus some summary information about two tests.
#The first plot is residuals vs. the regressor (height in this case) and the second is residuals against
#f
#itted values. In both cases, the blue line shown in the plot is not a local smoother but a quadratic term
#added to the model. If the line is flat, it indicates that the term will not improve the model. The results
#of tests of curvature are also shown. We will consider only the first one, corresponding to height. This
#is a test on the coefficient for a quadratic term in height added to the model. The null hypothesis is
##that coefficient corresponding to the quadratic term is zero. Interpret the output you get when using
#this function on your model.

residualPlots(model)

#since the blue lines aren't flat and close to 0, we know that the relationship is not linear, meaning a quadratic form could improve the model
#H0: linear, no quadratic term coefficient
#H1: non-linear, quadratic term coefficient is effecting.

#and with a p-value of height < 0.02, we reject the null hypothesis and the evidence points at the alternative which is there is a quadratic term coefficent, thus the relation ship is not linear.
#



#(f) Add a quadratic term in height to your model (you have to use the expression I(heightˆ2) in the
#              equation to do this). Print the summary table and interpret the results. What is the R2 for this model
#and how does it compare with the previous model?

model2 <- lm(weight ~ height + I(height^2), data = women)
summary(model2)

#intercept at 261.8781, 
#first order coefficient = -7.3483
#quadratic coefficient = 0.08306
#this gives us the function:

#Weight = 261.87818 - 7.3483 * Height + 0.08306 * Height^2

#R2 of this model is found to be 0.9995



#  (g) Plot the data and add the lines corresponding to the two model you fitted.

par(mfrow = c(1, 1))

plot(women$height, women$weight,
     main = "Weight vs Height with Linear and Quadratic Fits",
     xlab = "Height (inches)",
     ylab = "Weight (pounds)",
     pch = 19,            # solid circles
     col = "darkblue")
abline(model, col = "red", lwd = 2)
x_vals <- seq(min(women$height), max(women$height), length.out = 200)

y_quad <- predict(model2, newdata = data.frame(height = x_vals))

lines(x_vals, y_quad, col = "darkgreen", lwd = 2)

legend("topleft",
       legend = c("Data", "Linear fit", "Quadratic fit"),
       col = c("darkblue", "red", "darkgreen"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2),
       bty = "n")

#(h) Write down an equation for the final model. Predict the weight for a woman of 65 inches including a
#confidence interval using the quadratic model and compare with your previous prediction.




#The final model has the equation:

#Weight = 261.87818 - 7.3483 * Height + 0.08306 * Height^2

predict(model2,
        newdata = data.frame(height = 65),
        interval = "confidence",
        level = 0.98)

#and we predict a 65-inch tall woman to have weight of 135.1828, with a 98% confidence interval of [134.7825, 135.5831]
#compraed to our previous linear calculation:

#based on the fitted linear model, a 65-inch tall woman is predicted to weigh about 136.733 pounds.
#with an interval of 135.69 to 137.777 with 98% confidence.

#we see that we have a much smaller interval and a slightly lower fitted expected weight.
