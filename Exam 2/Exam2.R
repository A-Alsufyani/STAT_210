#(a) Read the data. Create a new data frame called Q1data with the variables age, mass, and diabetes.
#Check whether the new data frame has entries with age or mass equal to zero, and delete those entries
#from your file. How many observations are left?

data_1 <- read.csv("XM225F_q1.csv")
head(data_1)

Q1data <- data_1[, c("mass", "age", "diabetes")] #extract the data frame of wanted values
str(Q1data)
sum(is.na(Q1data[, "mass"])) # check if mass has any empty values
sum(is.na(Q1data[, "age"]))  # check if age has any empty values.

#No  empty values,  No observations deleted all 768 observations remain.






#(b) Create a new ordered factor in Q1data with name bmi according to the following rules: If mass is less
#than or equal to 25 the value of bmi is normal; if mass is over 25 and up to 30 the value of bmi is
#overweight, and if mass is above 30 the value is obese.
bmi_cutoff_names <- c("Normal", "Overweight", "Obese")
bmi_cutoff_values <- c(0, 25, 30, 1000) # from 0 to 25 is normal, 25 to 30 is overweight, 30 and above is obese.

Q1data$bmi <- cut(Q1data$mass,
                  breaks = bmi_cutoff_values,
                  labels = bmi_cutoff_names,
                  ordered = TRUE)




#(c) Create a contingency table for bmi and diabetes. Graph a mosaic plot of this table using different colors
#for the rectangles. Comment on what you observe. Using an appropriate statistical test, determine
#whether these two variables are independent. What are the underlying assumptions for the test? Discuss
#whether they are satisfied in this case.

(table1 <- table(Q1data$bmi, Q1data$diabetes)) # create a contingency table for bmi and diabetes


#draw the mosaic plot.
mosaicplot(table1,
           main = "BMI vs diabetes",
           xlab = "BMI Group (bmi)",
           ylab = "Diabetes Result (diabetes)",
           color = c("blue", "red"))

#From the mosaic plot we can notice two things,
#1: the amount of people considered obese is more than half the population of the dataset
#we can see that through the width of each rectangle.

#2: diabetes seems to be linked to BMI in a positive correlation, with higher BMI
#suggesting a higher probability of diabetes.


(chi_bmi_diabetes <- chisq.test(table1))

#with a p-value of 2.2e-16 << 0.02, we reject the null hypothesis of independancy. and we say that BMI and diabetes have an dependant relationship

#we then check the expected cell count to check the reliability of the test
chi_bmi_diabetes$expected
sum(chi_bmi_diabetes$expected < 5)

#the test requires that the expected cell values must not be less than 5
#a maximum of about 20% of the values being less than 5 is acceptable to suggest reliabilty.
#since we have 0 cells with expected value < 5, we can say that the test is reliable
# and that out deduction of dependancy is valid.




#(d) Create a new ordered factor in Q1data named fage by dividing age into four groups having approximately
#the same number of subjects. Name the levels a1, a2, a3, and a4. Produce a contingency table for
#fage and bmi (bmi should correspond to the rows of the table). Graph a mosaic plot of this table using
#different colors for the rectangles. Comment on what you observe. Using an appropriate statistical test,
#determine whether these two variables are independent. What are the underlying assumptions for the
#test? Discuss whether they are satisfied in this case

(q <- quantile(Q1data$age, probs = seq(0, 1, length.out = 5))) #split into approximately equal size chunks.


#make the new factor "fage" using the equal split sized of ages
Q1data$fage <- cut(Q1data$age,
                  breaks = q,
                  labels = c("a1","a2","a3",'a4'),
                  ordered = TRUE)

(table2 <- table(Q1data$bmi, Q1data$fage)) # create a contingency table for bmi and diabetes

mosaicplot(table2,
           main = "Fage vs BMI",
           xlab = "BMI Group (bmi)",
           ylab = "Age Group (fage)",
           color = c("green", "yellow", "blue", "red")) #draw the mosaic plot of the table.

#From the mosaic plot, we can see that for a1 group, they are mostly normal, and then overweight and have the lowest percentage of obesity.
#a2 has a high percentage of normal and obese, with a lowly populated portion for overweight.
#we also see a3 having the highest percentage of overweight people, followed by approximately the highest percentage of obese people.
#and we see that a4 has about an equal spread of different bmi levels.



(chi_fage_bmi <- chisq.test(table2))

#with a p-value of 0.1134 > 0.02, we fail to reject the null hypothesis and we say that age group and BMI are independent.

#we then check the expected cell count to check the reliability of the test
chi_fage_bmi$expected
sum(chi_bmi_diabetes$expected < 5)

#the test requires that the expected cell values must not be less than 5
#since we have 0 cells with expected value < 5, we can say that the test is reliable
# and that out deduction of independency is valid.


#================================================



Q2data <- read.csv("XM225F_q2.csv")
head(Q2data)
str(Q2data)
Q2data$fertilizer  <- as.factor(Q2data$fertilizer)
str(Q2data)


#(a) Do a scatterplot of yield as a function of rainfall, including the regression line. Fit a regression
#and print the summary table. Interpret the output in the table. State explicitly the assumptions that
#underlie this model and use diagnostic plots and tests to verify whether they are satisfied. Include your
#comments on every step that you take.

model_1 <- lm(yield ~ rainfall, data = Q2data)

plot(Q2data$rainfall, Q2data$yield,
     main = "Yield vs Rainfall",
     xlab = "Rainfall",
     ylab = "Yield",
     pch = 19, col = "blue") # do a scatterplot of rainfall vs yield.

abline(model_1, col = "red", lwd = 2) #draw the regression model on the plot

summary(model_1) #print summary of model.

#From the table we can see the information of the residuals, the coefficient estimates etc.

#we focus on the p-value of each predictor in the model, and we see that both the intercept and rainfall have
#p-values < 0.02 (0.0528 and 3.28e-8 respectively).

#For both cases we reject the null hypothesis of these coefficients actually being 0
#and we say that our model suggests that they are not 0 and are useful in predicting yield.

#we also see the R^2 and adjusted R^2 of 0.2288 and 0.2223, indicating that the model somewhat captures
#the plot but isn't a great fit at all.


#Regarding assumptions, A simple linear regression model has 4 assumptions.
#Linearity, Independence, Constant variance, normality of residuals.
#And we test these next:

par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))

#From the residuals vs fitted, we see that the line is roughly horizantal and close to 0 with an even spread of points, indicating Linearity.

#From QQ residuals we see the values fitting somewhat well in the middle but having strong tails, we also notice a weird curve in the middle. all of this indicates non-normality and further testing is required.

#From Scale-location we see the line having a somewhat steady positive slope going away from the horizantal 0 which may indicate non-constant variance, with the data points having some kind of even spread, but we would require further testing.

#From residuals vs leverage we determine there being no strongly influential points, but we do point out some points with high leverage.


#we then do the shapiro-wilk test of normality and the ncv Test for constant variance.

shapiro.test(residuals(model_1))

library(car)
ncvTest(model_1)


#The shapiro- wilk test gave a p-value of 5.378E-6 < 0.02, which means we reject the null hypothesis of normality, and we say that the spread of residuals is non-normal.

#the ncv test gave us a p-value of 0.022 > 0.02, which means we fail to reject the null hypothesis, and we say that the residuals have constant variance




#(b) Fit a model that includes rainfall, fertilizer, and the interaction between the two. Using a critical
#value for α of 0.05 and starting with the complete model, select a minimal adequate model. Compare
#the adjusted R2 and the standard deviation for the errors (residual standard error) with the previous
#model. Check the assumptions for the final model. Write down the equation for this regression model
#and predict the value of the yield for a rainfall of 770 for both types of fertilizer, including confidence
#intervals at the 98% confidence level. Include your comments on every step that you take.

model_2 <- lm(yield ~ rainfall * fertilizer, data = Q2data) #rainfall * fertilizer is basically both + interaction
summary(model_2)

#with a p-value of  0.809, the T test shows a very high probability of fertilizer being 0, thus having no signficant effect on predicting yield.
#therefore we delete it from our model since it's larger than our a_critical of 0.05

model_3 <- update(model_2, .~. - fertilizer)
summary(model_3)


#We can see that our new model has the same R^2 values, but it's expected since the predictor we got rid off was not affecting the prediction by much.
#we can also see that our adjusted R^2 increased, which is also expected since we got one less predictor for our model.

#we also see a lower value of the residual standard error, meaning the residuals are more tightly packed.



par(mfrow = c(2, 2))
plot(model_3)
par(mfrow = c(1, 1))

#From the residuals vs fitted plot, we see the line being horizantal with somewhat an even spread of points, indicating linearity.

#From Q-Q residuals we see the points lying neatly on the line, indicating normality

#From the scale-location plot, we see the line being mostly horizantal with an even spready of points, indicating constant variance.

#from residuals vs leverage: we see that there are no influential points, but some points with moderate leverage.


#we then do a shapiro-wilk and an ncv Test for further evidence.

shapiro.test(residuals(model_3))
ncvTest(model_3)

#With a shapir-wilk test p-value of 0.48 >> 0.02, we fail to reject the null hypothesis and say that the residuals are normally distrubted.
#with the ncv test p-value of 0.92228 >> 0.02, we fail to reject the null hypothesis and say that the residuals have constant variance.


#From the summary table of our minimal model, we extract two functions for the lines, one if the fertilizer is organic and one otherwise.

# Yield_organic =  1.9056084 + 0.0082430 * Rainfall - 0.0040822 Rainfall

# Yield_organic = 1.9056084 + 0.0041608 * Rainfall


#Yield_inorganic = 1.9056084 + 0.0082430 * Rainfall


#We now do the prediction using the two different types of fertilizers.

predict(model_3, newdata = data.frame(rainfall = 770, fertilizer = "Organic"), interval = "c", level = 0.98)
predict(model_3, newdata = data.frame(rainfall = 770, fertilizer = "Chemical"), interval = "c", level = 0.98)


#We predict a rainfall of 770  with use of Organic fertilizer would give a yield of 5.109491 with 98% confidence interval [4.880409, 5.338573]


#We also predict a rainfall of 770  with use of Chemical fertilizer would give a yield of 8.252753 with 98% confidence interval [8.007318, 8.498189]



#plot of yield ~ rainfall with fertilizer types
plot(Q2data$rainfall, Q2data$yield,
     col = c("blue", "red")[Q2data$fertilizer],
     xlab = "Age",
     ylab = "Duration",
     main = "Scatterplot of Duration vs Age")

abline(a = coef(model_3)[1], b = coef(model_3)[2], col = "blue", lwd = 2)

abline(a = coef(model_3)[1],
       b = coef(model_3)[2] + coef(model_3)[3],
       col = "red", lwd = 2)

legend("topleft",
       legend = c("Chemical", "Organic"),
       col = c("blue", "red"),
       lwd = 2)


#==============================================

#(a) Do a scatterplot matrix for the variables in the data set. Calculate and graph the correlation matrix
#for these variables. Comment on the results.



scatterplotMatrix(Q3data)

cor_1 <- cor(Q3data)
corrplot.mixed(cor_1)



#From the scatterplot matrix, we can see that los has a generally positive correlation with severity, bmi, p.adm, and age
#we can also see positive correlation with steady values for hr and cmb

#The correlation matrix graph shows the trends more clearly.
#with spb and dpb having showing the strongest positive correlation, seconded by los and p.adm, and then age and dbp.

#we also see that LOS seems to be postively correlated mostly with p.adm, severity, cmb, bmi, and age in this order.



#(b) Fit a regression model for length of stay (los) as a function of the rest of the variables. With a threshold
#for the variance inflation factor of 2, use a sequential procedure to eliminate variables that may cause
#multicollinearity problems.


#make the full model and print out its summary
full_model <- lm(los ~ age + bmi + sbp + dbp + hr + cmb + p.adm + severity, data = Q3data)
summary(full_model)


vif(full_model) # print out the vif of the model
#we see that the highest value is dbp and is higher than the threshold of 2, so we get rid of it and check again.
mod1 <- update(full_model, .~. - dbp)
vif(mod1)
#we now seee that this model has all values under the threshold of 2.





#(c) Using a backward selection procedure with a critical α of 0.10 and starting with the variables you
#selected in (b), obtain a minimal adequate model. Comment on the steps that you take.


#The general procedure is to use drop1 to print the single term deletions table and drop the predictor with the highest p-value larger than a_critical of 0.2.

drop1(mod1, test = "F")
mod2 <- update(mod1, .~. -hr) #here we drop hr which has a value of 0.91 > 0.1

drop1(mod2, test = "F")
mod3 <- update(mod2, .~. -sbp) #here we drop sbp, which has a value of 0.22 > 0.1

drop1(mod3, test = "F")

#Since all values are over a_crit, we got our minimally adequate model.
summary(mod3)



#(d) Starting with the complete model, fit a model using a stepwise procedure for the AIC criterion. Compare
#your final model with the result of (c).\



aic_model <- stepAIC(full_model) #use AIC to get a minimal model.
summary(aic_model) # print out the summary


#the two models are exactly the same using the same predictors
#and using the same predictors gives us the same values of R^2, adj R^2, and Residual standard error.


#(e) Write an equation for the final model in (c) and interpret the coefficients. Predict the length of stay
#(los) for a patient with the following covariates. Include a confidence interval at the 98% level.

summary(mod3)

#The equation for the model is:
#Los = 0.59390 + 0.06819 age + 0.24510 bmi + 0.50735 cmb + 1.42154 p.adm + 0.90215 severity



predict_data <- data.frame(age = 46, bmi = 30.1, sbp = 132, dbp = 93,
                           hr = 77, cmb = 3, p.adm = 1, severity = 2.1)

predict(mod3, newdata = predict_data, interval = "c", level = 0.98)


#with the given inputs, we predict that length of stay (los) would be 14.7587 with a 98% confidence interval of [13.57924, 15.93816]



#(f) State explicitly the assumptions required for the multiple regression model. Using graphs and tests
#verify whether these assumptions are satisfied.


#our assumptions are Linearity, Independence, Constant variance, normality of residuals.
#we test these out using the plots and further testing with shapiro-wilk and ncv tests.


par(mfrow = c(2, 2))
plot(mod3)
par(mfrow = c(1, 1))


#From the residuals vs fitted we can see that the line is mostly horizantal based on 0, which indicates linearity

#from the qq-resudals we can see the line almost perfectly fitting the points, indicating normality.

#From the scale location, we can see the line being mostly horizantal with a roughly even spread of points, indicating constant variance but may need further testing.

#From residuals vs leverage we see that there are no influential points, with most having weak influence.

#We can then do the shapiro-wilk and ncv tests.

shapiro.test(residuals(mod3))
ncvTest(mod3)


#with a p-value of 0.1384 > 0.02 for the shapiro test, we fail to reject the null hypothesis and say the residuals are normally distributed.

#with a p-value of 0.31955 > 0.02 for the ncv test, we fail to reject the null hypothesis and say that the residuals have constant variance.

