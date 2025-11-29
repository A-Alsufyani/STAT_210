q1_data <- read.csv("HW825FQ1.csv")
#head(data)
str(q1_data)
#summary(data)

library(car)
scatterplotMatrix(q1_data)
#we can see from the scatterplot matrix that X1 X2 and X3 seem to have positive correlation indicating they have a very similar effecT ON y.
#We also see that in general Y is linearly affected by all variables, with the exception of x6.

library(corrplot)
cor_1 <- cor(q1_data)
corrplot.mixed(cor_1)

#same conclusions we got from earlier with x1,x2,x3 having very similar effects on Y
#we also see strong positive relationship of Y with x1,x2,x3 and X7, and a negative relationship with X4 and x5.









full_model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data = q1_data)
summary(full_model)
#from the t tests we  see that variables x2, x3, x5, x6 seem to not be significant to the model (p > 0.02)
#also the p-value of the entire model is <2.2e-16, thus we reject the null hypothesis and say that at least one variable is useful for predicting Y






vif(full_model)
# x6 is the only one below a threshold of 2, we drop the highest value which is x3.
mod1 <- update(full_model, .~. - X3)
vif(mod1)
#again for x1
mod2 <- update(mod1, .~. - X1)
vif(mod2)
#again for x5
mod3 <- update(mod2, .~. - X5)
vif(mod3)
#now all remaining variables are under the threshold so we keep them.




#with alpha_critical = 0.1, we start using backward selection procedure.
drop1(mod3, test = "F")
#we drop the highest P-value (most insignificant variable) which is X6.
mod4 <- update(mod3, .~. - X6)
#and we repeat
drop1(mod4, test = "F")
#since all values are > alpha_critical, we found our minimal adequate model. using variables X2, X4, X7 to predict Y
summary(mod4)





library(MASS)
aic_model <- stepAIC(full_model)

summary(aic_model)

#this model usees X1, X4, and X7 to predict Y with an R squared value of 0.8391
#the model from part d uses X2 instead of X1 and has an R squared value of 0.8055, this means the AIC model is a better fit to the data





library(leaps)
subsets <- regsubsets(q1_data[ , -1], q1_data[,1])
summary(subsets)
summary(subsets)$adjr2 #extract adjr^2


best_adjR2_model <- lm(Y ~ X1 + X4 + X7, data = q1_data) #fit the maximum r^2 model
vif(best_adjR2_model) # check for multicollinearity.

#we get that the maximum adjusted R value is 0.8357875  which is the 3 predictor model which includes predictors X1, X4, and X7.
#also checking for multicollinearity with threshold 2, we see that all predictors are below that so no issues here.
#This model is an exact replica of the AIC_model, which means we already got the best fitting model using the AIC method.
#and of course it follows the same comparison to part D which used X2 instead of X1 and has a lower R^2 and adj R^2 values.

#Regarding choice i'd choose the model we found with best R^2 and AIC model which is the same, this gives us the best fit with the best predictors.





par(mfrow = c(2, 2))
plot(mod4)
par(mfrow = c(1, 1))

#we see from residuals vs fitted that the line is nearly horizantal and is near 0, indicating linearity.
#the QQ residuals plot shows the points roughly lying on the line, indicating normality
#the scale-location plot is also horizantal with an even spread of points which indicates that we have equal variances.
#residuals vs leverage shows a few points with higher leverage but no signficant influential points.

shapiro.test(residuals(mod4))
ncvTest(mod4)


#with a p-value of 0.6585 in shapiro test we can say that the resdiuals are normally distributed
#with a p-value of 0.83124 in the ncv test we can say that the resdiausl have equal variances.



predict_data <- data.frame(X2 = 9.6, X4 = 9.2, X7 = -0.8)
predict(mod4, newdata = predict_data, interval = "c", level = 0.98)


















q2_data <- read.csv("HW825FQ2.csv")
#head(data)
str(q2_data)
q2_data$member <- as.factor(q2_data$member)
str(q2_data)







mod_a <- lm(duration ~ age, data = q2_data)
summary(mod_a)

#we can see the p-values being very low indicating the coefficients != 0, we also see the adj r^2 value of 0.7529

plot(q2_data$age, q2_data$duration,
     xlab = "Age",
     ylab = "Duration",
     main = "Scatterplot of Duration vs Age")

abline(mod_a, col = "red", lwd = 2)

#we see that the data points show a positive relationship between duration and age, we also see that the model fits that general positive trend.

par(mfrow = c(2, 2))
plot(mod_a)
par(mfrow = c(1, 1))

#residuals vs fitted shows a very curved line with an inverted U shaped, this indicaites non-linearity.
#q-qresiduals shows the points lying close to the line but not perfectly fitting, indicating non-normality
#scale-location is mostly horizantal with a small positive slope, indicates constant variance but requires further testing
# residuals vs leverage shows a few points with high leverage (1, 19) but no influential points.

shapiro.test(residuals(mod_a))
ncvTest(mod_a)

#Shapiro test p-value of 0.06394 indicates linearity using since our signficance level is 0.02 and 0.06394 > 0.02
#ncv test p-value of 0.093 > 0.02 which means we have constant variance at that level

predict(mod_a, newdata = data.frame(age = 45), interval = "c", level = 0.98)

#we predict a 45 years old rider to have a duration of 83.8744, with interval [80.7554, 86.9933]







plot(q2_data$age, q2_data$duration,
     col = c("blue", "red")[q2_data$member],
     xlab = "Age",
     ylab = "Duration",
     main = "Scatterplot of Duration vs Age")

legend("topleft",
       legend = c("Non-Member", "Member"),
       col = c("blue", "red"),
       lwd = 2)
#we see that the data points seem to split into two different trendlines based on the member variable.





mod_b <- lm(duration ~ age * member, data = q2_data)
anova(mod_b)

#looking at the P values of the predictors, we see that age, member, and their interactions are signficant for predicting duration.
#therefore we don't drop any variable and take this to be the minimal adequate fitting model.

summary(mod_b)

#the equation we get is:
#duration = beta1 + beta2 * age + beta3 * member1 + beta4 (age * member1), with member1 being a dummy variable indicating whether they are a member or not
#and the 4 beta values are the coefficients of the model, (b1 = 27.40, b2 = 1.45, b3 = 6.72, b4 = -0.43)

#the estimated variance of errors is found to be 3.33 from the ANOVA table. R^2 is found to be 0.973 from the model summary which indicates a very good fit of the data.





plot(q2_data$age, q2_data$duration,
     col = c("blue", "red")[q2_data$member],
     xlab = "Age",
     ylab = "Duration",
     main = "Scatterplot of Duration vs Age")

legend("topleft",
       legend = c("Non-Member", "Member"),
       col = c("blue", "red"),
       lwd = 2)

abline(a = coef(mod_b)[1], b = coef(mod_b)[2], col = "blue", lwd = 2)

abline(a = coef(mod_b)[1] + coef(mod_b)[3],
       b = coef(mod_b)[2] + coef(mod_b)[4],
       col = "red", lwd = 2)


#The equation for this model is:
# Duration_Non_Member = 27.39758 + 1.44505 * age
# Duration Member = 27.39758 + 1.44505 * age + 6.72162 - 0.43375 * age = 34.1192 + 1.0113 * age




par(mfrow = c(2, 2))
plot(mod_b)
par(mfrow = c(1, 1))

#from residuals vs fitted we see the line curves a lot and isn't horizantal, indicating non-linearity
#from qq residuals we see the points roughly fit the dashed line, indicating normality
# from scale-locaiton we see the line curving a lot and and has a strong slope at t he end, indicating no constant variance
#from residuals vs leverage we see no influential points but some points with high leveratge (13, 28)

shapiro.test(residuals(mod_b))
ncvTest(mod_b)

#with a high p-value for both shapiro and ncv test, we say that the residuals follow a normal distribution and have constant variance.




(predict_non <- predict(mod_b, newdata = data.frame(age = 45, member = "0"), interval = "c", level = 0.98))
(predict_member <- predict(mod_b, newdata = data.frame(age = 45, member = "1"), interval = "c", level = 0.98))

#we predict that a non member aged 45 would have a rental duration of 92.425 with a 98% confidence interval [90.456, 94.394]
#and a member with the sage age to have a duration of 79.627 with a 98% confidence interval of [78.300, 80.955]



