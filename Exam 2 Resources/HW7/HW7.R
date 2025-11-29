

data <- read.csv("25Fhw7Q1.csv")
head(data)

# plot the data
plot(data$varx, data$vary,
    xlab = "varx", 
    ylab = "vary",
    main = "Scatterplot of vary vs varx")
#fit a  simple linear regressio model
model <- lm(vary ~ varx, data = data)


#adding the regression line

abline(model, col = "red", lwd = 2)

#the linaer regression line is not fitting to the data
#it is expected as the data point themselves seem to follow a very exponential curve with low slopes at the start and an insane rise at some point
#here the point is around 2.2


summary(model)

#we see the summary of values of the interval with the median being -58.74 which is not close to 0 (which is expected since it doesnt fit), 
#we can also see the coefficients estimates and their p-value
#the H0 here is that the coefficient is 0.
#given that both the intercept and linear part  P-value is < 0.02, we can say evidence suggests these values are not 0.
#we also see the residual standard error of 279
#we also see a low R squared value of 0.4628, which means a majority of the variation in vary is not  explained by varx.
#the equation of this model is vary = -212.55 + 287.02 varx

par(mfrow = c(2, 2))
plot(model)
#resdiuals vs fitted shows the trendline is not horizantal and deviates from 0, which indicates non-linearity.
#q-q plot shows the point lying on the line except for a very strong end tail, which indicates that the data is not normal.
# scale-location shows weird behaviour which indicates that equal variance assumption may be incorrect.
#residuals vs leverage  shows a couple influential outliers.


shapiro.test(residuals(model))

#shapiro test of normality, with p-value of 2.34e-07 < 0.02, we reject null hypothesis and we say the data is not normally distributed.
library(car)

ncvTest(model)
#at p-value of 1.47e-8 < 0.02, we reject null hypothesis and we say the data does not have homogenous variance. (equal variance)

predict(model, data.frame(varx = 2.5), interval = 'c', level = 0.98)
#using this model we predict varx = 2.5 to have vary of 505, with the interval [361.6351, 648.3796] with 98% confidence.


library(MASS)

par(mfrow = c(1, 1))
boxcox(model)
#given that our confidence interval includes lambda = 0, we choose a logarithmic transformation for vary.

data$log_vary <- log(data$vary)
plot(data$varx, data$log_vary,
     xlab = "varx", 
     ylab = "log_vary",
     main = "Scatterplot of log_vary vs varx")

(model_log <- lm(log_vary ~ varx, data = data))
abline(model_log, col = "red", lwd = 2)

#now this new plot and model appear to fit as expected. we now look at the summary

summary(model_log)
#we see our median somewhat close to 0 which is expected since the line fits better.
#we also see the p-values being too low thus we have coefficients for varx and intercept.
#function is: log(vary) = 1.08 + 1.95 varx
#we also have an R^2 of 0.91

par(mfrow = c(2,2))
plot(model_log)
#this time we see the residuals vs fitted being roughly horizantal and close to 0 which indicates linearity. unlike the original model
# Q-Q residuals line fits the points which indicates normality. unlike the original model
# scale-location shows some variance and the line is also not perfectly horizantal which may indicate that variance is not equal.
# residuals vs leverage shows that some points have strong leverage (points 10 and 47), but their influence is not as strong as the points of the original model.

shapiro.test(residuals(model_log))
#with a p-value > 0.02, we accept the null hypothesis and say that evidence points that the data is normal.

ncvTest(model)
#with p-value << 0.02, we reject the null hypothesis and say that the data appears to not have homogenous variance (non-equal variance)


#function is: log(vary) = 1.08 + 1.95 varx

(log_prediction <- predict(model_log, data.frame(varx = 2.5), interval = "c", level = 0.98))
(original_prediction <- exp(log_prediction))

#using this new model we predict vary to be 390.72 at varx = 2.5, with the 98% confidence interval [293.87,  519.49]

par(mfrow = c(1,1))

plot(vary ~ varx, data = data,
     xlab = "varx", ylab = "vary",
     main = "Scatterplot of vary vs varx")
abline(model, col = "blue", lwd = 2)

curve(exp(coef(model_log)[1] + coef(model_log)[2] * x),
      add = TRUE, col = "red", lwd = 2)

legend("topleft",
       legend = c("Original linear model", "Transformed model"),
       col = c("blue", "red"),
       lwd = 2)

#we can obviously see that the transformed model fits the data much much better than the original linear model.











#============================================================================

data2 <- read.csv("25Fhw7Q2.csv")
str(data2)

plot(w ~ t, data = data2,
     xlab = "t", ylab = "w",
     main = "Scatterplot of w vs t")

model2 <- lm(w ~ t, data = data2)
abline(model2, col = "red", lwd = 2)


summary(model2)
#we can see the residuals informatino at the beginning, with the median deviating from 0 which is expected since the line doesn't fit properly.
#we can also see the coefficients estimates, standard errors, and their t values along with their p-values
#the null hypothesis here is that these coefficients are 0, and with a 98% confidence and both having < 0.02, we can say that these coefficients are not 0.
# finally we can see the residual standard error of 3.901  (estimated standard deviation of errors), along with the R^2 and adjusted R^2 being 0.4826 and 0.4427 respectively.
#we also see the f-statistic and corresponding p-value, this is for a t-test.

#the equation for the model is:
# w = 11.3515 - 0.3238 t

par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))
#residuals vs fitted doesn't show the line as horizantal and close to 0, so this indicates non-linearity.
# q-q residuals roughly fits but has very strong tails, may indicate non-normality
# scale-location, the variances are all over the place and hte line isn't horizantal, indicating non homogenous variance.
#residuals vs leverage, we can see some strong influential points (like 15) and other influentials like point 12.

shapiro.test(residuals(model2))
#with a p-value > 0.02, we accept the null hypothesis and say that evidence points that the data is normal.

ncvTest(model2)
#with a p-value < 0.02, we reject the null hypothesis of equal variance.

#we can probably identify these points to be the point at t = 50, and the two points at t aprproximately = 35 and 36 with w values of less than -5.
#we can see them more clearly reading directly from the data
data2[15,]
data2[8, ]
data2[10, ]



clean_data2 <- data2[-c(8,10,15),]

plot(w ~ t, data = clean_data2,
     xlab = "t", ylab = "w",
     main = "Scatterplot of w vs t")

clean_model2 <- lm(w ~ t, data = clean_data2)
abline(clean_model2, col = "blue", lwd = 2)
abline(model2, col = "red", lwd = 2)

#we can see that the new model fits the data better than the original one (obviously excluding the outliers).

summary(clean_model2)

#we can see a median much closer to 0, which is expected since it fits the data better.
#we also get different coeffciient data. p-values are low thus we say these coefficients are not 0.
#Function of this new model is:
# w = 14.3349 - 0.4302 t
#estimated standard deviation of errors is 1.602
#R squared is 0.8942

#given that the standard deviation changed from 3.901 to 1.602, we can say that these points are influential
#we also see that the function itself changed drastically and R^2 doubled.

par(mfrow = c(2,2))
plot(clean_model2)
par(mfrow = c(1,1))

#R vs Fitted: line is now horizantal and close to 0 indicating linearity.
#QQ: points fit the line more cleanly with some tails.
#scale-location: line isn't horizantal and variances don't seem to be equal, further testing is needed.
#residuals vs leverage shows point 12 as influential here.


shapiro.test(residuals(clean_model2))
#with a p-value >> 0.02, we accept the null hypothesis and say that evidence points that the data is normal. which is the same result as the original data

ncvTest(clean_model2)
#with a p-value > 0.02, we accept the null hypothesis of equal variance. which is opposite of what we had in the original 


#the estimated parameters formula:
# β =N(β0,β1)′,σ^2(X′X)^-1


(invXtX <- summary(clean_model2)$cov.unscaled) #matrix (X'X)^-1
(std <-summary(clean_model2)$sigma) # standard deviation
(std^2) # estimated variance

vcov(clean_model2) # estimated covariance
#which should also be equivalent to
((std^2) * invXtX)




confint(clean_model2, level = 0.99)
#we got the following intervals of the clean model with 99% confidence:
# Intercept [10.06, 18.61]
# t [-0.58, -0.28]

confint(model2, level = 0.99)
#and this for the original model
# intercept [2.46, 20.24]
# t [-0.60, -0.04]
#we can see that the range of the interval of the clean one is much smaller.

predict(model2,
        newdata = data.frame(t = 45),
        interval = "confidence",
        level = 0.99)
#we predict -3.22, with interval [-8.44, 2.00] with the original model at 99% confidence.

predict(clean_model2,
        newdata = data.frame(t = 45),
        interval = "confidence",
        level = 0.99)
#we predict -5.02, with interval [-8.06, -1.99] with the clean model at 99% confidence.