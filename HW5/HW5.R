library(car)
data <- read.table("25Fhw5Q1", header = TRUE);
#head(data)
#unique(data$diet)
str(data)

#change up the diet column into type factor.

data$diet <- as.factor(data$diet)

str(data)

levels(data$diet)



#here we show the data visually as a box plot and as points on top of the same plot.
boxplot(weight ~ diet, data = data,
        main = "Weight gain by Diet type",
        xlab = "Diet",
        ylab = "Weigh Gain",
        col = c("blue", "green", "yellow", "red", "cyan"))

points(weight ~ diet, data = data, pch = 16, col = 'purple')



#For the hypothesis test, we choose the null hypothesis H0: mu_ctrl = mu_dt1 = mu_dt2 = mu_dt3 = mu_dt4
#Which means that our hypothesis is that all diets have the same mean gain.

#Thus the alternative hypothesis would be that at least one of the diets has a different mean. which means it probably had an effect on weight gain.

#we then fit the anova model to the data.

mod1 <- aov(weight ~ diet, data = data)
summary(mod1)


#since we have a very small p-value (9.68e-10), then we reject the null hypothesis which was that all diets are the same, or in other words, the diets had no effect.

#we next try to find the cell means and analyze further.
#we use model.tables with argument "mean" to find the cell mean of the different types of diets
model.tables(mod1, 'mean', se = T)

#to find the effects of each diet we use the same function without the "mean" argument.

model.tables(mod1, se = T)


#Equation of the Anova model:
#   Y_ij = mu + tau_i +  epsilon_ij

# Where:
# Y_ij = weight gain for the j-th observation in the i-th diet group
# mu = overall mean weight gain
# tau_i = effect of the i-th diet (deviation from overall mean)
# epsilon_ij = random error term
# i = 1, 2, 3, 4, 5 (diet groups: ctrl, dt1, dt2, dt3, dt4)
# j = 1, 2, ..., n_i (observations within each group)


#Assumptions for the Anova model:
#   Independence, Normality, Homogeneity of Variance.


#Diagnostic Plots:

par(mfrow=c(2,2))
plot(mod1)


#Residual vs fitted shows the trend line being close to 0 which indicates linearity. although there is some variation in the spread of residuals which violates homogenity of variance, further testing may be needed.
#Q-Q plot shows the points lying roughly on the line, which suggests normality.
#the scale-locaiton plot shows somewhat of an issue with our equal variance assumption, requires further testing.
#Constant Leverage plot shows all points within the dotted lines which shows no influential outliers.


#Levene's test for homogenity of variance
#H0 is that all groups have equal variance
#H1 is at least one group has a different variance.

leveneTest(mod1)

#with p-value = 0.2686 , we accept the null hypothesis and that the honogenity of variance assumption is valid.



#Shapiro-wilk test for normality
#H0: Residuals are normally distributed
#H1: Residuals are not normally distributed
shapiro.test(rstandard(mod1))

#with p-value = 0.7903 , we accept the null hypothesis and that the distribution of residuals is normal.



(mod1.tky <- TukeyHSD(mod1))

#By analysing  Tukey's HSD procedure. we see that Diet 1 and 4 show significant difference compared to the control group.
#We also see that diets 2 and 3 have no signficant difference to the control group and are not distinguishable.
#we can also see that diets 1 and 4 are also not distinguishable.

par(mfrow=c(1,1))
plot(mod1.tky, las = 1 )



#for the non-parametric test we use the Kruskal-Wallis test which is the non-parametric alternative to the anova model.

kruskal.test(weight ~ diet, data = data)

#the test gives a p-value of 0.0001082, which leads to the same conclusion as the anova model.




#if the goal is to maximize weight gain, Diets 1 and 4 are recommended.
#Diets 1 and 4 showed significantly higher average weight gains compared to the other diets and control group, with no signficant difference between them.





#===================================================================


# Read in the fertilizer data
data2 <- read.table("25Fhw5Q2", header = TRUE)
str(data2)
data2$fertilizer <- as.factor(data2$fertilizer)
levels(data2$fertilizer)


# Visualize the data: boxplot with points
boxplot(height ~ fertilizer, data = data2,
        main = "Plant Growth by Fertilizer Type",
        xlab = "Fertilizer Type",
        ylab = "Plant Height (cm)",
        col = c("blue", "green", "yellow", "red", "cyan"))

points(height ~ fertilizer, data = data2, pch = 16, col = 'purple')


# Hypothesis Test:
# H0: mu_control = mu_A = mu_B = mu_C = mu_D
# (All fertilizers produce the same mean growth)
# H1: At least one fertilizer has a different mean effect on plant growth.

mod2 <- aov(height ~ fertilizer, data = data2)
summary(mod2)

#with p_value = 2.1e-5, we reject the null hypothesis.

# Find cell means and standard errors
model.tables(mod2, "mean", se = TRUE)

# Find effects for each fertilizer
model.tables(mod2, se = TRUE)


# Equation of the ANOVA model:
#   Y_ij = mu + tau_i + epsilon_ij
#
# Where:
# Y_ij = plant height for the j-th observation in the i-th fertilizer group
# mu = overall mean height
# tau_i = effect of the i-th fertilizer (deviation from overall mean)
# epsilon_ij = random error term
# i = 1, 2, 3, 4, 5 (fertilizer groups: control, A, B, C, D)
# j = 1, 2, ..., n_i (observations within each group)
#
# Assumptions: Independence, Normality, and Homogeneity of Variance.


# Diagnostic Plots
par(mfrow = c(2, 2))
plot(mod2)

# Residuals vs Fitted: shows random scatter around 0 (linearity, equal variance).
# Q-Q plot: points are roughly on the line,  suggests normality.
# Scale-location: variance is mostly constant across fitted values.
# Residual vs Leverage: shows no influential outliers.

# Levene’s Test for Homogeneity of Variance
# H0: all groups have equal variance
# H1: at least one group differs
leveneTest(mod2)

#p-value = 0.8688, therefore we accept H0, variances are homogenous.




# Shapiro–Wilk Test for Normality of Residuals
# H0: residuals are normally distributed
# H1: residuals are not normally distributed
shapiro.test(rstandard(mod2))

#p-value = 0.4985, therefore we accept H0, residuals are normally distributed.



# Tukey’s HSD Comparisons
(mod2.tky <- TukeyHSD(mod2))

#We see that F1 and F2 are indistinguishable and they have a significantly higher mean than the control group, they are also somewhat indistinguishable from ft3 which had the strongest results.
#we also see that ft4 is not significantly  different from the ctrl group.
#therefore for best performance we see ft3, then ft1 or ft2, and lastly ft4.

par(mfrow = c(1, 1))
plot(mod2.tky, las = 1)


# Non-parametric Alternative: Kruskal–Wallis Test
kruskal.test(height ~ fertilizer, data = data2)

#with p-value of 0.001262, it leads to the same conclusion as the anova model.



#For the goal of maximizing the plant height, Fertilizer 3 is recommended as it has shown the most signficant improvement.
# Fertilizers 1 and 2 also showed some improvement, but not to the level of Fertilizer 3.
# Fertilzer 4 showed no significant improvement of plant height.
