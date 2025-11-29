Given that MethodB is cheaper we want to check if its output preserves the quality of the metal rods.
the only measure of quality we have is the tensile strength, and we know MethodA had an average strength of 150 MPa.
Thus we want to test if the new method can keep up with this quality control

We would use a one-sample left tailed t-test with the hypotheses:
H0: mu = 150 (quality is maintained)
H1: mu != 150 (quality has changed)

Assumptions of the t-test are:
Independance: samples (rods) should not affect each other.
Identical conditions(low variance): assumed since all factors in the factory remain unchanged except for the method itself.
approximately normal distribution: We test that in the following snippet of code.


```{r}
library(car)
methodB <- c(152.4, 155.2, 149.6, 153.8, 156.1, 150.9, 154.5, 157.0, 151.8,
             155.6, 150.3, 153.0, 154.2, 156.7, 149.8, 152.9, 155.1, 154.0)

qqPlot(methodB, line = "r", main = "QQ Plot for Method B Values")

shapiro.test(methodB) #shapiro-wlk test of normality.

```
We can see through the QQ Plot and the shapiro test that we have strong evidence that the data is normally distributed, and we know that the t-test would work.

the test statistic formula is t = (mean- mu) / (sd / sqrt(n)) with mu being our hypothesis (150).

```{r}
(t_stat <- (mean(methodB) - 150) / (sd(methodB) / sqrt(25)))
```

Type I Error (False Positive): concluding that the system did not preserve the quality even if mu = 150
in other words, falsely rejecting the hypothesis.

Type II Error (False Negative): concluding that the system did preserve quality even though mu != 150
in other words, false failure to reject hypothesis.


```{r}
#I did the test on multiple alternatives depending on what the engineer wants to test.

#If we wanted to check that we at least have 150 MPa, 
# H1: mu < 150
#t.test(methodB, mu = 150, alternative = "less", conf.level = 0.99) 

#If we wanted to check that we hold that value and it doesn't change much which is my understanding of "perserve"
# H1: mu != 150
t.test(methodB, mu = 150, alternative = "two.sided", conf.level = 0.99)

#If we wanted to check that our tensile has at most 150 MPa,
# H1: mu > 150
#t.test(methodB, mu = 150, alternative = "greater", conf.level = 0.99)


#If the wanted test is for the tensile strength to have a minimum of 150, then we have very strong evidence that it is.

#If the objective was to hold a maximum tensile strength at 150 then we reject the null hypothesis based on the p-value 3.367e-6 << 0.01
#and we have evidence pointing that the mean tensile strength increased
```

If the objective was to test if the new method would hold the tensile strength value, then we reject the null hypothesis based on the p-value 6.735e-6 << 0.01
and we have evidence to point out that the mean tensile strength shifted.



=====================================================================
Since we have two data sets, we can use the paired t-test with the following hypotheses:
  H0: mu_B = mu_A (same strength)
  H1: mu_B > mu_A (methodB produces stronger rods)

  
Assumptions of the paired t-test are similar to the one-sample t-test:
  
Independance: samples (rods) should not affect each other.
Identical conditions(low variance): assumed since all factors in the factory remain unchanged except for the method itself, and they all run for one day each.
approximately normal distribution: We test that in the following snippet of code.


```{r}
machineA <- c(149.2, 151.0, 148.7, 150.1, 149.8, 152.3, 150.5, 151.2,
              148.9, 149.7, 150.9, 149.4)
machineB <- c(153.4, 155.1, 152.8, 154.3, 156.0, 155.6, 153.9, 154.8,
              152.5, 155.2, 154.0, 153.7)
qqPlot(machineA, line = "r", main = "QQ Plot for Machine A Values")
qqPlot(machineB, line = "r", main = "QQ Plot for Machine B Values")


shapiro.test(machineA) #shapiro-wlk test of normality.
shapiro.test(machineB) #shapiro-wlk test of normality.

```

From the QQ plots of the data and the shapiro test pvalues of 0.9514 and 0.8054 >> 0.01,
we can safely assume the data are normally distributed.

Type I Error (False Positive): conclude that machine B does not produce stronger rods even though mu_B > mu_A

Type II Error (False Negative): conclude that machine B produces stronger rods when mu_B = mu_A.


```{r}
t.test(machineB, machineA, paired = TRUE, alternative = "greater", conf.level = 0.99)
```
With a p-value of 3.319e-09 << 0.01, we reject the null hypothesis and we have strong evidence suggesting machine B does in fact produce stronger rods than machine A

=====================================================================
  
Similar to part B, We can do a paired t-test to analyze this data with hypotheses:
  H0: mu_B = mu_A
  H1: mu_B > mu_A

Assumptions:
  
  Independance: samples (rods) should not affect each other. further emphasized since this is a controlled lab environment.
  Identical conditions(low variance): assumed since the same raw materials were used and the sampling was done in a controlled environment.
  approximately normal distribution: We test that in the following snippet of code.
  

```{r}
mtdA <- c(148.5, 149.7, 150.3, 151.1, 149.0, 150.8, 149.5, 150.0, 151.2, 149.8)
mtdB <- c(149.6, 150.5, 151.1, 152.0, 149.2, 151.6, 150.1, 151.0, 152.3, 150.4)
qqPlot(mtdA, line = "r", main = "QQ Plot for Machine A Values")
qqPlot(mtdB, line = "r", main = "QQ Plot for Machine B Values")


shapiro.test(mtdA) #shapiro-wlk test of normality.
shapiro.test(mtdB) #shapiro-wlk test of normality.

```
  
from the QQ plots and the shapiro tests p-values of 0.8371 and 0.9388 >> 0.01, normal distribution assumption is valid.

Type I Error (False Positive): conclude that mtdB does not produce stronger rods even though mu_B > mu_A

Type II Error (False Negative): conclude that mtdB produces stronger rods when mu_B = mu_A.


```{r}
t.test(mtdB, mtdA, paired = TRUE, alternative = "greater", conf.level = 0.99)
```

given that the p-value is 3.685e-6 << 0.01, we reject the null hypothesis and have evidence pointing to the fact that mtdB does in fact produce stronger rods.

=====================================================================
A non-parametric alternative to the  t-test would be the  Wilcoxon sign-ranked test.

The Wilcoxon signed rank test could be thought of as the non-parametric version of the t-test.
it is used to check if the median of the population differs from a specified value

Assumptions:
  Continuous Data: correct assumption given that we found the normality assumption to be true
  Symmetry: Data points are symmetric around the median, which can be seen in the QQ plots. also reasonable since we found normality assumption to be true

  
```{r}
#Non-parametric test for data in part A
#Wilcoxon Signed rank test

#H0: median of methodB = 150
#H1: median of methodB != 150
wilcox.test(methodB,
            mu = 150,
            alternative = "two.sided",
            conf.level = 0.99)

#Non-parametric test for data in part B
#Paired Wilcoxon Signed rank test

#H0: median of machineB = median of machineA
#H1: median of machineB > median of machineA
wilcox.test(machineB, machineA,
            paired = TRUE,
            alternative = "greater",
            conf.level = 0.99)


#Non-parametric test for data in part C
#Paired Wilcoxon Signed rank test

#H0: median of mtdB = median of mtdA
#H1: median of mtdB > median of mtdA
wilcox.test(mtdB, mtdA,
            paired = TRUE,
            alternative = "greater",
            conf.level = 0.99)

```
For part A using the wilcox test, we get a p-value of 5.341e-05 << 0.01 which means we reject the null hypothesis and
have strong evidence that the medians are different; matching our previous parametric test.


For part B using the wlcox test, we get a p-value of  0.001 < 0.01 which means we reject the null hypothesis,
and have evidence that machine B does produce stronger rods; matching our previous parametric test
  
  
For part C using the wlcox test, we get a p-value of  0.002929 < 0.01 which means we reject the null hypothesis,
and have evidence that mtdB does produce stronger rods; matching our previous parametric test
=====================================================================
  
  
```{r}
data <- read.csv("XM125F_Q2.csv", header = TRUE) # read the file
str(data)

data$lubricant <- as.factor(data$lubricant) #make the lubricant a factor instead of character
str(data)

boxplot(friction ~ lubricant, data = data) #boxplot of the friction for each lubricant

points(friction ~ lubricant,data = data, col = "purple", pch = 16) #plot the points on the boxplot

```

For the parametric test, we will use an anova with the hypotheses as follows:
H0: mu_A = mu_B = mu_C = mu_D = mu_E
H1: at least one mean is different.

H0 basically means that all lubricant types are the same and have no effect.

The anova model assumptions are independence, normality, and equal variance.

```{r}
mod1<- aov(friction ~ lubricant, data = data)
summary(mod1)
```

Since the p-value of the anova (1.3e-9) is lower than our signficance level of 0.01
we reject the null hypothesis and we have evidence to say that at least one lubricant is different.

we then find the cell means and standard error.

```{r}
model.tables(mod1, 'mean', se = T)

model.tables(mod1, se = T)

```

Equation of the anova model:
  Y_ij = mu + tau_i + epsilon_ij

Where: Y_ij = weight gain for the j-th observation in the i-th diet group mu = overall mean weight gain
tau_i = effect of the i-th diet (deviation from overall mean) epsilon_ij = random error term i = 1, 2, 3, 4,
5 (diet groups: ctrl, dt1, dt2, dt3, dt4) j = 1, 2, ..., n_i (observations within each group)



we then print out the diagnostic plots:
  
```{r}
par(mfrow=c(2,2))
plot(mod1)
```


From the plot we see the following:
 -Residuals vs fitted: the line is orizantal at or very near 0, which indicates linearity
 -QQ Residuals: The data is aligned with the normal line, which indicates normality.
 -Scale-Location: the line is mostly horizantal with some tails, further testing may be needed to decide if variance is equal (levene test)
 -Constant lev: plot shows no influential outliers

Testing for equal variance:
```{r}
leveneTest(mod1)
```
With a p-value of 0.551 >> 0.01, we fail to reject the null hypothesis and evidence suggests that we have equal variance.


Testing for normality:
```{r}
shapiro.test(rstandard(mod1))
```
with a p-value of 0.6644 >> 0.01, we fali to reject the null hypothesis and evidence suggests that data is normal



Next we use TukeyHSD with confidence level = 0.99 to further analyze the data:

```{r}
(mod1.tky <- TukeyHSD(mod1, conf.level = 0.99))

par(mfrow = c(1,1))
plot(mod1.tky, las = 1)
```

from the printed data, we see that the following pairs have p values > 0.01, therefore
A and D are indistinguishable
B and D are indistinguishable
yet A and B are distinguishable
C and E are indistinguishable

from the difference we see that C has lower friction than A, B, and D and its indistinguishable from E.

therefore if we want lower friction we go with C or E, and for higher friction we go for lubricants A, B, or D



For a non-parametric alternative to the anova we use the Kruskal-Wallis rank sum test
it has the same assumptions of the anova except for the normality requirement.

```{r}
kruskal.test(friction ~ lubricant, data = data)
```

with a p-value of 0.0002605 << 0.01, it leads to the same conclusion as the anova model we did.


Since the objective is to get the lowest amount of friction, we have three major groupings.

-C,E are indistinguishable and they have the lowest friction values.

-A,D are indistinguishable and they have medium friction values.

-B,D are indistinguishable and they have high friction values.

This interaction happens because although D is indistinguishable from A and B, A and B ARE distinguishable from each other.

Thankfully since the objective is to find the lubricants with the lowest friction, we can recommend C,E for best results. 