We first show the data in the form of a histogram and add in an estimated density curve and a normal density curve to compare.


```{r}
data <- read.table("25Fhw4Q1")

mean_emss1 <- mean(data$emss1) 
sd_emss1 <- sd(data$emss1)

#creates a histogram of the data in emss1. probability = true normalizes the y axis so the total value in histo = 1.
hist(data$emss1,
  probability = TRUE,
  main = "CO2 Emissions with New Exhaust System",
  xlab = "CO2 Emissions (g/km)",
  ylab = "Density",
  breaks = seq(min(data$emss1), max(data$emss1) + 4, by = 4)
)

#draw the line of estimated density
lines(density(data$emss1), col = "blue", lwd = 2)

#create a normal density curve
x_vals <- seq(min(data$emss1), max(data$emss1), length = 100)
lines(x_vals, dnorm(x_vals, mean = mean_emss1, sd = sd_emss1),
      col = "red", lwd = 2)


#add in a legend for the two lines
legend("topright",
  legend = c("Kernel Density", "Normal Density"),
  col = c("blue", "red"),
  lwd = 2)


#create a Q-Q plot of the data/
library(car)
qqPlot(data$emss1, line = "r", main = "Normal Q-Q Plot for CO2 Emissions")
```


The arguement line = r makes it a robust regression line. which means it becomes more resistant to outlier data. this gives a better visualisation to check normality
Comparing the histogram and estimated density with the normal curve. there is some difference but normality is possible
We then did the Q-Q test with the points lying close to the normality line. thus the assumption that the data is normally distributed is valid.









```{r}
alpha <- 0.01
n <- length(data$emss1)
t_critical <- qt(1 - alpha, df = n - 1)

upper_bound <- mean_emss1 + t_critical * sd_emss1 / sqrt(n)
c(-Inf, upper_bound)


````

to get the lower one-sided 100 - alpha confidence interval for the mean the region would be mean + t-critical value * sd / sqrt(n)
applied above we found the region to be from - infinity to 191.436, and the reference value of 191.5 doesn't lie in our 99% confidence interval.
that means we have strong evidence pointing that the mean of the new system is less than 191.5 g/km





We use a one-sample left tailed t-test with the hypothesis mu = 191.5 g/km and the alternative hypothesisi mu < 191.5 g/km
the assumptions we make are the following:
Independance: each sample is measured independantly since the runs shouldn't affect each other
Identical conditions (low variance): assumed since we were told identical cars were used under similar conditions in city traffic.
approximately normally distributedd: this was further explored in part A where we used the histogram and Q-Q plot to check normality and we had strong evidence of normality
and given that the sample n is small (25), the t-test is most appropriate.

the test statistic formula is t = (mean - mu) / (sd / sqrt(n)) with mu being our hypothesis (191.5).
```{r}
(mean_emss1 - 191.5) / (sd_emss1 / sqrt(25));
```
and we reject  the hypothesis if t <= 2.5206


Type 1 error (False Positive): concluding that the new system reduces CO2 (rejecting hypothesis) even though mu = 191.5.  
Type 2 error (False Negative): failing to detect reduction (accepting hypothesis) even though mu < 191.5

```{r}
t.test(data$emss1, mu = 191.5, alternative = "less", conf.level = 0.99)
```




The wilcoxon signed rank test could be thought of as the non-parametric version of the one-sample t-test. it is used to check if the median of the population differs
from a specified value (the given 191.5).
Assumptions for this test are similar, but the most important difference is it does not require approximate normality.
Independence: samples are measured independently (satisfed as given).
Continuous distribution: the measured data is continuous.
Symmetry: the data should be symmetrical around the median (shown in the histogram and qq plot in part A).

therefore we choose a H0 to be mu = 191.5, and  H1 to be < 191.5

```{r}
wilcox.test(data$emss1,
            mu = 191.5,
            alternative = "less",
            conf.level = 0.99)
```

we found the p-value to be 0.01032 > 0.01. which means we fail to reject H0. this contradicts our previous result from the t-test but given that
the t-test rejected the hypothesis, and the p-value of the wilcox test was very close to the p-value of rejection (0.01 or less).
we can say that we have stronger evidence that the new system does in fact lower CO2 emissions.












```{r}
data2 <- read.table("25Fhw4Q2")

#create a boxplot of the two sets
boxplot(data2$wt1, data2$wt2,
        names = c("Before", "After"),
        main = "Rat Weights Before and After Drug Treatment",
        ylab = "Weight (grams)",
        col = c("blue", "green"))

#create a plot of the data using wt1 on x and wt2 on y. each rat of the 15 gets one point
plot(data2$wt1, data2$wt2,
     main = "Before vs After Weight per Rat",
     xlab = "Before (wt1)",
     ylab = "After (wt2)",
     pch = 19, col = "purple")

#create a line with slope 1 to show the no difference in weight (before = after)
abline(a = 0, b = 1, col = "red", lwd = 2)
```

The boxplot shows that almost all parametters (mean, sd...) of the weight were reduced in the "after" sample.

The scatter plot shows all but one of the data points being under the no-change line. which means that the majority of rats had a weight reduction.







The hypothesis H0 is that mu = 0 (no weight change), and alternative H1 is mu < 0 (weight reduction).

Since we have before and after sets, this presents what is called paired measurements. we do a quick qqplot to determine normality and choose a proper test
```{r}
diff <- data2$wt2 - data2$wt1
qqPlot(diff, line = "r", main = "Normal Q-Q Plot for the difference in rat weight")
```

Points are close to the qq line and do not curve away with heavy tails. therefore normality assumption is reasonable.

I decided to use a paired-sample t-test with the same required assumptions as the one-sampled version.:
Independence: Rats are independent subjects and do not affect eachother.
Identical conditions: rats in the experiment face similar conditions (sleep time, cage size, etc.), which is more than likely satisfied.
Approximate normality: was concluded using the QQ plot above.


Type 1 error (false positive): conclude the drug reduces weight when it does not.
Type 2 error (false negative): fail to reject hypothesis and conclude drug has no effect.

```{r}
t.test(data2$wt2, data2$wt1,
       paired = TRUE,
       alternative = "less",
       conf.level = 0.99)
```

The test gives a 99% confidence interval (-inf to -1.16625) so we can reject the hypothesis and conclude that there is strong evidence that the drug
causes weight reduction in rats.





Since nothing fundamental changed except for the tested hypothesis, we redo the t.test with the new hypotheses
H0: mu = -2,   H1: mu < -2.

Independence: Rats are independent subjects and do not affect eachother.
Identical conditions: rats in the experiment face similar conditions (sleep time, cage size, etc.), which is more than likely satisfied.
Approximate normality: was concluded using the QQ plot above.

Type I error: Concluding the mean reduction > 2 g when it is not.
Type II error: Failing to detect a true reduction > 2 g.

```{r}
t.test(data2$wt2, data2$wt1,
       paired = TRUE,
       mu = -2,
       alternative = "less",
       conf.level = 0.99)

```

given that the p-value = 0.3343 >> 0.01, we fail to reject H0 therefore there is strong evidence that the weight reduction does not exceed 2 grams.







If normality is in quesiton, we can perform the paired wilcoxon sign-ranked test which has the following assumptions:
Paired observations: each rat is measured before and after the use of the drug
Independence: each rats response to the drug is independent
Symmetry: the data points are symmetrical around the median (shown by the qq plot)

we then perform the test using the two different pairs of hypotheses.


```{r}
wilcox.test(data2$wt2, data2$wt1,
            mu = 0,
            paired = TRUE,
            alternative = "less",
            conf.level = 0.99)
```
This shows a p-value << 0.01 which means we reject the hypothesis H0: mu = 0. and the evidence strongly suggests that the drug is causing weight reduction.



```{r}
wilcox.test(data2$wt2, data2$wt1,
            mu = -2,
            paired = TRUE,
            alternative = "less",
            conf.level = 0.99)
```

This shwos a p-value >> 0.01 which means we fail to reject the hypothesis H0 : mu = -2, and there is no evidence suggesting that the weight reduction is more than 2g.


