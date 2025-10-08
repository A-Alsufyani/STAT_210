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
)


















