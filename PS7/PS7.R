data <- read.csv("iguanodon.csv")
head(data)
str(data)

plot(bite_st ~ head_length, data = data,
     xlab = "Head Length (cm)",
     ylab = "Bite Strength",
     main = "Bite Strength vs Head Length",
     pch = 19, col = "blue")

model <- lm(bite_st ~ head_length, data = data)
abline(model, col = "red", lwd = 2)
#This plot doesn't show the fit since the axis are zoomed in.


plot(data$head_length, data$bite_st,
     xlab = "Head Length (cm)",
     ylab = "Bite Strength",
     main = "Bite Strength vs Head Length",
     pch = 19, col = "blue",
     xlim = c(min(data$head_length) - 2, max(data$head_length) + 2),
     ylim = c(65, max(data$bite_st) + 12))


abline(model, col = "red", lwd = 2)

summary(model)
predict(model,
        newdata = data.frame(head_length = 90),
        interval = "prediction",
        level = 0.98)
#Assumptions:
#Independence, Constant Variance, Normality. all of these are for errors,


par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


hist(residuals(model), breaks=10,
     freq = FALSE,
     main="Histogram of Residuals",
     xlab="Residuals")
lines(density(residuals(model)),
      col = "blue", lwd = 2)
curve(dnorm(x, mean = mean(residuals(model)), sd = sd(residuals(model))),
      add = TRUE,
      col = "red", lwd = 2)
legend("topright",
       legend = c("Density Curve", "Normal Curve"),
       col = c("blue", "red"),
       lwd = 2)

shapiro.test(residuals(model))
library(car)
ncvTest(model)


data$species <- as.factor(data$species)

model_full <- lm(bite_st ~ head_length * species, data = data)
summary(model_full)

model_reduced_1 <- update(model_full, . ~ . - head_length:species)
anova(model_full, model_reduced_1)
#p value less than 0.05, the interaction term is important and removing it makes the model worse.
#therefore we leave the interaction as is.

model_reduced_2 <- update(model_full, . ~ . - species)
anova(model_full, model_reduced_2)
#p value less than alpha, species term is important

model_reduced_3 <- update(model_full, .~. - head_length)
anova(model_full, model_reduced_3)

shapiro.test(residuals(model_full))
ncvTest(model_full)


#doing the all thing.
model_all <- lm(bite_st ~ length + weight + width + height +
                  leg_length + arm_length + head_length + species,
                data = data)
summary(model_all)

model_red1 <- update(model_all, . ~ . - species)
anova(model_all, model_red1)
summary(model_red1)

model_red2 <- update(model_red1, . ~ . - length)
anova(model_red1, model_red2)
summary(model_red2)

model_red3 <- update(model_red2, . ~ . - weight)
anova(model_red2, model_red3)
summary(model_red3)

model_red4 <- update(model_red3, . ~ . - width)
anova(model_red3, model_red4)
summary(model_red4)

model_red5 <- update(model_red4, . ~ . - arm_length)
anova(model_red4, model_red5)
summary(model_red5)

model_red6 <- update(model_red5, . ~ . - leg_length)
anova(model_red5, model_red6)
summary(model_red6)

model_red7 <- update(model_red6, . ~ . - head_length)
anova(model_red6, model_red7)
summary(model_red7)


shapiro.test(residuals(model_red7))
ncvTest(model_red7)


plot(bite_st ~ head_length, data = data,
     xlab = "Head Length (cm)",
     ylab = "Bite Strength",
     main = "Models on Head Length",
     pch = 19,
     col = ifelse(data$species == "A", "blue", "red"))

abline(a = coef(model_full)["(Intercept)"],
       b = coef(model_full)["head_length"],
       col = "blue", lwd = 2)

abline(a = coef(model_full)["(Intercept)"] + coef(model_full)["speciesB"],
       b = coef(model_full)["head_length"] + coef(model_full)["head_length:speciesB"],
       col = "red", lwd = 2)
legend("left",
       legend = c("Full model (Species A)", "Full Model (Species B)"),
       col = c("blue", "red"),
       lwd = 2)


plot(data$height, data$bite_st,
     xlab = "Height",
     ylab = "Bite Strength",
     main = "Model_red7 (Height)",
     pch = 19,
     col = "blue")

abline(model_red7, col = "red", lwd = 2)


predict(model_full,
        newdata = data.frame(head_length = 90, species = factor("A", levels = c("A", "B"))),
        interval = "prediction",
        level = 0.98)
predict(model_full,
        newdata = data.frame(head_length = 90, species = factor("B", levels = c("A", "B"))),
        interval = "prediction",
        level = 0.98)

