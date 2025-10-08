## ----------------------------
## Problem 1 — Bending strength
## ----------------------------

## (0) Read data
dat <- read.csv("PS3/PL3_P1.csv", header = TRUE)
str(dat)
bs1 <- dat$bs1
ref <- 2.1
n   <- length(bs1)

## (a) Graphical exploratory analysis
par(mfrow = c(1,3))

# Boxplot with horizontal red line at reference
boxplot(bs1, horizontal = TRUE, main = "Boxplot of bs1",
        xlab = "Bending strength (N-m/cm)")
abline(v = ref, col = "red", lwd = 2)

# Stripchart with vertical red bar at reference
stripchart(bs1, method = "jitter", pch = 19,
           main = "Stripchart of bs1",
           xlab = "Bending strength (N-m/cm)")
abline(v = ref, col = "red", lwd = 2)

# Dotchart using sorted values + red reference line
bs1_sorted <- sort(bs1)
dotchart(bs1_sorted, pch = 19, xlab = "Bending strength (N-m/cm)",
         main = "Dotchart of sorted bs1")
abline(v = ref, col = "red", lwd = 2)

par(mfrow = c(1,1))

## Quick numeric summary for context:
summary(bs1)
mean_bs1 <- mean(bs1); sd_bs1 <- sd(bs1)
cat(sprintf("\nMean = %.3f, SD = %.3f, n = %d\n", mean_bs1, sd_bs1, n))

## (b) Normality checks: histogram/density, QQ plot, Shapiro-Wilk
par(mfrow = c(1,3))

hist(bs1, breaks = "FD", main = "Histogram of bs1",
     xlab = "Bending strength (N-m/cm)", probability = TRUE)
lines(density(bs1), lwd = 2)

qqnorm(bs1, main = "Normal Q-Q plot for bs1")
qqline(bs1, col = "red", lwd = 2)

# Shapiro-Wilk test
par(mfrow = c(1,1))
shapiro_res <- shapiro.test(bs1)
shapiro_res

## (c) Sampling distribution & 98% lower one-sided CI for the mean
## Assumption: bs1 ~ Normal(μ, σ^2)
## Sampling distribution of the standardized mean uses t with df = n-1:
##   ( \bar{X} - μ ) / (S / sqrt(n)) ~ t_{n-1}
alpha <- 0.02               # 98% one-sided lower CI
t_star <- qt(1 - alpha, df = n - 1)
se     <- sd_bs1 / sqrt(n)
lower_98 <- mean_bs1 - t_star * se
cat(sprintf("\n98%% lower one-sided CI for μ: [%.3f, +Inf)\n", lower_98))
cat(sprintf("Reference value %.2f is %s this interval.\n",
            ref, ifelse(ref >= lower_98, "inside", "below")))

## (d) Parametric one-sample t-test (H0: μ = 2.1 vs H1: μ < 2.1)
## Assumptions: i.i.d., approximately normal (or n moderately large).
t_res <- t.test(bs1, mu = ref, alternative = "less")
t_res

## For clarity, extract key pieces:
Tstat <- (mean_bs1 - ref) / se
cat(sprintf("\nOne-sample t-test details:\n  t = %.3f, df = %d, p-value = %.4f\n",
            Tstat, n - 1, t_res$p.value))
cat("Type I error (α): chosen test significance level.\n")
cat("Type II error (β): failing to reject H0 when μ < 2.1; depends on true μ.\n")

## (e) Nonparametric tests
## (1) Wilcoxon signed-rank test against median = 2.1, H1: median < 2.1
## Assumptions: data are symmetric about the median (less strict than normality).
## Note: wilcox.test ignores exact p with ties; that’s fine here.
wilcox_res <- suppressWarnings(wilcox.test(bs1, mu = ref, alternative = "less", exact = FALSE, conf.int = TRUE, conf.level = 0.98))
wilcox_res

## (2) Sign test via binom.test (counts above vs below ref, ignoring ties)
pos <- sum(bs1 > ref)
neg <- sum(bs1 < ref)
ties <- sum(bs1 == ref)
n_eff <- pos + neg
cat(sprintf("\nSign test counts: >ref = %d, <ref = %d, ties = %d, effective n = %d\n", pos, neg, ties, n_eff))
## H0: P(X > ref) = 0.5 vs H1: P(X > ref) < 0.5
sign_res <- binom.test(pos, n_eff, p = 0.5, alternative = "less")
sign_res

## ----------------------------
## Light interpretation helpers
## ----------------------------
cat("\n--- Quick interpretation guide ---\n")
cat("* (a) If many points lie below the red line (2.1), that’s a visual flag.\n")
cat("* (b) If QQ-plot is roughly linear and Shapiro p > 0.05, normality is plausible.\n")
cat("* (c) The 98% lower CI gives a guaranteed lower bound for the mean; if it’s well below 2.1,\n")
cat("      we cannot claim the mean meets the reference at 98% confidence.\n")
cat("* (d) If the one-sided t-test p-value is small (e.g., < 0.05), there’s evidence mean < 2.1.\n")
cat("* (e) Wilcoxon/sign tests check the median and are more robust; compare their p-values with (d).\n")

