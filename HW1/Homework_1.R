#Q1_A

words <- c("one", "two", "three", "four", "five")
probabilities <- c(0.16, 0.20, 0.28, 0.20, 0.16)

v1 <- sample(words, 200, replace = TRUE, prob = probabilities)
#Q1_B

fact1 <- factor(v1, levels = c("one", "two", "three", "four", "five"), ordered = TRUE)

#Q1_C
fact2 <- factor(v1, levels = c("one", "two", "three", "four", "five"), labels = c("awful", "poor", "normal", "good", "excellent"), ordered = TRUE)

#Q1_D

colors <- c("yellow", "green", "blue", "red")
v2 <- sample(colors, 200, replace = TRUE)

#Q1_E
fact3 <- factor(v2, levels = c("yellow", "green", "blue", "red"))

#Q1_F
df1 <- data.frame(item1 = fact1, item2 = fact3)
#head(df1)

#Q1_G
table(df1$item1, df1$item2)

#========================================

#Q2_A
N <- 0
S <- 0

while (S <= 1) {
  S <- S + runif(1)
  N <- N + 1
}
N

#Q2_B
k <- 1e6
N_list <- numeric(k)

for(i in 1:k){
  N <- 0
  S <- 0
  
  while (S <= 1) {
    S <- S + runif(1)
    N <- N + 1
  }
  N_list[i] <- N
}
#head(N_list)

#Q2_C

mean(N_list)

error <- abs(mean(N_list) - exp(1))

error

rel_table <- table(N_list) / length(N_list)
rel_table
barplot(rel_table, main = "Relative Frequency of N", ylab = "Proportion")

#As expected the 


