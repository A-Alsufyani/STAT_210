

data1 <- read.table("weights.txt", header = TRUE) #reads the file, header assumes the first line is column name
str(data1)

any(is.na(data1))          # is.na returns a matching size matrix of the arguement with true values for missing spots.
                           # any() returns true if at least one element in the arguement is true.



data1$diff <- data1$post_weight - data1$pre_weight  #adds a new difference column that is the after - before weight.
head(data1)




data1b <- subset(data1, select = c(gender, age, height, diet, diff)) #makes a subset from the main data frame with only specific columns
str(data1b)




tapply(data1b$diff, data1b$diet, mean)  #calculate the means of the 3 different diet groups
tapply(data1b$diff, data1b$diet, sd)    # calculate the standarad deviations of the 3 different diet groups

# the average change of diets 1 and 2 is an increase of weight which means its not really working.
# also their standard deviation is high which means it might work for a specific group and doesn't work for another group.
# diet 3 had the expected result of a weight losing diet, where the average difference is lower weight (negative) and the 
# standard deviation is low which means it works in a similar way on the entire sample.



tapply(data1b$diff, list(data1b$diet, data1b$gender), mean)
tapply(data1b$diff, list(data1b$diet, data1b$gender), sd)

#According to the data diet 1 works for male individuals, diet 2 seems to not have a major difference, and diet 3 works for both genders.
#the standard deviations shows larger deviations for the first 2 diets and a low deviation for the 3rd one.
#this could mean that the first 2 diets are not as strict as diet 3, allowing for more individual variability which increases the SD.


d1 <- split(data1, data1$gender) #splits the data frame into a list of multiple data frames according to the number of gender options.
str(d1)

ql1 <- quantile(d1$F$height)  #splits the data by cutting it into percentiles with 0% and 100% being the minimum and maximum.
ql2 <- quantile(d1$M$height)
ql2 / ql1  #divides male height percentiles over the female.

#The division shows the factor between the percentile values of each groups height. it shows that all percentile values are >1 which means
#the average height, minimum, and maximum height values.



data1$bmi <- data1$pre_weight / (((data1$height) / 100)^2) #adds a bmi column using its calculation formula.
sum(data1$bmi > 30) # counts the amount of entires with bmi higher than 30.





#Creates a matrix of size 3x5, each entry corresponds to the number
#of successes in 15 trials with prob 0.25
m1 <- matrix(rbinom(15, size = 15, prob = 0.25), nrow = 3, ncol = 5)
head(m1)
#creates a similar matrix with flipped sizes 
#but with a poission dist using lambda = 3
m2 <- matrix(rpois(15, lambda = 3), nrow = 5, ncol = 3)
head(m2)

#creates a vector of negative binom distribution
v1 <- rnbinom(3, size = 2, prob = 0.25)



#create a list using the two matrices and vector.
hwlist <- list(item1 = m1, item2 = m2, item3 = v1)
#matrix multiply the two matrices into another entry (produces 3x3)
hwlist$item4 <- hwlist$item1 %*% hwlist$item2
#matrix multiply in reverse order (produces 5x5)
hwlist$item2 %*% hwlist$item1
head(hwlist)
rm(m1, m2, v1)


#Checks if both sides are equal and that the given formula is correct.
all.equal(t(hwlist$item1 %*% hwlist$item2), t(hwlist$item2) %*% t(hwlist$item1))
#same check for reverse order
all.equal(t(hwlist$item2 %*% hwlist$item1), t(hwlist$item1) %*% t(hwlist$item2))



#adds a 3x3 identity matrix to item 4.
hwlist$item4 <- hwlist$item4 + diag(3)


#solves item4 * x = item3 (skeleton Ax = b)
x <- solve(hwlist$item4, hwlist$item3)
#verifies correct solution, while using the matrix as vector
all.equal(as.vector(hwlist$item4 %*% x), hwlist$item3)

#get the inverse given that A times its inverse gives the identity of that size.
hwlist$item4_inv <- solve(hwlist$item4, diag(3))
#Check if the inverse is correct
all.equal(hwlist$item4 %*% hwlist$item4_inv, diag(3))


