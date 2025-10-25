library(palmerpenguins)


table(penguins$species) #extract species to a table

table(penguins$island) # extract islands to a table

sum(is.na(penguins)) # find the sum of empty values in penguins

#==================================================
head(penguins)

boxplot(body_mass_g ~ sex + species, data = na.omit(penguins),
        col = c("blue", "green", "red"),
        main = "Body Mass by Sex and Species",
        ylab = "Body Mass (g)")

boxplot(bill_depth_mm ~ sex + species, data = na.omit(penguins),
        col = c("blue", "green", "red"),
        main = "Bill Depth by Sex and Species",
        ylab = "Bill Depth (mm)")

