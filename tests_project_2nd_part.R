data(ToothGrowth)

head(ToothGrowth)

summary(ToothGrowth)

require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

table(ToothGrowth$supp, ToothGrowth$dose)

boxplot(len ~ dose, data = ToothGrowth, main = "Tooth Length distribution by Dose", ylab = "length", xlab = "dose (mgm/day)")
boxplot(len ~ supp, data = ToothGrowth, main = "Tooth Length distribution by Supplement Type", ylab = "length", xlab = "sumplement type")

library(datasets)
data("ChickWeight")
head(ChickWeight)


alpha <- 0.10

# for al dosis
t <- t.test(len ~ supp, paired = FALSE, var.equal = TRUE, data = ToothGrowth, alternative = "two.sided",conf.level = 1-alpha, mu = 6.92)
t$conf.int
t

ToothGrowth$dosef <- as.factor(ToothGrowth$dose)

# different test for each levels of doses
for (i in levels(ToothGrowth$dosef))
{
    s <- subset(ToothGrowth, dosef == i)
    test <- t.test(len ~ supp, paired = FALSE, var.equal = TRUE, data = s, alternative = "two.sided",conf.level = 1-alpha, mu = 0)
    cat("Dose = ",i,"; p-value:",test$p.value, "Confidence intervals:", test$conf.int, "\n") 
}




# Example from https://rexplorations.wordpress.com/2015/08/13/hypothesis-tests-2-sample-tests-ab-tests/

