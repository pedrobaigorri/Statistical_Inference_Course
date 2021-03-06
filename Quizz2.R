#
# scripts for the resolution of the QUIZZ 2 for Statistical Inference course
#

#
# Question 1
#
# What is the variance of the distribution of the average an IID draw of n 
# observations from a population with mean μ and variance σ2.
#

print("sigma cuadrado /n")

# Question 2
#
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally 
# distributed with a mean of 80 (mm Hg) and a standard deviation of 10. 
# About what is the probability that a random 35-44 year old has a DBP less than 70?

pnorm(70, mean = 80, sd = 10, lower.tail = TRUE)

# Question 3
#
# Brain volume for adult women is normally distributed with a mean of about 
# 1,100 cc for women with a standard deviation of 75 cc. What brain volume 
# represents the 95th percentile?

qnorm(0.95, mean = 1100, sd = 75)

# Question 4
#
# Refer to the previous question. Brain volume for adult women is about 1,100 cc
# for women with a standard deviation of 75 cc. Consider the sample mean of 100 
# random adult women from this population. What is the 95th percentile of the 
# distribution of that sample mean?

# z = (X - Mu) / (SD/sqrt n) where z for 95th = 1.645
# z(SD/ sqrt n) = X - Mu
# X = z (SD/ sqrt n) + Mu
(1.645 * 75 / (sqrt(100))) + 1100

n <- 100
qnorm(0.95, mean = 1100, sd = (75/(sqrt(n))))

# Question 5
#
# You flip a fair coin 5 times, about what's the probability of getting 4 or 5 heads?

round(pbinom(3, 5, 0.5, lower.tail = FALSE ) * 100)


# Question 6
#
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for
# a specific population has a mean of 15 (sleep events per hour) and a standard 
# deviation of 10. They are not normally distributed. Give your best estimate
# of the probability that a sample mean RDI of 100 people is between 14 and 16 events per hour?

μ <- 15
σ <- 10
n <- 100
SE <- σ/sqrt(n)

left <- 14
right <- 16

percentageLeft <- pnorm(left, mean = μ, sd = SE) * 100
percentageRight <- pnorm(right, mean = μ, sd = SE) * 100

probPercentage <- round(percentageRight - percentageLeft)
probPercentage

# Question 7
#
# Consider a standard uniform density. The mean for this density is .5 and the 
# variance is 1 / 12. You sample 1,000 observations from this distribution and 
# take the sample mean, what value would you expect it to be near?

print("Via the LLN it should be near .5")

# Question 8
# 
# The number of people showing up at a bus stop is assumed to be Poisson with 
# a mean of 5 people per hour. You watch the busstop for 3 hours. 
# About what's the probability of viewing 10 or fewer people?

round(ppois(10, lambda = 5 * 3) * 100)
