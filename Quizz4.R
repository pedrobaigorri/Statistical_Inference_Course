#
# scripts for the resolution of the QUIZZ 4 for Statistical Inference course
#

# QUESTION 1
# A pharmaceutical company is interested in testing a potential blood pressure 
# lowering medication. Their fi􀃒rst examination considers onlysubjects that receiv
# ed the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)


#Subject Baseline Week 2
#1 140 132
#2 138 135
#3 150 151
#4 148 146
#5 135 130

# Consider testing the hypothesis that there was a mean reduction in 
# blood pressure? Give the P-value for the associated two sided T test.

bl <- c(140, 138, 150, 148, 135)
fu <- c(132, 135, 151, 146, 130)

t.test(fu, bl, alternative = "two.sided", paired = TRUE)

# QUESTION 2
# A sample of 9 men yielded a sample average brain volume of 1,100cc and a 
# standard deviation of 30cc. What is the complete set of values of mu0 that a test
# of H0: mu = m0 would fail to reject the null hypothesis in a two sided 5% Students t-test?

n <- 9
mu <- 1100
sd <- 30

round(mu + c(-1, 1) * qt(0.975, n - 1) * 30/sqrt(n))

# QUESTION 3
# Researchers conducted a blind taste test of Coke versus Pepsi. Each of four 
# people was asked which of two blinded drinks given in random order that they
# preferred. The data was such that 3 of the 4 people chose Coke. 
# Assuming that this sample is representative, report a Pvalue
# for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

# solution
# Let be the proportion of people who prefer Coke. Then, we want to test

# H0: p = 0.5 versus Ha: p >0.5. Let X be the number out of that prefer Coke; assume X ∼ Binomial(p, .5).

pbinom(2, size = 4, prob = 0.5, lower.tail = FALSE)
choose(4, 3) * 0.5^4 + choose(4, 4) * 0.5^4

# QUESTION 4
# Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A
# hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is
# the one sided P-value for the relevant test of whether the hospital is *below* the standard?


# solution H0 : λ = 1/100 Ha : λ < 1/100 X = 11 t = 1,787 X ∼H0 Poisson(0.01×t)
lambda <- 1/100
x <- 10
t <- 1787

round(ppois(x, lambda = lambda * t), 2)

# QUESTION 5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were
# measured at a baseline and again after having received the treatment or placebo for four weeks. The average di􀃗erence from follow-up to
# the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard
# deviations of the dif􀃗erences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear
# to diff􀃗er between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a
# pvalue for a two sided t test.



n1 <- n2 <- 9
x1 <- -3 ##treated
x2 <- 1 ##placebo
s1 <- 1.5 ##treated
s2 <- 1.8 ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
ts <- (x1 - x2)/(s * sqrt(1/n1 + 1/n2))
2 * pt(ts, n1 + n2 - 2)

# QUESTION 7
# Researchers would like to conduct a study of 100 healthy adults to detect a 
# four year mean brain volume loss of .01 mm3. Assume that the standard deviation 
# of four year volume loss in this population is .04 mm3. About what would be the
# power of the study for a 5% one sided test versus a null hypothesis of no volume loss?

n <- 100
mu0 <- 0.01
sd0 <- 0.04
z95 <- 1.645
pnorm(z95 * 0.004, mean = 0.01, sd = 0.004, lower.tail = FALSE)


alpha = 0.05
z = qnorm(1 - alpha)
pnorm(z * sd0/sqrt(n), mean = mu0, sd = sd0/sqrt(n), lower.tail = FALSE)


# QUESTION 8
# Researchers would like to conduct a study of n healthy adults to detect a four 
# year mean brain volume loss of .01 mm3. Assume that the standard deviation of 
# four year volume loss in this population is .04 mm3. About what would be the 
# value of n needed for 90% power of type one error rate of 5% one sided test versus
# a null hypothesis of no volume loss?

mu0 <- 0.01
sd0 <- 0.04
power <- 0.90
error <- 0.05

x <- sd0/mu0

ceiling((x * (qnorm(1 - error) - qnorm(1 - power)))^2)

