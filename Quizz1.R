#
# scripts for the resolution of the QUIZZ 1 for Statistical Inference course
#

#
# Question 1
#
# Consider influenza epidemics for two parent heterosexual families. Suppose that 
# the probability is 17% that at least one of the parents has contracted the disease. 
# The probability that the father has contracted influenza is 12% while the probability 
# that both the mother and father have contracted the disease is 6%. 
# What is the probability that the mother has contracted influenza?
#
# (Hints look at lecture 2 around 5:30 and chapter 4 problem 4).
#


# A=Mother, B = Father, P(A∪B)=17%, P(B)=12%, P(A∩B)=6%. Since we know P(A∪B)=P(A)+P(B)−P(A∩B) 

P_ANY <- 17
P_FATHER <- 12
P_BOTH <- 6
P_MOTHER <- P_ANY - P_FATHER + P_BOTH

#
# Question 2
#
# A random variable, X is uniform, a box from 0 to 1 of height 1. 
# (So that its density is f(x)=1 for 0≤x≤1.) What is its 75th percentile?
#
# (Hints, look at lecture 2 around 21:30 and Chapter 5 Problem 5. 
# Also, look up the help function for the qunif command in R.)
#
qunif(0.75, 0, 1)


#
# Question 5
#
# Consider the following PMF shown below in R
#
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp

# What is the mean?
result_mean <- temp[1,1] * temp[2, 1] + temp[1, 2]* temp[2, 2] + temp[1, 3] * temp[2, 3] +
        temp[1, 4]* temp[2, 4]
result_mean2 <- sum(temp["X",] * temp["Prob",])
sum(x*p)

#
# Question 6
#
# A web site (www.medicine.ox.ac.uk/bandolier/band64/b64-7.html) for home 
# pregnancy tests cites the following: “When the subjects using the test were 
# women who collected and tested their own samples, the overall sensitivity 
# was 75%. Specificity was also low, in the range 52% to 75%.” 
# Assume the lower value for the specificity. Suppose a subject has a positive 
# test and that 30% of women taking pregnancy tests are actually pregnant. What 
# number is closest to the probability of pregnancy given the positive test?

# (Hints, watch Lecture 3 at around 7 minutes for a similar example. Also, 
# there's a lot of Bayes' rule problems and descriptions out there, for example 
# here's one for HIV testing. Note, discussions of Bayes' rule can get pretty heady. 
# So if it's new to you, stick to basic treatments of the problem. Also see Chapter 3 Question 5.)

sensivity <- 0.75 # la probabilidad de que el test es positivo cuando el resultado es positivo -> P(+|D)
specifity <- 0.52 # la probabilidad de que el test sea  negativo cuando el resultado es negativo. -> P(-|Dc)

p_d <- 0.3 # P(D)

positive_predictive_vale <- (sensivity * p_d) / ((sensivity * p_d) + ((1 - specifity)* (1 - p_d)))



