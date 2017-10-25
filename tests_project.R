#script for testing things for the inference statistics project


hist(runif(1000))


mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)



# setting the main attributes
lambda <- 0.2
n <- 40


# calculating the theorical mean and variance
th_mean <- 1/lambda
th_var <- (1/(lambda^2))/n

# creating a data frame to store the data of distribution, mean and variance for the different simulations
df_mean <- data.frame(th_mean, "Theoretical")
colnames(df_mean) <- c("mean", "size")

df_var <- data.frame(th_var, "Theoretical")
colnames(df_var) <- c("variance", "size")

df_dist <- data.frame(x = numeric(), size = character())


#
# loop for simulations of 100, 1000, 10000
#
simulations <- c(100, 1000, 10000)


for (j in simulations){
    
    mns = NULL
    
        for (i in 1 : j) mns = c(mns, mean(rexp(n, lambda)))
    
    # setting the data of distribution
    df_dist_t <- data.frame(mns, as.factor(j))
    colnames(df_dist_t) <- c("x", "size")
    df_dist <- rbind (df_dist, df_dist_t)
    
    #setting the data of means
    df_mean_t <- data.frame(mean(mns), as.factor(j))
    colnames(df_mean_t) <- c("mean", "size")
    df_mean <- rbind(df_mean, df_mean_t)
    
    #setting the data of variances
    df_var_t <- data.frame(var(mns), as.factor(j))
    colnames(df_var_t) <- c("variance", "size")
    df_var <- rbind(df_var, df_var_t)
    
}


# plotting distribution charts
g <- ggplot(df_dist, aes(x = x, fill = size)) + 
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 

g + facet_grid(. ~ size) + ggtitle("Distribution charts for different set of simulations")

# plotting mean charts
df_mean$size <- factor(df_mean$size, levels = c("100", "1000", "10000", "Theoretical"))

g <- ggplot(df_mean, aes(size, mean)) + geom_bar(stat = "identity", aes(fill = size)) +
    ggtitle ("Means values for different set of simulations")

g +  geom_text(aes(label = round(mean, 3), y = mean - 0.5* mean))


# plotting variance charts
df_var$size <- factor(df_var$size, levels = c("100", "1000", "10000", "Theoretical"))

g <- ggplot(df_var, aes(size, variance)) + geom_bar(stat = "identity", aes(fill = size)) +
    ggtitle ("Variance values for different set of simulations")

g +  geom_text(aes(label = round(variance, 3), y = variance - 0.5* variance))
