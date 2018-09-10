#Ch. 3 HW
#data indicate the gender (male=1, female=0) ofofficially reported first and second born children in 100 two-child families.
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0, 0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0, 1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0, 1,0,1,1,1,0,1,1,1,1) 
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0, 1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1, 0,0,0,1,1,1,0,0,0,0)

library(rethinking) 
data(homeworkch3)

#####################################################################################################
#3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?
#####################################################################################################
#Prob(boy) across both birth events will be:
dat <- sum(birth1, birth2)
dat
p_grid <- seq( from=0 , to=1 , length.out=1000 ) 
prior <- rep( 1 , 1000 ) 
plot(p_grid, prior, type="l", xlab="p", col="darkgreen", ylim=c(0, 1))
likelihood <- dbinom( dat , size=200 , prob=p_grid ) 
plot(p_grid, likelihood, type="l", xlab="p", col="darkblue")
posterior <- likelihood * prior 
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type="l", col="blue",xlab="Proportion Male")
MAP<-p_grid[ which.max(posterior) ]
MAP
abline(v=MAP, col="red")


#####################################################################################################
#3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals
#####################################################################################################
samples <- sample( p_grid , prob=posterior , size=10000 , replace=TRUE )
head(samples)
plot(samples)
dens(samples)

# 50%
least<-HPDI(samples , prob=0.5)
least
#Lower = 0.53, Upper = 0.58
abline(v=least, col="red", lty=2)

# 89%
mid<-HPDI(samples , prob=0.89)
mid  
#Lower = 0.50, Upper = 0.61
abline(v=mid, col="blue", lty=2)

# 97%
most<-HPDI(samples , prob=0.97)
most
#Lower = 0.48, Upper = 0.63
abline(v=most, col="darkgreen", lty=2)

legend(0.4, 8, legend=c("50% HPDI", "89% HPDI", "97% HPDI"), col=c("red", "blue", "darkgreen"), lty=2, cex=0.8)


#####################################################################################################
#3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?
#####################################################################################################
sims <- rbinom( 10000 , size=200 , prob=samples) 
table(sims)/10000
simplehist(sims , xlab="Boys Count" )
dens(sims)
abline(v=111, col="blue")
legend(71, 0.035, legend="Actual (111)", col="blue", lty=1, cex=0.8)
# Yes, the model fits the data well. The actual value fell in the peak density of simulations.


#####################################################################################################
#3H4. Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys in the first births, birth1. How does the model look in this light?
#####################################################################################################
data<-sum(birth1)
likelihood1 <- dbinom(data , size=100 , prob=p_grid ) 
plot(p_grid, likelihood1, type="l", xlab="p", col="darkblue")
posterior1 <- likelihood1 * prior 
posterior1 <- posterior1 / sum(posterior1)
plot(p_grid, posterior1, type="l", col="blue",xlab="Proportion Male")
MAP1<-p_grid[ which.max(posterior1) ]
MAP1
abline(v=MAP1, col="red")
# The maximum a posteriori (MAP) is lower than in the multiple birth distribution (it is ~ 51%, rather than ~ 55%). This is because the density distribution uses different data. In above examples, we were considering both birth1 and birth2. The average of these:
mean((birth1+birth2)/2)
#And the avg of only the birth1 data:
mean(birth1)

# If comparing MAP for first births to broader model of multiple births:
sims <- rbinom( 10000 , size=100 , prob=samples) 
table(sims)/10000
dens(sims)
abline(v=51, col="blue")
# So, again, first births alone do not fit the model of multiple births very well.


#####################################################################################################
# 3H5. The model assumes that sex of first and second births are independent. To check this assumption, focus now on second births that followed female first borns. Compare 10,000 simulated counts of boys to only those second births that followed girls. To do this correctly, you need to count the number of first borns who were girls and simulate that many births, 10,000 times. Compare the counts of boys in your simulations to the actual observed count of boys following girls. How does the model look in this light? Any guesses what is going on in these data?
#####################################################################################################
girls1<-sum(1-birth1)
girls1
BafterG<-sum(birth2[birth1 == 0])
BafterG
sims2 <- rbinom( 10000 , size=girls1 , prob=samples) 
dens(sims2)
abline(v=BafterG, col="red")
# The model really falls apart here. Out of the 49 females who were born first, you would expect around 28 of them to have younger male siblings, but we actually have 39 males. This suggests that the sex of the first and second births are not, in fact independent.



