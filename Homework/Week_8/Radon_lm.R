radon_dat <- read.csv("radon_MN.csv", as.is=TRUE)
radon_dat$log_radon <- log(ifelse(radon_dat$radon==0,0.1,radon_dat$radon))
radon_dat$county <- factor(radon_dat$county)
radon_dat$floor_x <- ifelse(radon_dat$floor=="basement",0,1)
head(radon_dat)
uranium_dat <- read.csv("radon_MN_U.csv", as.is=TRUE)
head(uranium_dat)
uranium_dat$logu <- log(uranium_dat$uppm)
for ( i in 1:nrow(radon_dat) ) {
  radon_dat$logu[i] <- uranium_dat$logu[uranium_dat$county==radon_dat$county[i]]
}
radon_dat[sample(1:nrow(radon_dat),50),] #print a sample of 50 rows to check
ppfit_bayesR <- stan_lmer( log_radon ~ floor_x + logu + (1|county), data=radon_dat )
samples <- extract(ppfit_bayesR$stanfit)
names(samples)
str(samples$alpha) ##Samples of overall mean. Matrix: samples by row, 1 col
str(samples$b) #Samples of state deviations. Matrix: samples by row, 50 cols
str(samples$beta)
head(samples$beta)
