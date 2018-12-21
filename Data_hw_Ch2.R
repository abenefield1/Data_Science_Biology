####################################################################################################
## 2M1
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid

prior <- rep(x = 1, length = length(p_grid))

compute_posterior <- function(w, n, prior, p = p_grid) {
  likelihood <- dbinom(x = w, size = n, prob = p)
  unstd_posterior <- likelihood * prior
  return( unstd_posterior / sum(unstd_posterior) )}


plot_posterior <- function(x, y) {
  pdf(file=“rplot.pdf”,width=350, height=350)
  plot(x = x, y = y, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
  title <- paste( length(x), "Points")
  mtext(title)
  dirname <- "/Desktop/Data_science/hw_1"
  dev.print(pdf, file.path(dirname, paste(name, ".pdf", sep = "")))}




# (1)
name<-"2M1_1"
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)


####################################################################################################
## 2M2
prior <- ifelse(test = p_grid < .5, 0, 1)

# (1)
name<-"2M2_1"
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)
dev.print(pdf, 'Desktop/Data_science/hw_1.png/'paste(name))

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)
dev.print(pdf, 'Desktop/graph2.png')


# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)
dev.print(pdf, 'Desktop/graph3.png')


####################################################################################################
## 2M3
prior <- c(.5, .5)
likelihood <- c(.3, 1)
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)
posterior[1]
round( posterior[1], 2)


####################################################################################################
## 2M4
card_1_likelihood <- 2
card_2_likelihood <- 1
card_3_likelihood <- 0
likelihood <- c(card_1_likelihood, card_2_likelihood, card_3_likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# prob the other side is black = the prob that card_1 was picked
posterior[1] == 2/3


####################################################################################################
## 2M5
card_1_likelihood <- 2
card_2_likelihood <- 1
card_3_likelihood <- 0
card_4_likelihood <- 2
likelihood <- c(card_1_likelihood, card_2_likelihood, card_3_likelihood, card_4_likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# prob the other side is black = the prob that card_1 or card_4 was picked
posterior[1] + posterior[4]


####################################################################################################
## 2M6
card_1_likelihood <- 2
card_2_likelihood <- 1
card_3_likelihood <- 0
likelihood <- c(card_1_likelihood, card_2_likelihood, card_3_likelihood)
prior <- c(1, 2, 3)
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# prob the other side is black = the prob that card_1 was picked
posterior[1] == .5


####################################################################################################
## 2M7
card_1_2_likelihood <- 2
card_2_1_likelihood <- 0
card_1_3_likelihood <- 4
card_3_1_likelihood <- 0
card_2_3_likelihood <- 2
card_3_2_likelihood <- 0

likelihood <- c(card_1_2_likelihood, card_2_1_likelihood, card_1_3_likelihood, card_3_1_likelihood, card_2_3_likelihood, card_3_2_likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# prob the other side of first card is black = the prob that card_1 was picked first, which means we had to pick either (1,2) or (1,3)
posterior[1] + posterior[3] == .75


####################################################################################################
## 2H1

# find posterior for plausibility of each pandas species following the first birth of twins
species_A_likelihood <- .1
species_B_likelihood <- .2
likelihood <- c(species_A_likelihood, species_B_likelihood)
prior <- c(1, 1)
unstd_posterior <- likelihood * prior
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# probability next birth is set of twins
posterior[1] * .1 + posterior[2] * .2



####################################################################################################
## 2H2
species_A_likelihood <- .1
species_B_likelihood <- .2
likelihood <- c(species_A_likelihood, species_B_likelihood)
prior <- c(1, 1)
unstd_posterior <- likelihood * prior
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# probability panda is from species A
posterior[1]



####################################################################################################
## 2H3

species_A_likelihood <- .1 * (1 - .1)
species_B_likelihood <- .2 * (1 - .2)
likelihood <- c(species_A_likelihood, species_B_likelihood)
prior <- c(1, 1)
unstd_posterior <- likelihood * prior
posterior <- unstd_posterior / sum(unstd_posterior)
posterior

# probability panda is from species A
posterior[1]



####################################################################################################
## 2H4

# without birth information
species_A_likelihood <- .8
species_B_likelihood <- 1 - .65
likelihood <- c(species_A_likelihood, species_B_likelihood)
prior <- c(1, 1)
unstd_posterior <- likelihood * prior
posterior_vet_test <- unstd_posterior / sum(unstd_posterior)

# probability panda is from species A, given veterinarian test
posterior_vet_test[1]

# with birth information
species_A_likelihood <- .1 * (1 - .1)
species_B_likelihood <- .2 * (1 - .2)
likelihood <- c(species_A_likelihood, species_B_likelihood)
prior <- c(1, 1)
unstd_posterior <- likelihood * prior
posterior_birth_info <- unstd_posterior / sum(unstd_posterior)

# probability panda is from species A, given vet test & birth info
composite_unstd_posterior <- posterior_vet_test * posterior_birth_info
composite_posterior <- composite_unstd_posterior / sum(composite_unstd_posterior)
composite_posterior[1]

