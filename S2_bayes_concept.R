#install.packages("tidyverse")
library(tidyverse)

#install.packages("cowplot")
library(cowplot)

# range of all possible hatching rates from 0 to 1
p_grid <- seq(from = 0, to = 1, length.out = 101)

##### PRIORS ########
# flat prior with no influence = P(Hypothesis)
prior_flat <- rep(1, 101)
plot(p_grid, prior_flat)

# made up example for rainbow trout hatching rates from the literature
# we found that hatching rates are beta distributed with shape parameters 6 and 3
# build informative prior based on literature review
prior_beta <- dbeta(p_grid, 6, 3)
plot(p_grid, prior_beta)

###### LIKELIHOOD #########
# the likelihood = P(Data/Hypothesis)
# our experiment found that 4 out of 10 eggs hatched
# likelihood will shows us what other hatching success rates are plausible given the data
likelihood <- dbinom(4, 10, prob = p_grid)
plot(p_grid, likelihood)

###### PROBABILITY OF THE DATA ######
# the probability of the data = P(Data)
prob_data_flat <- sum(likelihood * prior_flat)
prob_data_beta <- sum(likelihood * prior_beta)

##### POSTERIOR PROBABILITY DISTRIBUTION #######
# Bayes' theorem. The posterior probability with flat prior
posterior_flat <- likelihood * prior_flat / prob_data_flat
plot(p_grid, posterior_flat)
posterior_beta <- likelihood * prior_beta / prob_data_beta
plot(p_grid, posterior_beta)

##### SAMPLING FROM POSTERIOR PROBABILITY DISTRIBUTION #######
# sampling of parameters from p_grid weighted by the posterior
sample_posterior_flat <- sample(p_grid, size = 100000, replace = TRUE, prob = posterior_flat)
hist(sample_posterior_flat)
sample_posterior_beta <- sample(p_grid, size = 100000, replace = TRUE, prob = posterior_beta)
hist(sample_posterior_beta)

##### SOME SIMPLE QUERIES FROM THE SAMPLED POSTERIOR DISTRIBUTION #########
# 95% credible interval
quantile(sample_posterior_flat, c(0.025, 0.975))
quantile(sample_posterior_beta, c(0.025, 0.975))

# probability of observing a hatching success rate equal or less than 0.3
sum(posterior_flat[p_grid <= 0.3])
sum(posterior_beta[p_grid <= 0.3])

# probability of observing a hatching success rate equal or larger than 0.3
sum(posterior_flat[p_grid >= 0.5])
sum(posterior_beta[p_grid >= 0.5])

# combine data into one data frame
bayes_concept <- tibble(p_grid, prior_flat, likelihood, prior_beta, posterior_flat, posterior_beta)


####### GENERATE PLOTS ########
# likelihood
bayes_concept %>% 
  ggplot(aes(x = p_grid, y = likelihood)) +
  geom_line() +
  geom_point(data = bayes_concept %>% 
               dplyr::filter(p_grid %in% seq(from = 0, to = 1, by = 0.1)),
             aes(x = p_grid, likelihood)) +
  geom_vline(aes(xintercept = 0.4), linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(0, 1, length.out = 11))) + 
  labs(x = bquote("Hatching success rate"~theta),
       y = "Likelihood") -> p_likelihood

# flat prior
bayes_concept %>% 
  ggplot(aes(x = p_grid, y = prior_flat)) +
  geom_line() +
  geom_vline(aes(xintercept = 0.4), linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(0, 1, length.out = 11))) +
  labs(x = bquote("Hatching success rate"~theta),
       y = "Prior probability") -> p_prior_flat

# informative prior
bayes_concept %>% 
  ggplot(aes(x = p_grid, y = prior_beta)) +
  geom_line() +
  geom_vline(aes(xintercept = 0.4), linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(0, 1, length.out = 11))) +
  labs(x = bquote("Hatching success rate"~theta),
       y = "Prior probability") -> p_prior_beta


# posterior probability distribution with flat prior

  # These additional two steps are needed to color the area under the curve in the next plot
bayes_concept %>% 
  dplyr::select(p_grid, posterior_flat) %>% 
  dplyr::filter(p_grid < 0.3) -> area_flat

p_flat <- round(sum(posterior_flat[p_grid <= 0.3])*100, 2)
  # Now the actual plot
bayes_concept %>% 
  ggplot(aes(x = p_grid, y = posterior_flat)) +
  geom_area(data = area_flat, aes(x = p_grid), fill = "red", alpha = 0.7) +
  geom_line() +
  geom_vline(aes(xintercept = 0.4), linetype = "dashed") +
  geom_label(aes(x = 0.05, y = 0.005), label = paste0("Pr = ",p_flat," %"), size = 3) + 
  scale_x_continuous(breaks = c(seq(0, 1, length.out = 11))) + 
  labs(x = bquote("Hatching success rate"~theta),
       y = "Posterior probability") -> p_posterior_flat



# posterior probability distribution with informative prior

  # These additional two steps are needed to color the area under the curve in the next plot
bayes_concept %>% 
  dplyr::select(p_grid, posterior_beta) %>% 
  dplyr::filter(p_grid < 0.3) -> area_beta

p_beta <- round(sum(posterior_beta[p_grid < 0.3])*100, 2)

  # Now the actual plot
bayes_concept %>% 
  ggplot(aes(x = p_grid, y = posterior_beta)) +
  geom_area(data = area_beta, aes(x = p_grid), fill = "red", alpha = 0.7) +
  geom_line() +
  geom_vline(aes(xintercept = 0.4), linetype = "dashed") +
  geom_label(aes(x = 0.2, y = 0.005), label = paste0("Pr = ", p_beta, " %"), size = 3) + 
  scale_x_continuous(breaks = c(seq(0, 1, length.out = 11))) + 
  labs(x = bquote("Hatching success rate"~theta),
       y = "Posterior probability") -> p_posterior_beta



(plot_grid(p_likelihood, p_prior_flat, p_posterior_flat,
          labels = c('A', 'B','C'),
          label_size = 12, ncol = 1, align = "v") -> example_bayes_flat)

#png("S2_FIG1.png", width = 6, height = 8, res = 300, units = "in")
#print(example_bayes_flat)
#dev.off()

(plot_grid(p_likelihood, p_prior_beta, p_posterior_beta,
          labels = c('A', 'B','C'),
          label_size = 12, ncol = 1, align = "v") -> example_bayes_beta)

#png("S2_FIG2.png", width = 6, height = 8, res = 300, units = "in")
#print(example_bayes_beta)
#dev.off()

#### END ####