# required package
library(tidyverse)

# create a simulation function for 95% confidence intervals and p-values
simulation <- function(n, mu, stdev) {
  s <- rnorm(n, mu, stdev)
  tibble(
    N = length(s),
    sample_mean = mean(s),
    sample_sd = sd(s),
    sample_se = sample_sd / sqrt(N),
    confint_95 = sample_se * qt(0.975, N - 1),
    t_statistic = (sample_mean - mu) / sample_se,
    p_value = (1-pt(abs(t_statistic), N - 1)) *2
  )
}

# specify parameters and sample size for each draw or experiment
n <- 10 # sample size
mu <- 0 # population mean
stdev <- 5 # population standard deviation


############ SIMULATION ##############

# choose number of repetitions
sim <- 100

# rerun experiment under exact same conditions
rerun(sim, simulation(n, mu, stdev)) %>%
  bind_rows() %>%
  mutate(experiment_id = 1:sim) -> draws


# intervals that do not capture the true population mean are colored red (same as significant p-values)
draws %>% 
  mutate(color_confint = if_else(sample_mean - confint_95 > mu | sample_mean + confint_95 < mu, "red", "black"),
         color_p = if_else(p_value <= 0.05, "red", "black")) -> draws_colored 

# check 95% confidence
draws_colored %>% 
  group_by(color_confint) %>% 
  tally() %>% 
  mutate(prop = n/sum(n)*100)

# plot 95% confidence intervals
ggplot(data = draws_colored, aes(x = sample_mean, y = experiment_id, color = color_confint)) + geom_point() + 
  geom_errorbarh(aes(xmax = sample_mean + confint_95, xmin = sample_mean - confint_95)) + 
  geom_vline(xintercept = mu) +
  scale_colour_manual(name = "95% CI", values = c("black", "red")) +
  labs(x = "Sample means", y = "Experiment ID") + theme(legend.position = "none")

# plot p-values
ggplot(data = draws_colored, aes(x = p_value, y = experiment_id, color = color_p)) + geom_point() + 
  geom_vline(xintercept = 0.05) +
  scale_colour_manual(name = "p-values", values = c("black", "red")) +
  labs(x = "p-values", y = "Experiment ID") + theme(legend.position = "none")

# RERUN CODE HEADER "SIMULATION"
# OUTPUT WILL ALWAYS SLIGHLTY CHANGE SINCE NEW SAMPLES ARE BEING DRAWN RANDOMLY
# ON AVERGAE HOWEVER 5 OUT 100 CONFIDENCE INTERVALS WILL NOT CAPTURE THE TRUE POPULATION MEAN
# -> THE DEFINITION OF A 95% CONFIDENCE INTERVAL
