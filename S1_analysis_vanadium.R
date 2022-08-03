#THE ANALYSIS WAS DONE USING R version 4.1.2 (2021-11-01) -- "Bird Hippie" 
#THE CODE WAS WRITTEN AND FUNCTIONAL USING THE FOLLOWING PACKAGES (incl. their dependencies) UPDATED ON JAN 4, 2022

# Packages - if you have never used those packages you need to install them first. Simply run the function install.packages without the # sign
# NOTE: This can take some time because many packages depend on other packages. Those dependencies will be installed automatically.
# Once a package is installed, it need to be loaded into the current session using the library() function.

#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggridges")
library(ggridges)
#install.packages("glmmTMB")
library(glmmTMB)
#install.packages("brms")
library(brms)
#install.packages("DHARMa")
library(DHARMa)
#install.packages("car")
library(car)
#install.packages("emmeans")
library(emmeans)
#install.packages("cowplot")
library(cowplot)
#install.packages("multcomp")
library(multcomp)

# read data
d <- read_csv("S1_data_cleaned.csv")

d %>% 
  mutate(yr = factor(yr),
         qrt = factor(qrt),
         yr_qrt = paste(yr, qrt, sep = "-"),
         station_id = factor(station_id, levels = c("M2", "M3", "M4", "M5", "M6", "M7"))) -> d1


d1 %>% 
  filter(metals == "vanadium",
         type == "dissolved") -> d_van

# check for replicates in experimental design for all levels
# main effects and interaction not balanced!
replications(~ yr * station_id, data = d_van)

# check repeated measures per sample date and station
# replications function needs its input as factor
d_van$sample_dateF <- factor(d_van$sample_date)

replications(~ sample_dateF : station_id, data = d_van)
replications(~ station_id : sample_dateF, data = d_van)

# exploratory plots for vanadium concentration
# points
d_van %>% 
  ggplot(aes(sample_date, concentration)) +
  geom_point(aes(colour = qrt), position = position_jitter(height = .2)) +
  labs(x = "Sample date", y = "Concentration (ug/L)", colour = "Quarter") +
  facet_wrap(~ station_id) +
  ggtitle("Vanadium raw data over time") -> ex_van1

# boxplots
d_van %>% 
  ggplot(aes(concentration, station_id)) +
  geom_boxplot() +
  labs(x = "Concentration (ug/L)", y = "Station") +
  facet_wrap(~ yr, scales = "free_x") +
  ggtitle("Vanadium raw data by station and year - Boxplots") -> ex_van2

# density curves
d_van %>%
  ggplot(aes(concentration, station_id)) +
  geom_density_ridges(rel_min_height = 0.01) +
  facet_wrap(~ yr, scales = "free_x") +
  labs(x = "Concentration (ug/L)", y = "Station") +
  ggtitle("Vanadium concentration by station and year - Density plots") -> ex_van3

# combine all three plots on one panel
plot_grid(ex_van1, ex_van2, ex_van3,
          labels = c('A', 'B', 'C'),
          label_size = 12, nrow = 3) -> van_plots

# print a png of the combined plots
png("S1_vanadium_rawdata.png", width = 7, height = 12, res = 300, units = "in") 
print(van_plots)
dev.off()

######################### FREQUENTIST ANALYSIS ###########################

# fit GLMM model (unique_id is the random effect to account for multiple transect measurements for each date and location combination)
# for model formulation and various other questions related to mixed effects model refer to: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
fit_van <- glmmTMB(concentration ~ station_id  + yr + (1|unique_id), family = Gamma(log), dispformula = ~station_id  + yr, data = d_van)

# inspect output
summary(fit_van)

# calculate Deviance table to test for significance of fixed effects
Anova(fit_van) # only Year is significant


# run model diagnostics
# some background on diagnostic plots using DHARMa
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# https://stats.stackexchange.com/questions/295340/what-to-do-with-glm-gamma-when-residuals-are-not-normally-distributed
# https://stats.stackexchange.com/questions/307221/why-residual-plots-are-used-for-diagnostic-of-glm

diagnostics <- simulateResiduals(fittedModel = fit_van, plot = F, n = 1000)
plot(diagnostics)

# write out as png
png("S1_vanadium_residuals.png", width = 8, height = 6, res = 300, units = "in") 
plot(diagnostics)
dev.off()


# posthoc tests (only year was significant)
# year
year_means <- emmeans(fit_van, ~ yr, type = "response")
year <- cld(year_means, Letters = letters)

# prepare output for plotting
year %>% 
  as_tibble() %>% 
  mutate(.group = str_remove_all(.group, "\\s")) -> year_sig

# plot mean values with confidence intervals
  ggplot(year_sig, aes(x = yr, y = response)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.25) + 
  geom_text(data = year_sig, aes(x = yr, y = 0, label = .group)) +
  labs(x = "Year", y = "Concentration (ug/L)", title = "Vanadium") -> van_year

#station
station_means <- emmeans(fit_van, ~ station_id, type = "response")
station <- cld(station_means, Letters = letters)

# prepare output for plotting
station %>% 
  as_tibble() %>% 
  mutate(.group = str_remove_all(.group, "\\s")) -> station_sig

# plot means but without significance letters since station_id was not significant
ggplot(station_sig, aes(x = station_id, y = response)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.25) + 
  #station not significant -> won't add letters denoting significant differences
  #geom_text(data = station_sig, aes(x = station_id, y = 0, label = .group)) +
  labs(x = "Station", y = "Concentration (ug/L)", title = "Vanadium") -> van_station

# combine plots
cowplot::plot_grid(van_station, van_year,
          labels = c('A', 'B'),
          label_size = 12, nrow = 2) -> means_van

# write out
png("S1_vanadium_emmeans.png", width = 6, height = 7, res = 300, units = "in") 
print(means_van)
dev.off()


######################### BAYESIAN ANALYSIS ###########################

# check default prior for proposed model
get_prior(concentration ~ station_id  + yr + (1|unique_id), family = Gamma(log), data = d_van)

# fit GLMM model (unique_id is the random effect to account for multiple transect measurements for each date and location combination)
# for model formulation and various other questions related to mixed effects model refer to: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# NOTE: THIS WILL TAKE SOME TIME COMPUTING!!
fit_bayes_van <- brm(concentration ~ station_id  + yr + (1|unique_id), family = Gamma(log), data = d_van)

# generate a summary of the model fit
summary(fit_bayes_van)


# plot the MCMC chains as well as the posterior distributions for all model terms
par(mfrow=c(2,2))
plot(fit_bayes_van, ask = FALSE)

# investigate model fit
loo(fit_bayes_van)
pp_check(fit_bayes_van)


# get means
# year
year_bayes_means <- emmeans(fit_bayes_van, ~ yr, type = "response")

# prepare output for plotting
year_bayes_means %>% 
  as_tibble() -> year_bayes

# plot mean values with confidence intervals
ggplot(year_bayes, aes(x = yr, y = response)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.25) + 
  scale_y_continuous(limits = c(0, 0.515), breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
  labs(x = "Year", y = "Concentration (ug/L)", title = "Vanadium") -> van_bayes_year

# station
station_bayes_means <- emmeans(fit_bayes_van, ~ station_id, type = "response")

# prepare output for plotting
station_bayes_means %>% 
  as_tibble() -> station_bayes

# plot means but without significance letters since station_id was not significant
ggplot(station_bayes, aes(x = station_id, y = response)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.25) + 
  labs(x = "Station ID", y = "Concentration (ug/L)", title = "Vanadium") -> van_bayes_station

# combine plots
cowplot::plot_grid(van_bayes_station, van_bayes_year,
                   labels = c('A', 'B'),
                   label_size = 12, nrow = 2) -> means_bayes_van

# write out
png("S1_vanadium_emmeans_bayes.png", width = 6, height = 7, res = 300, units = "in") 
print(means_bayes_van)
dev.off()
