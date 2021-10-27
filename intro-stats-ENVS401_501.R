# Intro to Stats in R for ENVS 401/501
# Austin Rutherford
# Date: 2021-10-25
# Email: arutherford@email.arizona.edu

# load (or install) needed packages
#install.packages("palmerpenguins")

library(palmerpenguins)
library(tidyverse)

# examine your data
head(penguins)

glimpse(penguins)

summary(penguins)

# A question I have, is there a difference in body mass between the Adelie and Gentoo penguin species?

# base R
boxplot(formula = body_mass_g ~ species, data = penguins)

# ggplot
penguins %>% 
  ggplot(mapping = aes(x = species, y = body_mass_g)) + 
  geom_boxplot()

# t-test means between two groups, our groups = Adelie and Gentoo
adelie <- penguins %>% 
  filter(species == "Adelie")

glimpse(adelie)

# visualize normality assumption
hist(adelie$body_mass_g)

# quick assessment of homogeneity of variance assumption, gotta check for NAs, sd don't like em
adelie %>% na.omit() %>% 
  summarise(sd_mass = sd(body_mass_g))

# extract only gentoo data
gentoo <- penguins %>% 
  filter(species == "Gentoo")

glimpse(gentoo)

# visualize normality assumption
hist(gentoo$body_mass_g)

# quick look at homogeneity of variance
gentoo %>% na.omit() %>% 
  summarise(sd_mass = sd(body_mass_g))

# ready to do a t-test
penguin.mass.test <- t.test(x = adelie$body_mass_g, y = gentoo$body_mass_g)

# look at output of t-test
penguin.mass.test

# save outputs to a text file, really helpful for reporting later
sink(file = "Output/penguin-mass-t-test.txt")
penguin.mass.test
sink()

# different data types in R, factor = groups or categorical data
glimpse(penguins)

# Check how many groups?
# Names of groups
levels(penguins$species)
# Count the groups
nlevels(penguins$species)

# Analysis of Variance, hypothesis: penguin species have different mean body masses, null H: no difference
penguin.mass.aov <- aov(formula = body_mass_g ~ species, data = penguins)

penguin.mass.aov

summary(object = penguin.mass.aov)

# Now, we have tested and see there is a difference b/w species, but which species?
# post-hoc test for within group pairwise differences
TukeyHSD(x = penguin.mass.aov)

# Does the post-hoc test make sense? View boxplots again.
penguins %>% 
  ggplot(mapping = aes(x = species, y = body_mass_g)) + 
  geom_boxplot()

# Save the output of the ANOVA to a text file
sink(file = "Output/penguin-mass-anova.txt")
summary(object = penguin.mass.aov)
TukeyHSD(x = penguin.mass.aov)
sink()

# Next, simple linear regression, let's look at some new data

# Use a data set called gapminder, which is a Swedish non-profit that aggregates
# World data to allow people to ask global social questions

# install.packages("gapminder")
library(gapminder)

# Look at data
glimpse(gapminder)
summary(gapminder)

# Linear regression is about understanding relationships
# What's the relationship between wealth and life expectancy?
# What columns would I need to answer this question (lifeExp, gdpPercap)
# Also, if I know the gdp, can I accurately predict the mean life expectancy?

# close to normal, not horrible
hist(gapminder$lifeExp)

# wooooow that's not normal
hist(gapminder$gdpPercap)
# fail normality assumption, know we're gonna have to address this

# is there a linear relationship, nooo
gapminder_fig <- gapminder %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  #scale_color_manual(values = c("blue", "darkorange", "darkgreen", "darkred", "purple")) +
  theme_classic()

gapminder_fig

# we can transform data to conform to the SLR assumptions
hist(gapminder$gdpPercap)

# log is one of the most common ways, but also sqrt, exp
hist(log10(gapminder$gdpPercap))

# create a new column of log transformed data, do we remember the tidyverse function?
gapminder_log <- gapminder %>% 
  mutate(gdp_log = log10(gdpPercap))

head(gapminder_log)

# look at the general relationship between life exp and log gdp
gapminder_log_fig <- gapminder_log %>% 
  ggplot(mapping = aes(y = lifeExp, x = gdp_log, color = continent)) +
  geom_point() +
  #scale_color_manual(values = c("blue", "darkorange", "darkgreen", "darkred", "purple")) +
  theme_classic()

gapminder_log_fig

# Meeting assumption, now we can do the test/model
# test the relationship
gdp_v_life_model <- lm(formula = lifeExp ~ gdp_log, data = gapminder_log)

gdp_v_life_model

summary(gdp_v_life_model)

# look at line
gapminder_lm_fig <- gapminder_log %>% 
  ggplot(mapping = aes(y = lifeExp, x = gdp_log, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  #scale_color_manual(values = c("blue", "darkorange", "darkgreen", "darkred", "purple")) +
  theme_classic()

gapminder_lm_fig

# Save the output of the LM to a text file
sink(file = "Output/log-gdp-life-lm.txt")
summary(object = gdp_v_life_model)
sink()

# lifeExp = -9.1 + 19.35*log10(gdpPercap)



