# This script will generate a fig with ggplot.
# Austin Rutherford
# 2021-10-25
# arutherford@email.arizona.edu

# Overview
# Bring in libraries and data, and ask questions about our data.
# Generate a visualization to address our question
# Play with some bells and whistles


# Bring in libraries
library(tidyverse)
library(remotes)

# Bring in the data
library(palmerpenguins)

data(penguins)

# view the data
summary(penguins)
class(penguins$species)

# Create a subset of penguins using tidyverse's filter and select
# only penguins from biscoe island, only want columns species, bill_length_mm and bill_depth_mm

biscoe <- penguins %>% 
  filter(island == "Biscoe") %>% 
  select(species, bill_length_mm, bill_depth_mm)

# make a plot of penguins using base R
plot(penguins$bill_length_mm, penguins$bill_depth_mm)


# make a plot using ggplot
ggplot(data = penguins, mapping = aes(x = species, y = flipper_length_mm)) +
  geom_bar(stat="identity")

adelie_slice <- penguins %>% 
  filter(species == "Adelie") %>% 
  slice(6:10)

# New ggplot, can you ID penguins by their bill shape?
# Hypothesis: you can, predicition: coloring dots by bill shap, color clusters

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                                      color = species)) +
  geom_point()

penguinplot <- penguins %>% 
  filter(island == "Biscoe") %>% 
  select(species, bill_length_mm, bill_depth_mm) %>% 
  ggplot(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                                        color = species)) +
  geom_point() +
  xlab("Bill length (mm)")+
  ylab("Bill depth (mm)")+
  ggtitle("Length vs depth of penguins")+
  scale_color_discrete(name = "Species")+
  theme_classic()

penguinplot

plotname <- paste0("Output/", Sys.Date(), "_penguinplot.png")

ggsave(filename = plotname, plot = penguinplot, height = 9, width = 9, units = "in", dpi = 300)
        