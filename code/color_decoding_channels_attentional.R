# --------------------------------------------------------------------------
# PRELIMINARIES
# --------------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(class)
library(MASS)
library(caret)
library(reshape2)
library(plotly)
library(gridExtra)
library(here)
setwd(here()) # set wd as the parent folder which contains an .rproj file
source('code/functions.R')

# --------------------------------------------------------------------------
# GENERATE DATA
# --------------------------------------------------------------------------
# sample colors from RGB space
values <- seq(0.1,1,length=10)
vrgb <- expand.grid(r=values, g=values, b=values)
vrgb$color <- rgb(vrgb$r, vrgb$g, vrgb$b)

# transform to HSV
vhsv <- t(rgb2hsv(t(select(vrgb, -color)), maxColorValue = 1))
vhsv <- as.data.frame(vhsv)
vhsv$color <- vrgb$color

# --------------------------------------------------------------------------
# RESULTS
# --------------------------------------------------------------------------

#correlation among dimensions
cor(vrgb[1:3],vhsv[1:3]) %>% round(2) %>% abs() %>% apply(1, mean)


# run simulation for only one set of parameters
res <- replicate(30, run_color_simulation(1,0.075,0.5), simplify = FALSE)

do.call('rbind', res) %>% 
  ggplot(aes(att, acc, color=model, linetype=model, shape=model, group=model)) +
  stat_summary() +
  stat_summary(geom='line') +
  theme_classic() +
  xlab('Attention modulation condition') +
  ylab('Mean rank accuracy')

# ----

# run simulation for a grid of noise and selectivity parameters
# it takes 2-5 hours, depending on the machine. The results are saved in the
# 'simulation_results.RData' file, so they can be loaded directly

# sim_res <- expand.grid(noise_sd=seq(0.025,0.2,0.025), att_select_sd=seq(0.2,0.9,0.1)) %>% 
#   group_by(noise_sd, att_select_sd) %>% 
#   do({res = replicate(30, run_color_simulation(1, .$noise_sd, .$att_select_sd), simplify = FALSE)
#       do.call('rbind',res)})
# save(sim_res, file='output/simulation_results.RData')

load('output/simulation_results.RData')


# ----

sim_res %>% 
  filter(noise_sd > 0.025, att_select_sd <= 0.8) %>% 
  ggplot(aes(att, acc, color=model, linetype=model, shape=model, group=model)) +
  stat_summary() +
  stat_summary(geom='line') +
  theme_classic() +
  xlab('Attention modulation condition') +
  ylab('Mean rank accuracy') +
  facet_grid(noise_sd~att_select_sd) +
  scale_x_discrete(labels=c('no','r','g','b')) +
  theme(legend.position = 'bottom')

ggsave('figures/attentional_modulation_par_space.tiff', units='in', width=7.5, height=8, compression="lzw")

