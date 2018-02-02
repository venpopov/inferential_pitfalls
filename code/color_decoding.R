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
# VISUALIZE THE COLORS SPACES
# --------------------------------------------------------------------------

plot_ly(vrgb, x=~r, y=~g, z=~b, color=~id, type="scatter3d", mode="markers", 
        marker=list(color=vrgb$color,
                    size=20))
plot_ly(vhsv, x=~h, y=~s, z=~v, color=~id, type="scatter3d", mode="markers", 
        marker=list(color=vhsv$color,
                    size=20))

# --------------------------------------------------------------------------
# MODELLING
# --------------------------------------------------------------------------
# fit encoding model and extract percential rank accuracy
set.seed(345)
pred <- encoding_model(vhsv[1:3], vrgb[1:3])
prank <- get_percentile_rank_acc(vhsv[1:3], pred)

# mean rank accuracy and wilcox test for significance
mean(unlist(prank))
wilcox.test(unlist(prank), mu=0.5)


# --------------------------------------------------------------------------
# MODEL PLOTS
# --------------------------------------------------------------------------
# Plot the predicted colors

preds <- do.call('rbind', pred)
preds <- arrange(preds, id)
preds$color <- vhsv$color
preds <- preds %>% 
  mutate(V2 = ifelse(V2 < 0, 0, ifelse(V2 > 1, 1, V2)),
         V3 = ifelse(V3 < 0, 0, ifelse(V3 > 1, 1, V3)),
         V4 = ifelse(V4 < 0, 0, ifelse(V4 > 1, 1, V4)))
preds <- filter(preds, V2>=0, V2<=1, V3>=0,V3<=1,V4>=0,V4<=1)
preds$predcolor <- hsv(preds$V2, preds$V3, preds$V4)
preds <- left_join(vhsv, preds)
preds <- arrange(preds, v, h, s)

plotdf1 <- data.frame(x=c(1:nrow(preds), 1:nrow(preds)), 
                      y=rep(c(1,2), each=nrow(preds)), 
                      col=c(preds$color, preds$predcolor), 
                      s=c(preds$s, preds$s), 
                      v=c(preds$v,preds$v))
plotdf1 <- plotdf1 %>% 
  mutate(s = round(s,1)) %>% 
  group_by(s,y,v) %>%
  mutate(x = 1:length(x)) %>% 
  filter(s > 0.1, v>=0.4)

plots <- plotdf1 %>% 
  ungroup() %>% 
  mutate(col = as.character(col)) %>% 
  complete(s,v, fill=list(col=0)) %>% 
  group_by(s,v)  %>% 
  do(p={
    dat <- filter(., !is.na(col), col != 0)
    if (!is.null(dat$col)) {
      ggplot(dat, aes(x,y)) + 
        geom_tile(fill=dat$col) +
        theme_void()
        # ggtitle(label=paste0('s=',unique(.$s),' v=',unique(.$v)))
    } else {
      grid::grid.rect(gp=grid::gpar(col="white"))
      # return(0)
    }
  }) 

(f1 <- do.call('grid.arrange', c(plots$p, ncol=7)))
ggsave('figures/color_predictions.tiff', f1, width=10, height=7.5, units='in')
