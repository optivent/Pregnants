library(tidyverse)

dat <- datasets::anscombe
dat <- data.frame(
  group  = rep(1:4, each = 11),
  x = unlist(dat[,c(1:4)]),
  y = unlist(dat[,c(5:8)])
) %>% as_tibble()

set.seed(111)

noise <- matrix(nrow = 100, ncol = 10) %>% as_tibble() %>% mutate_all(~ runif(100, min = min(dat$y), max = max(dat$y))) 


group2 <- dat %>% filter(group == 2) %>% dplyr::select(-group) %>% dplyr::sample_n(100,replace=T) %>% bind_cols(noise)

lm(y~., data = group2) %>% summary()

library(Boruta)
Boruta(y~.,data = group2, doTrace=2) %>% attStats()


# Not run: 
require(ICEbox)
require(randomForest)


## build a RF:
bhd_rf_mod = randomForest(y ~ ., data = group, importance = TRUE)

## Create an 'ice' object for the predictor "age":
bhd.ice = ice(object = RF2, X = groups[[2]] %>% select(-y), predictor = "x") 

# PDP try
#(devtools::install_github("bgreenwell/pdp"))
library(pdp)

partialPlot(RF2, pred.data = unlist(groups[[2]]), x.var = "y") 



## plot
plot(RF2, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1) 

## centered plot
plot(bhd.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_plot = 1, 
     centered = TRUE) 

## color the curves by high and low values of 'rm'.
# First create an indicator variable which is 1 if the number of 
# rooms is greater than the median:
median_rm = median(X$rm)
bhd.ice$Xice$I_rm = ifelse(bhd.ice$Xice$rm > median_rm, 1, 0)  

plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,  
     x_quantile = T, plot_orig_pts_preds = T, color_by = "I_rm")
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age")             
plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,  
     x_quantile = T, plot_orig_pts_preds = T, color_by = y)            

## End(Not run)

############################# https://bgreenwell.github.io/pdp/articles/pdp.html

library(randomForest)  # for fitting random forests
library(pdp)           # for partial dependence plots
library(vip)           # for variable importance plots

# Fit a random forest to the Boston housing data
set.seed(101)  # for reproducibility
boston_rf <- randomForest(y ~ ., data = group2, importance = TRUE)

# Variable importance plot (compare to randomForest::varImpPlot(boston_rf))
vip(boston_rf, bar = FALSE, horizontal = FALSE, size = 1.5)  # Figure 1

library(ggplot2)
library(pdp)


library(tidyverse)

dat <- datasets::anscombe
dat <- data.frame(
  group  = rep(1:4, each = 11),
  x = unlist(dat[,c(1:4)]),
  y = unlist(dat[,c(5:8)])
) %>% as_tibble()

p0 <- dat %>% ggplot(aes(x,y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(cols = vars(group))

set.seed(111)

groups <- dat %>% group_split(group, keep = FALSE) %>% 
  map(~ .x %>% dplyr::sample_n(100, replace = T) %>% 
        bind_cols(
          matrix(nrow = 100, ncol = 10) %>% as_tibble() %>%
            mutate_all(~ runif(100, min = min(dat$y), max = max(dat$y))) 
        ) 
  ) 

library(randomForest)
RF1 <- randomForest(y ~ ., data = groups[[1]], importance = TRUE)
RF2 <- randomForest(y ~ ., data = groups[[2]], importance = TRUE)
RF3 <- randomForest(y ~ ., data = groups[[3]], importance = TRUE)
RF4 <- randomForest(y ~ ., data = groups[[4]], importance = TRUE)

p1 <- pdp::partial(RF1, pred.var = "x", plot = TRUE, plot.engine = "ggplot2")
p2 <- pdp::partial(RF2, pred.var = "x", plot = TRUE, plot.engine = "ggplot2")
p3 <- pdp::partial(RF3, pred.var = "x", plot = TRUE, plot.engine = "ggplot2")
p4 <- pdp::partial(RF4, pred.var = "x", plot = TRUE, plot.engine = "ggplot2")


library("gridExtra")

grid.arrange(p0,
             arrangeGrob(p1,p2,p3,p4, ncol = 4),
             nrow = 2) 


#####################
## Not run: 
require(ICEbox)
require(randomForest)
require(MASS) #has Boston Housing data, Pima

data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL

## build a RF:
bhd_rf_mod = randomForest(X, y)

## Create an 'ice' object for the predictor "age":
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age",
              frac_to_build = 1) 

## plot
plot(bhd.ice, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1) 

## centered plot
plot(bhd.ice, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, centered = TRUE) 

## color the curves by high and low values of 'rm'.
# First create an indicator variable which is 1 if the number of 
# rooms is greater than the median:
median_rm = median(X$rm)
bhd.ice$Xice$I_rm = ifelse(bhd.ice$Xice$rm > median_rm, 1, 0)  

plot(bhd.ice, frac_to_plot = 1, centered = TRUE, prop_range_y = TRUE,  
     x_quantile = F, plot_orig_pts_preds = T, color_by = "I_rm")
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age",
              frac_to_build = 1)             
plot(bhd.ice, frac_to_plot = 0.5 , centered = F, prop_range_y = F,  
     x_quantile = F, plot_orig_pts_preds = T, color_by = y)            

## End(Not run)


