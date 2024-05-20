#Lauren test

library(ggplot2)
library(dplyr)
library(unmarked)

setwd("~/GitHub/UWIN_tutorials_LP")
setwd("./tutorials/static occupancy") # update to your local folder containing project files or create an R project which will navigate you to this folder
raccoon <- read.csv("chicago_raccoon.csv", head = TRUE, skip = 3) 

# Check out what data we're working with.
head(raccoon)

length(unique(raccoon$Site))

day_cols <- raccoon[,grep("^Day_",colnames(raccoon))]

n_weeks <- ceiling(ncol(day_cols)/6)
week_groups <- rep(1:n_weeks, each = 6)[1:ncol(day_cols)]

combine_days <- function(y, groups){
  ans <- rep(NA, max(groups))
  for(i in 1:length(groups)){
    tmp <- as.numeric(y[groups == i])
    if(all(is.na(tmp))){
      next
    } else {
      ans[i] <- as.numeric(sum(tmp, na.rm = TRUE)>0)
    }
  }
  return(ans)
}

# Apply this function across rows (in groups of 6)
week_summary <- t( # this transposes our matrix
  apply(
    day_cols, 
    1, # 1 is for rows
    combine_days,
    groups = week_groups
  )
)

# update column names
colnames(week_summary) <- paste0("Week_",1:n_weeks)

# drop visits (days) from data.frame
raccoon_wk <- raccoon[,-grep("^Day_", colnames(raccoon))]

# and add occasions
raccoon_wk <- cbind(raccoon_wk, week_summary)

raccoon_wk <- raccoon_wk %>% 
  select(-Week_6)

# read in landcover data
landcover <- read.csv("Chicago_NLCD_landcover.csv", head = TRUE)
head(landcover)

# first we need to make sure 'sites' are named the same to join these datasets
colnames(raccoon_wk)
colnames(landcover)

# we'll go ahead and rename 'sites' to 'Site' in the 'landcover' dataset so these data.frame can communicate
landcover <- rename(landcover, Site = sites)

# Now we can join our datasets and drop NA's. 
raccoon_wk <- left_join(raccoon_wk, landcover, by = 'Site') %>% 
  na.omit(.)

vignette("unmarked")
?unmarkedFrameOccu()

y <- raccoon_wk %>% 
  select(Week_1:Week_5)

siteCovs <- raccoon_wk %>% 
  select(c(water, forest))

ggplot(raccoon_wk, aes(x = water)) +
  geom_histogram() +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  labs(x = "Proportion water", y = "Site count")

ggplot(raccoon_wk, aes(x = forest)) +
  geom_histogram() +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  labs(x = "Proportion forest", y = "Site count") 

# scale covariates
siteCovs <- siteCovs %>% 
  mutate(water_scale = scale(water)) %>% 
  mutate(forest_scale = scale(forest))

siteCovs_df <- data.frame(siteCovs)

# Now we can make our unmarkedFrameOccu() dataframe
raccoon_occ <- unmarkedFrameOccu(y = y, siteCovs = siteCovs_df)

# examine covariate details and site summary
summary(raccoon_occ)

# learn more about this function modeled after MacKenzie et al. (2002)
?occu()

null_model <- occu(~1 # detection
                   ~1, # occupancy
                   data = raccoon_occ)

habitat_model <- occu(~1 # detection
                      ~ forest_scale + water_scale, # occupancy
                      data = raccoon_occ)
# examine model estimates and standard errors
null_model
habitat_model

fitlist <- fitList(m1 = null_model, m2 = habitat_model)
modSel(fitlist)

# We can also use `confit` to calculate the associated error for each estimate
# 95% confidence intervals for occupancy
occ_error <- cbind(coef(null_model, type = "state"),
                   confint(null_model, type = "state"))
# 95% confidence intervals for detection
det_error <- cbind(coef(null_model, type = "det"),
                   confint(null_model, type = "det"))

# Convert confidence intervals back to probability from log-odds estimate
# plogis() = to exp() / 1 + exp()
plogis(occ_error)
plogis(det_error)

# Our naive occupancy
siteValue <- apply(X = y,
                   MARGIN = 1, # 1 = across rows
                   FUN = "max", na.rm = TRUE) # This function finds the max value

mean(siteValue)

# examine the ranges of both data types
range(siteCovs_df$forest)
range(siteCovs_df$forest_scale)

# recreate 'clean' data for plotting later
forest_real <- c(0, 0.5)

# Create a prediction data.frame and make sure to use the same covariate names as included in the occupancy model
dat_plot <- data.frame(
  forest_scale = seq(forest_real[1], forest_real[2], length.out = 400),
  water_scale = 0 # zero because water has been scaled/centered
)

# rescale 'clean' forest data exactly how we did in our model
dat_pred <- dat_plot
dat_pred$forest_scale <- (dat_pred$forest_scale - mean(siteCovs_df$forest)) / sd(siteCovs_df$forest)

# Make predictions with these data
pred_forest <- predict(habitat_model, type = "state", newdata = dat_pred)
head(pred_forest)

plot(pred_forest$Predicted ~ dat_plot$forest_scale, # y-axis ~ x-axis
     type = "l",  # plot out a line
     bty = "l", # box type is an L around plot
     xlab = "Proportion forest", # x label
     ylab = "Occupancy", # y label
     ylim = c(0, 1), # range to y axis
     xlim = c(0,.5),
     lwd = 2, # width of the line
     las = 1 # have numbers on y axis be vertical
)
# add 95% confidence intervals
lines(pred_forest$lower ~ dat_plot$forest_scale, # y-axis ~ x-axis
      lty = 2 # make a checked line
) 
lines(pred_forest$upper ~ dat_plot$forest_scale, # y-axis ~ x-axis
      lty = 2 # make a checked line
)

# first merge the two datasets (predicted occupancy and forest data)
all_dat <- bind_cols(pred_forest, dat_plot)

ggplot(all_dat, aes(x = forest_scale, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) +
  geom_path(size = 1) + # adds line
  labs(x = "Proportion forest", y = "Occupancy probability") +
  ggtitle("Raccoon Occupancy")+
  scale_x_continuous(limits = c(0,.5)) +
  ylim(0,1)+
  theme_classic()+ # drops gray background and grid
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(size = 15), 
        text = element_text(size = 18))




