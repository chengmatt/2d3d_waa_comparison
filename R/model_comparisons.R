# Purpose: To extract outputs from models and compare 
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date: 5.25.23

margAIC <- function(optim_model) {
  
  # Get number of parameters 
  k <- length(optim_model[["par"]])
  
  # Extract objective function
  nLL <- optim_model[["objective"]]
  
  # Calculate AIC
  margAIC <- 2*k + 2*nLL
  
  return(margAIC)
}

# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(TMB)
library(cowplot)

load(here("output", "2d3d_models.RData")) # obj is called models

# Load in WAA matrix (only use fishery data)
waa_df <- read.csv(here("data", "ebs_waa.csv")) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source)

# Load in std for WAA matrix
waa_std_df <- read.csv(here("data", "ebs_waa_std.csv")) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source)


# Get WAA Values ----------------------------------------------------------

# Dimensions
years <- 1991:2024 # Add projection years
ages <- 3:15
model_names <- c("3d_gmrf", "2d_gmrf", "2d_ar1")

# Empty dataframe to store values in
WAA_re_df_all <- data.frame()
mean_WAA_all <- data.frame()

# extract waa_re values
for(model in 1:length(models)) {
  
  # coerce these values into a matrix
  WAA_re <- matrix(
    t(exp(models[[model]]$env$parList()$ln_Y_at)),
    ncol = length(ages), nrow = length(years)
  )
  
  # standard deviations
  WAA_sd <- matrix(
    t(sqrt(models[[model]]$sd_rep$diag.cov.random)),
    ncol = length(ages), nrow = length(years)
  )
  
  # Next, melt into a dataframe
  WAA_re_df <- reshape2::melt(WAA_re)
  colnames(WAA_re_df) <- c("yrs", "ages", "vals")
  WAA_re_df$model <- model_names[model] # input model name for indexing
  WAA_re_df$ages <- WAA_re_df$ages + 2 # Adding the true start age back
  WAA_re_df$yrs <- WAA_re_df$yrs + 1990 # Adding the true start year back
  
  # do the same for the sds
  WAA_re_sd_df <- reshape2::melt(WAA_sd)
  colnames(WAA_re_sd_df) <- c("yrs", "ages", "sd")
  WAA_re_df$sd <- WAA_re_sd_df$sd
  
  mean_waa <- reshape2::melt(models[[model]]$rep$mu_at[,1]) %>% 
    mutate(ages = 3:15, model = model_names[model]) %>% 
    rename(mean_waa = value)
  
  # Now rbind to everything else
  WAA_re_df_all <- rbind(WAA_re_df_all, WAA_re_df)
  mean_WAA_all <- rbind(mean_waa, mean_WAA_all)
  
} # end model loop

# Plot lines
ggplot(WAA_re_df_all %>% filter(yrs <= 2022),
       aes(x = factor(yrs), y = vals, color = factor(model), group = factor(model))) +
  geom_line(alpha = 0.75, size = 1.6) +
  scale_x_discrete(breaks = seq(1990, 2020, 5)) +
  theme_bw() +
  facet_wrap(~ages, ncol = 4, scales = "free") +
  labs(x = "Year", y = "Weight", color = "Ages") +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 21),
        legend.position = "top")


# Compare AICs ------------------------------------------------------------

# Df to store aic values
AIC_df <- data.frame(AIC = NA, model = model_names)
for(i in 1:length(models)) AIC_df$AIC[i] <- margAIC(models[[i]]$optim)


# Compare parameters ----------------------------------------------------

models[[1]]$sd_rep
models[[2]]$sd_rep
models[[3]]$sd_rep
