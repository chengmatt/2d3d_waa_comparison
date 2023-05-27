# Purpose: To extract outputs from all_models and compare 
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date: 5.25.23

# Function to compute marginal AIC
margAIC <- function(optim_model) {
  # Get number of parameters 
  k <- length(optim_model[["par"]])
  # Extract objective function
  nLL <- optim_model[["objective"]]
  # Calculate AIC
  margAIC <- 2*(k+nLL)
  return(margAIC)
}

# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(TMB)
library(cowplot)

load(here("output", "2d3d_models.RData")) # obj is called all_models

# list species 
species_name <- c("atkamack", "ebspollock", "hake")
n_species <- length(species_name)

# Number of projection years
n_proj_years <- 3

# Get WAA Values ----------------------------------------------------------

# Empty dataframe to store values in
WAA_re_df_all <- data.frame()
mean_WAA_all <- data.frame()

# extract waa_re values
for(model in 1:length(all_models)) {
  
  # Get dimensions here
  years <- ncol(all_models[[model]]$rep$mu_at) 
  ages <- nrow(all_models[[model]]$rep$mu_at)
  
  # coerce these values into a matrix
  WAA_re <- matrix(
    t(exp(all_models[[model]]$env$parList()$ln_Y_at)),
    ncol = ages, nrow = years
  )
  
  # standard deviations
  WAA_sd <- matrix(
    t(sqrt(all_models[[model]]$sd_rep$diag.cov.random)),
    ncol = ages, nrow = years
  )
  
  # Next, melt into a dataframe
  WAA_re_df <- reshape2::melt(WAA_re)
  colnames(WAA_re_df) <- c("yrs", "ages", "vals")
  
  # Differentiate model
  WAA_re_df$model <- all_models[[model]]$Model 
  WAA_re_df$var_param <- all_models[[model]]$Var_Param 
  WAA_re_df$species <- all_models[[model]]$Species 

  # do the same for the sds
  WAA_re_sd_df <- reshape2::melt(WAA_sd)
  colnames(WAA_re_sd_df) <- c("yrs", "ages", "sd")
  WAA_re_df$sd <- WAA_re_sd_df$sd
  
  mean_waa <- reshape2::melt(all_models[[model]]$rep$mu_at[,1]) %>% 
    mutate(ages = 1:ages) %>% 
    rename(mean_waa = value)
  
  # Now rbind to everything else
  WAA_re_df_all <- rbind(WAA_re_df_all, WAA_re_df)
  mean_WAA_all <- rbind(mean_waa, mean_WAA_all)
  
} # end model loop

# Number of variance paramerzations
Var_Param <- c("Conditional", "Marginal")

pdf(here("figs", "waa_sd_comparison.pdf"), width = 14, height = 21)
for(species in 1:n_species) {
  for(var_param in 1:length(Var_Param)) {
    
    # Dataframe
    df <- WAA_re_df_all[WAA_re_df_all$species == species_name[species]
                        & WAA_re_df_all$var_param == Var_Param[var_param],]
    
    # waa plot
    waa_plot <- ggplot(df, aes(x = yrs, y = vals, color = factor(model), group = factor(model))) +
      geom_line(alpha = 0.75, size = 1.6) +
      theme_bw() +
      facet_wrap(~ages, scales = "free_y") +
      labs(x = "Year", y = "Weight", color = "Ages",
           title = paste(species_name[species],Var_Param[var_param], sep = "_")) +
      theme(axis.title = element_text(size = 21),
            axis.text = element_text(size = 18, color = "black"),
            legend.title = element_text(size = 21),
            legend.text = element_text(size = 18),
            strip.text = element_text(size = 21), 
            title = element_text(size = 21),
            legend.position = "top")
    
    # sd
    sd_plot <- ggplot(df, aes(x = yrs, y = sd, color = factor(model), group = factor(model))) +
      geom_line(alpha = 0.75, size = 1.6) +
      theme_bw() +
      facet_wrap(~ages, scales = "free_y") +
      labs(x = "Year", y = "SD", color = "Ages",
           title = "") +
      theme(axis.title = element_text(size = 21),
            axis.text = element_text(size = 18, color = "black"),
            legend.title = element_text(size = 21),
            legend.text = element_text(size = 18),
            strip.text = element_text(size = 21), 
            title = element_text(size = 21),
            legend.position = "none")
    
    print(plot_grid(waa_plot, sd_plot, ncol = 1))
    
  } # end var_param loop
} # end species loop

dev.off()


# Compare AICs and nLLs ------------------------------------------------------------

# Df to store aic values
AIC_vec <- vector()
species_name <- vector()
model_name <- vector()
var_param_name <- vector()
nLL_obs <- vector()
nLL_GMRF <- vector()

for(i in 1:length(all_models)) {
  AIC_vec[i] <- margAIC(all_models[[i]]$optim) # Get aic
  species_name[i] <- all_models[[i]]$Species # species name
  model_name[i] <- all_models[[i]]$Model # model name
  var_param_name[i] <- all_models[[i]]$Var_Param # variance paramterization
  nLL_obs[i] <- all_models[[i]]$rep$nLL_obs # nLL observations
  nLL_GMRF[i] <- all_models[[i]]$rep$nLL_gmrf   # nLL GMRF
}

AIC_df <- data.frame(AIC = AIC_vec, Species = species_name, 
                  Model = model_name, Var_Param = var_param_name,
                  nLL_obs = nLL_obs, nLL_GMRF = nLL_GMRF) %>% 
  pivot_longer(!c(Model, Var_Param, Species), values_to = "Values", names_to = "Metric")

# AIC plot
aic_plot <- ggplot(AIC_df %>% filter(Metric == "AIC"), 
       aes(x = factor(Model), y = Values, color = Var_Param, group = Var_Param)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top",
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 21)) +
  labs(x = "Model", y = "AIC")

# nLL GMRF
gmrf_nll_plot <- ggplot(AIC_df %>% filter(Metric == "nLL_GMRF"), 
       aes(x = factor(Model), y = Values, color = Var_Param, group = Var_Param)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 21)) +
  labs(x = "Model", y = "nLL_GMRF")

# nLL observations
obs_nll_plot <- ggplot(AIC_df %>% filter(Metric == "nLL_obs"), 
       aes(x = factor(Model), y = Values, color = Var_Param, group = Var_Param)) +
  geom_point(position = position_dodge(width = 0.5), size = 5) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 21),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 21)) +
  labs(x = "Model", y = "nLL_obs")

pdf(here("figs", "metric_comparison.pdf"), width = 21, height = 18)
plot_grid(aic_plot, gmrf_nll_plot, obs_nll_plot, ncol = 1)
dev.off()


