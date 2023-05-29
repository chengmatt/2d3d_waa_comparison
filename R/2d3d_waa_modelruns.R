# Purpose: To compare 3d gmrf outputs to 2dar1
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date 5/25/23

# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(TMB)
library(cowplot)
library(TMBhelper)

# Compile and load in model
setwd(here("src"))
compile("2d3d_comparison.cpp")
# dyn.unload( dynlib("2d3d_comparison") )
dyn.load(dynlib("2d3d_comparison"))

# list species 
species_name <- c("atkamack", "ebspollock", "hake", "goapollock")
n_species <- length(species_name)

# Specify options here
# Number of projection years
n_proj_years <- 0
# Define number of extra newton steps we want to take
n.newton <- 3

# Set up vector for loop (0 == 3d gmrf, 1 == 2dar)
WAA_re_model = c(0, 0, 0, 1)
# Set up mapping list to turn on and off variables
map_list <- list(
  map1 = list("ln_Linf" = factor(NA), "ln_beta" = factor(NA)), # all 3 corr pars
  map2 = list("ln_Linf" = factor(NA), "ln_beta" = factor(NA), "rho_c" = factor(NA)), # no cohort par for 3d gmrf
  map3 = list("ln_Linf" = factor(NA), "ln_beta" = factor(NA), "rho_c" = factor(NA), "rho_y" = factor(NA), "rho_a" = factor(NA)), # iid
  map4 = list("ln_Linf" = factor(NA), "ln_beta" = factor(NA), "rho_c" = factor(NA)) # no cohort par for 2d ar1
)

# Variance Parameterization 
Var_Param <- c(0, 1)

# Empty list to store model objects
rho_models <- list()
var_models <- list()
re_models <- list()
all_models <- list()

# Species Loop - Read in Data ---------------------------------------------

for(species in 1:n_species) {

# Load in WAA matrix (only use fishery data)
waa_df <- read.csv(here("data", paste(species_name[species], "waa.csv", sep = "_"))) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source, -X)

# Load in std for WAA matrix
waa_std_df <- read.csv(here("data", paste(species_name[species], "waa_std.csv", sep = "_"))) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source, -X)


# Set up TMB Data ---------------------------------------------------------

# Years
years <- waa_df$year
# Ages (goes from age 3 - 15+)
ages <- parse_number(colnames(waa_df)[-1])
# Read in data weight at age matrix
X_at <- t(as.matrix(waa_df[,-1])) # removing first col (year column)
# Create projection columns (append to X_at matrix)
proj_cols <- matrix(NA, nrow = length(ages), ncol = n_proj_years) 

# Read in standard deviations for weight at age matrix
Xse_at <- t(as.matrix(waa_std_df[,c(-1)])) # removing first col (year column)
# # Convert to CV
Xcv_at <- sqrt( (exp(Xse_at^2) - 1) )
# # Now convert back to sd in lognormal space
Xsd_at <- sqrt((log((Xcv_at)^2 + 1))/(log(10)^2))
if(species_name[species] == "goapollock") Xsd_at <- sqrt(log(Xse_at^2 + 1))

# Append NA for projection year
X_at <- cbind(X_at, proj_cols)

# Create an index for ages and years to feed into TMB, which helps construct the precision matrix
ay_Index <- as.matrix(expand.grid("age" = seq_len(length(ages)), 
                                  "year" = seq_len(length(years) + n_proj_years) ))

# Parameters for TMB model --------------------------------------------------------

# Input parameters into a list
parameters <- list( rho_y = 0,
                    rho_a = 0,
                    rho_c = 0,
                    log_sigma2 = log(0.1),
                    ln_L0 = log(45),
                    ln_Linf = log(80),  # Fixed at arbitrary value
                    ln_k = log(0.15),
                    ln_alpha = log(3.5e-7), # Start alpha at a reasonable space 
                    # Starting value for alpha derived from a run where none of the rhos were estimated.
                    ln_beta = log(3), # Fix at isometric
                    ln_Y_at = array(0,dim=dim(X_at))) 

# Run models --------------------------------------------------------------

for(model in 1:length(WAA_re_model)) {
  
  # Mapping list to not estimate certain parameters
  map <- map_list[[model]]
  
  for(n_varParam in 1:length(Var_Param)) {

    # Input components into data list - iterate WAA_re_model
    data <- list( years = years,
                  ages = ages,
                  X_at = X_at,
                  Xsd_at = Xsd_at,
                  ay_Index = ay_Index,
                  n_proj_years = n_proj_years,
                  rho_gmrf_constrain = 0, # == 0 Partial correlation GMRF, == 1 Rho - 1 and 1
                  Var_Param = Var_Param[n_varParam], # Var_Param == 0 Conditional, == 1 Marginal
                  WAA_re_model = WAA_re_model[model]) # WAA_re_model == 0 3d gmrf, == 1 2dar1
    
    # Now, make AD model function
    waa_model <- MakeADFun(data = data, parameters = parameters,
                           map = map, random = "ln_Y_at",
                           DLL = "2d3d_comparison", silent = FALSE)
    
    # Now, optimize the function
    waa_optim <- stats::nlminb(waa_model$par, waa_model$fn, waa_model$gr,  
                               control = list(iter.max = 1e5, eval.max = 1e5))
    
    # Take some additional newton steps to make sure we reach a minimum
    tryCatch(expr = for(i in 1:n.newton) {
      g = as.numeric(waa_model$gr(waa_optim$par))
      h = optimHess(waa_optim$par, fn = waa_model$fn, gr = waa_model$gr)
      waa_optim$par = waa_optim$par - solve(h,g)
      waa_optim$objective = waa_model$fn(waa_optim$par)
    }, error = function(e){e})
    
    # Save optimized model results
    waa_model$optim <- waa_optim
    # Get report
    waa_model$rep <- waa_model$report(waa_model$env$last.par.best)
    # Get sd report
    waa_model$sd_rep <- sdreport(waa_model, getJointPrecision = TRUE)

    # save models in varaince parameterization loop
    # Differentiate models here
    if(Var_Param[n_varParam] == 0) waa_model$Var_Param = "Conditional"
    if(Var_Param[n_varParam] == 1) waa_model$Var_Param = "Marginal"
    if(names(map_list[model]) == "map1") waa_model$Model = "3D_GMRF"
    if(names(map_list[model]) == "map2") waa_model$Model = "2D_GMRF"
    if(names(map_list[model]) == "map3") waa_model$Model = "iid_GMRF"
    if(names(map_list[model]) == "map4") waa_model$Model = "2D_AR1"
    waa_model$Species = species_name[species]

    var_models[[n_varParam]] <- waa_model
    
    margAIC(waa_model$optim)
    
   } # end variance parameterization loop
    re_models <- c(var_models, re_models)
    
} # end model loop

  all_models <- c(all_models, re_models)
  print(model)
  
} # end species loop

# save models
save(all_models, file = here("output", "2d3d_models.RData"))
beepr::beep(4)

# Quick model checking
# Extract WAA random effects
# WAA_re <- t(waa_model$env$parList()$ln_Y_at)
# # Munge WAA for plotting
# WAA_re <- reshape2::melt(WAA_re) 
# WAA_re$value <- exp(WAA_re$value)
# colnames(WAA_re) <- c("yrs", "ages", "vals")
# WAA_re <- WAA_re %>% mutate(Type = "Random")
# 
# # Plot WAA re
# ggplot(WAA_re, 
#        aes(x = yrs, y = vals, group = 1)) +
#   geom_line(alpha = 0.85) +
#   facet_wrap(~ages, scales = "free") +
#   theme_bw() +
#   labs(x = "Year", y = "Weight") +
#   theme(axis.title = element_text(size = 17),
#         axis.text = element_text(size = 15),
#         legend.title = element_text(size = 17),
#         legend.text = element_text(size = 15),
#         legend.position = "none")

