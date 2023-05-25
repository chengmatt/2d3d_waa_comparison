# Purpose: To compare 3d gmrf outputs to 2dar1
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date 5/25/23

# Set up ------------------------------------------------------------------

library(here)
library(tidyverse)
library(TMB)
library(cowplot)

# Compile and load in model
setwd(here("src"))
compile("2d3d_comparison.cpp")
dyn.load(dynlib("2d3d_comparison"))

# Load in WAA matrix (only use fishery data)
waa_df <- read.csv(here("data", "ebs_waa.csv")) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source)

# Load in std for WAA matrix
waa_std_df <- read.csv(here("data", "ebs_waa_std.csv")) %>% 
  filter(source == "fishery") %>% 
  dplyr::select(-source)


# Set up TMB data ----------------------------------------

# Number of projection years
n_proj_years <- 3
# Years
years <- waa_df$year
# Ages (goes from age 3 - 15+)
ages <- parse_number(colnames(waa_df)[-1])
# Read in data weight at age matrix
X_at <- t(as.matrix(waa_df[,-1])) # removing first col (year column)
# Create projection columns (append to X_at matrix)
proj_cols <- matrix(NA, nrow = length(ages), ncol = n_proj_years) 

# Append NA for projection year
X_at <- cbind(X_at, proj_cols) 
# Read in standard deviations for weight at age matrix
Xse_at <- t(as.matrix(waa_std_df[,c(-1)])) # removing first col (year column) 
# Convert to CV
Xcv_at <- sqrt( (exp(Xse_at^2) - 1) )
# Now convert back to sd in lognormal space
Xsd_at <- sqrt((log((Xcv_at)^2 + 1))/(log(10)^2))

# Create an index for ages and years to feed into TMB, which helps construct the precision matrix
ay_Index <- as.matrix(expand.grid("age" = seq_len(length(ages)), 
                                  "year" = seq_len(length(years) + n_proj_years) ))


# Set up TMB Model --------------------------------------------------------

# Now, input these components into a data list
data <- list( years = years,
              ages = ages,
              X_at = X_at,
              Xsd_at = Xsd_at,
              ay_Index = ay_Index,
              n_proj_years = n_proj_years,
              Var_Param = 0, # Var_Param == 0 Conditional, == 1 Marginal
              WAA_re_model = 0) # WAA_re_model == 0 3d gmrf, == 1 2dar1

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

# Run models ----------------------------------------------------

# Define number of extra newton steps we want to take
n.newton <- 3

# Empty list to store model objects
models <- list()

# map <- list("ln_Linf" = factor(NA), "ln_beta" = factor(NA), "rho_c" = factor(NA))
map <- list("ln_Linf" = factor(NA), "ln_beta" = factor(NA))

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
waa_model$sd_rep <- sdreport(waa_model)
  
 
  



