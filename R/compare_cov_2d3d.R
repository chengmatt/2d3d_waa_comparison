# Purpose: To compare the covariacne of a 2dar1 vs. 3d gmrf
# Creator: Matthew LH. Cheng
# Date 5/29/23

# Function to fill in an n x n correlation AR(1) matrix
get_CorrMat <- function(n, correlation) {
  corrMatrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      # Calculate the correlation based on the lag distance
      corrMatrix[i, j] <- correlation^(abs(i - j))
    }
  }
  return(corrMatrix)
}

# Set up ------------------------------------------------------------------
library(mvtnorm)
library(fields)
library(here)
source(here("R", "Construct_precision.R"))



# Set up dimensions
n_a = 3
n_t = 5

pcorr_age = 0.5
pcorr_year = 0.3
pcorr_cohort = 0.2
var_value= 0.01

# GMRF - 3d
three_Q = make_precision(n_a, n_t, pcorr_age, pcorr_year, pcorr_cohort, var_value,
                   Var_Type = "Marginal")
three_V = solve(three_Q)
three_Vdense = as.matrix(three_V)

# GMRF - 2d
Q = make_precision(n_a, n_t, pcorr_age, pcorr_year, 0, var_value,
                   Var_Type = "Marginal")
V = solve( Q )
Vdense = as.matrix(V)

# 2DAR1 
# Fill in correaltion matrix for age and year
cor_mat_age = get_CorrMat(n = n_a, correlation = pcorr_age)
cor_mat_yr = get_CorrMat(n = n_t, correlation = pcorr_year)

# Get krnoecker and then calculate covariance
cor_total = kronecker(cor_mat_age, cor_mat_yr)
cov_total = 0.01 * cor_total # get covariance

# Visualize
pdf(here("figs", "2d3d_comp_cov.pdf"), width = 5, height = 15)
par(mfrow = c(3,1))
# Get different color scale
col = colorRampPalette(c("blue", "white", "red"))(1000)
# covariance
image.plot(three_Vdense, main = "3D_GMRF Covariance", col = col) # Covariance 3dgmrf
image.plot(Vdense, main = "2D_GMRF Covariance", col = col) # Covariance 2d gmrf
image.plot(cov_total, main = "2D_AR1 Covariance", col = col) # Covariance 2dar1
dev.off()

# precision
three_gmrf_prec = image(Matrix(solve(three_Vdense), sparse = TRUE), main = "3D_GMRF Precision") # Precision 2d gmrf
gmrf_prec = image(Matrix(solve(Vdense), sparse = TRUE), main = "2D_GMRF Precision") # Precision 2d gmrf
twd_prec = image(Matrix(solve(cov_total), sparse = TRUE), main = "2D_AR1 Precision") # Precision 2dar1

pdf(here("figs", "2d3d_comp_prec.pdf"), width = 7, height = 15)
gridExtra::grid.arrange(three_gmrf_prec, gmrf_prec, twd_prec, ncol = 1)
dev.off()
