# pcorr_year ->  Correlation for ages within a year
# pcorr_age ->  Correlation for years within an age
make_precision <-
function( n_a,
          n_t,
          pcorr_age,
          pcorr_year,
          pcorr_cohort,
          var_value= 1,
          Var_Type,
          what = "Q" ){
  
  library(here)
  library(Matrix)
  library(mvtnorm)
  library(tidyverse)

  index = expand.grid( "age"=seq_len(n_a), "year"=seq_len(n_t) )

  i = j = x = NULL
  for( n in 1:nrow(index) ){
    age = index[n,'age']
    year = index[n,'year']
    if( age>1 ){
      i = c( i, n )
      j = c( j, which(index[,'age']==(age-1) & index[,'year']==year) )
      x = c(x, pcorr_year)
    }
    if( year>1 ){
      i = c( i, n )
      j = c( j, which(index[,'age']==age & index[,'year']==(year-1)) )
      x = c(x, pcorr_age)
    }
    if( age>1 & year>1 ){
      i = c( i, n )
      j = c( j, which(index[,'age']==(age-1) & index[,'year']==(year-1)) )
      x = c(x, pcorr_cohort)
    }
  }

  #marg_pcorr_cohort = pcorr_cohort + 2*pcorr_age*pcorr_year
  #condvar = (1 - pcorr_age^2 - pcorr_year^2 - marg_pcorr_cohort^2) * var_value
  # condvar = (1 - pcorr_age^2 - pcorr_year^2 - pcorr_cohort^2) * var_value
  #d = NULL
  #for( n in 1:nrow(index) ){
  #  age = index[n,'age']
  #  year = index[n,'year']
  #  if( age==1 & year==1 ){
  #    d = c(d, var_value)
  #  }else if( age==1 & year>1 ){
  #    d = c(d, var_value* (1-pcorr_age^2))
  #  }else if( age>1 & year==1 ){
  #    d = c(d, var_value* (1-pcorr_year^2))
  #  }else{
  #    d = c(d, condvar)
  #  }
  #}

  # Assemble SAR precision
  B = sparseMatrix( i=i, j=j, x=x, dims=rep(n_a*n_t,2) )
  I = sparseMatrix( i=seq_len(n_a*n_t), j=seq_len(n_a*n_t), x=rep(1,n_a*n_t) )
  L = solve(I-B)
  
  # Solve Omega recursively for stationary variance
  if(Var_Type == "Marginal") {
    d = rep(NA, nrow(index))
    for( n in 1:nrow(index) ){
      if(n==1){
        d[n] = var_value
      }else{
        cumvar = sum(L[n,seq_len(n-1)] * d[seq_len(n-1)] * L[n,seq_len(n-1)])
        d[n] = (var_value-cumvar) / L[n,n]^2
      }
    }
    if(any(d<0)) stop("Check d")
    Omega = sparseMatrix( i=seq_len(n_a*n_t), j=seq_len(n_a*n_t), x=d, dims=rep(n_a*n_t,2) )
  }
  
  if(Var_Type == "Conditional") {
    d = var_value
    Omega = sparseMatrix( i=seq_len(n_a*n_t), j=seq_len(n_a*n_t), x=d^2, dims=rep(n_a*n_t,2) )
  }

  Omega_inv = sparseMatrix( i=seq_len(n_a*n_t), j=seq_len(n_a*n_t), x=1/d, dims=rep(n_a*n_t,2) )

  # Eq. 2 from Ver Hoef et al. 2018 "On the relationship between conditional (CAR) and simultaneous (SAR) autoregressive models"
  #Q = tcrossprod(I-t(B))
  #Q = (I-t(B)) %*% (I-B)
  Q = (I-t(B)) %*% Omega_inv %*% (I-B)
  Var = L %*% Omega %*% t(L)

  if(what=="Q") return(Q)
  if(what=="Var") return(Var)
  if(what=="Omega") return(Omega)
  if(what=="dmat") return(matrix(d,nrow=n_a,ncol=n_t))
}

# Explore
n_a = 5
n_t = 2
pcorr_age = 0.5
pcorr_year = 0.3
pcorr_cohort = 0.2
# # #marg_var = condvar / (1 - pcorr_age^2 - pcorr_year^2 - pcorr_cohort^2)
var_value= 0.01
# # var_value* (1 - pcorr_age^2 - pcorr_year^2)
# 
Q = make_precision(n_a, n_t, pcorr_age, pcorr_year, pcorr_cohort, var_value,
                   Var_Type = "Marginal")
V = solve( Q )
Vdense = as.matrix(V)
image(Vdense)

# 
# # Visualize
Y_at = matrix( rmvnorm(n=1, mean=rep(0,n_a*n_t), sigma=Vdense), nrow=n_a, ncol=n_t )
# image( y=seq_len(n_a), x=seq_len(n_t), z=t(Y_at), xlab="Year", ylab="Age" )
