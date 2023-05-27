# Purpose: To output .dat files into .csv
# Creator: Matthew LH. Cheng
# Date 5/25/23

# Set up ------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(here)
source(here("R", "readadmb.R"))
source(here("R", "helper.R"))

# List files in examples folder for species
species <- list.files(here("examples"))[-c(2, 4:6, 8:11)]

# EBS pollock -------------------------------------------------------------

for(i in 1:length(species)) {
  
  # read data file
  df <-read_dat(here("examples",species[i],"wt.dat"))
  
  # Get fishery data
  df_fsh <- data.frame(year=df$fshry_yrs,df$fishery)
  df_fsh_std <- data.frame(year=df$fshry_yrs, df$fishery_std)
  
  # Get survey data
  df_srv <- data.frame(year=df$survey_yrs,df$survey)
  df_srv_std <- data.frame(year=df$survey_yrs,df$survey_std)
  
  # Bind both survey and fishery
  if(nrow(df_srv) != 0) { # if there are survey data
    df_bth <- rbind(data.frame(df_fsh,source="fishery"), data.frame(df_srv,source="survey"))
    df_bth_std <- rbind(data.frame(df_fsh_std,source="fishery"), data.frame(df_srv_std,source="survey"))
  } else{
    df_bth <- rbind(data.frame(df_fsh,source="fishery"))
    df_bth_std <- rbind(data.frame(df_fsh_std,source="fishery"))
  }
  
  # Write out csvs
  write.csv(df_bth, here("data", paste(species[i], "waa.csv", sep = "_")))
  write.csv(df_bth_std, here("data", paste(species[i], "waa_std.csv", sep = "_")))
  
} # end i
