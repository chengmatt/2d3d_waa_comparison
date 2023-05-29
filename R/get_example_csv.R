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

# Loop through
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

# Do GOA pollock but from example .RData files from AKWHAM repo
load(here("examples", "input.RData"))
load(here("examples", "cv_survey.RData"))

# Get WAA values
X_at = input$data$waa[2,,]
X_at = reshape2::melt(X_at)
colnames(X_at) = c("year", "ages", "values")
X_at = X_at %>% pivot_wider(names_from = "ages", values_from = "values" ) %>% 
  mutate(source = "fishery") # not really fishery. data (survey data), but using to keep consistency in model runs
write.csv(X_at, here("data", paste("goapollock", "waa.csv", sep = "_")))

# Get WAA CV values
cvsurvey = as.matrix(out_data)
cvsurvey[is.na(cvsurvey)] = 0
waa_cv = array(0, dim = dim(X_at[,-1]))
waa_cv[] = cvsurvey
waa_cv = reshape2::melt(waa_cv)
colnames(waa_cv) = c("year", "ages", "values")
waa_cv = waa_cv %>% pivot_wider(names_from = "ages", values_from = "values" ) %>% 
  mutate(source = "fishery")
write.csv(waa_cv, here("data", paste("goapollock", "waa_std.csv", sep = "_")))
