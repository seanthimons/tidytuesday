
# packages ---------------------------------------------------------------

{
  library(tidyverse)
  library(toxpiR)
}


# raw --------------------------------------------------------------------

{
tuesdata <- tidytuesdayR::tt_load('2025-05-20')

water_quality <- tuesdata$water_quality
weather <- tuesdata$weather
  
rm(tuesdata)  
}


# explore ----------------------------------------------------------------

