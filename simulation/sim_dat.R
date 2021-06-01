####################################################################################
## Purpose: Simulate data for CHIM to show what a shiny app could do
##
## Author: Nick Brazeau
##
## Date: 05 April, 2021
##
## Notes:
####################################################################################
library(tidyverse)
library(randomNames)

#......................
# simulate
#......................
# num patients
npt <- 200
df <- tibble::tibble(
  name = randomNames::randomNames(npt),
  a1c = round(rnorm(npt, mean = 7.5, sd = 1), 2),
  last_a1c_date = sample(seq(as.Date('2019/01/01'),
                             as.Date('2021/03/31'),
                             by="day"),
                         npt) # https://stackoverflow.com/questions/21502332/generating-random-dates

)

#......................
# viz data
#......................
summary(df$a1c)
hist(df$a1c)

#......................
# write out
#......................
readr::write_csv(df, "~/Downloads/sim_data_a1c.csv")
