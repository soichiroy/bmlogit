library(ccesMRPprep)
library(tidyverse)
library(bmlogit)
library(devtools)
library(usethis)

cc18_GA

# pop
acs_educ_GA <- get_acs_cces(acscodes_age_sex_educ, year = 2018, states = "GA")
acs_race_GA <- get_acs_cces(acscodes_age_sex_race, year = 2018, states = "GA")

use_data(acs_educ_GA, overwrite = TRUE)
use_data(acs_race_GA, overwrite = TRUE)


edu_tgt <- count(acs_educ_GA, cd, educ, wt = count, name = "count")
