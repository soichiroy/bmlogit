library(ccesMRPprep)
library(tidyverse)
library(bmlogit)
library(devtools)
library(usethis)

cc18_GA

# pop
acs_educ_GA <- get_acs_cces(acscodes_age_sex_educ, year = 2018, states = "GA")
raw_race_GA <- get_acs_cces(acscodes_age_sex_race, year = 2018, states = "GA")

# acs
acs_race_GA <- raw_race_GA %>%
  mutate(race = fct_collapse(
    race, `Other` = c("Native American", "All Other"))
    )


use_data(acs_educ_GA, overwrite = TRUE)
use_data(acs_race_GA, overwrite = TRUE)


# make groups consistent
# check with ccesMRPprep
fit <- synth_bmlogit(educ ~ race + age + female,
                     microdata = cc18_GA,
                     fix_to = edu_tgt,
                     fix_by_area = FALSE,
                     poptable = acs_race_GA,
                     count_var = "count",
                     area_var = "cd")
