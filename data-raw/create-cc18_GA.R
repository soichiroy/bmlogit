library(ccesMRPprep) # kuriwaki/ccesMRPprep, latest version
library(tidyverse)

# CCES data
cc18 <- get_cces_dataverse("cumulative", year_subset = 2018)

cc18_GA <- cc18 %>%
  mutate(st = as.character(as_factor(st))) %>%
  filter(st == "GA") %>%
  mutate(race_age = case_when(
    race == 1 & age %in% 18:29 ~ "White 18-29",
    race == 1 & age %in% 30:44 ~ "White 30-44",
    race == 1 & age %in% 45:64 ~ "White 45-64",
    race == 1 & age >= 65 ~ "White 65+",
    race != 1 & age %in% 18:29 ~ "Non-White 18-29",
    race != 1 & age %in% 30:44 ~ "Non-White 30-44",
    race != 1 & age %in% 45:64 ~ "Non-White 45-64",
    race != 1 & age >= 65 ~ "Non-White 65+"
  )) %>%
  mutate(female = as.integer(gender == 2)) %>%
  select(year, case_id, tookpost, weight, weight_post,
         st, cd, cd_post, county_fips, zipcode,
         pid3, pid3_leaner, ideo5, gender:faminc, female,
         race_age,
         voted_pres_party, intent_pres_party,
         voted_gov_party, intent_gov_party,
         voted_rep_party, intent_rep_party,
         matches("vv_")
         ) %>%
  mutate(across(where(haven::is.labelled), as_factor))

cc18_GA <- cc18_GA %>%
  mutate(voted_govR  = recode(as.character(voted_gov_party), Republican = 1, Democratic = 0, .default = 0.5, .missing = 0.5),
         intent_govR = recode(as.character(intent_gov_party), Republican = 1, Democratic = 0, .default = 0.5, .missing = 0.5)
  ) %>%
  mutate(voted_govR = replace(voted_govR, tookpost == "Did Not Take Post-Election Survey", NA_real_))


usethis::use_data(cc18_GA, overwrite = TRUE)




# https://docs.google.com/spreadsheets/d/1GvP9C3x5398WMfDk2imGNJXu5eJIU5JYEzmvscmcMW4/edit#gid=48431491
elec18_GA <- tribble(
  ~cd, ~abrams, ~kemp, ~total,
  "GA-01",	107922,	143090,	253294,
  "GA-02",	130095,	101543,	232770,
  "GA-03",	103639,	191210,	297805,
  "GA-04",	233750,	 59105,	294841,
  "GA-05",	268462,	 34559,	305460,
  "GA-06",	162945,	151873,	319687,
  "GA-07",	141719,	137781,	283240,
  "GA-08",	90608,	164934,	257268,
  "GA-09",	57745,	226096,	286867,
  "GA-10",	116225,	189043,	307958,
  "GA-11",	123637,	186795,	314815,
  "GA-12",	105607,	146536,	253985,
  "GA-13",	226020,	 69540,	297440,
  "GA-14",	55311,	176304,	233900
)

usethis::use_data(elec18_GA, overwrite = TRUE)

# electorate
# https://medium.com/@CatalistAnalytics/what-happened-in-the-georgia-gubernatorial-election-32e06254c0ab
cl_raw <- c(
  "Non-White 18-29" = 0.06,
  "Non-White 30-44" = 0.09,
  "Non-White 45-64" = 0.14,
  "Non-White 65+" = 0.06,
  "White 18-29" = 0.07,
  "White 30-44" = 0.12,
  "White 45-64" = 0.25,
  "White 65+" = 0.19)

catalist18_GA <- enframe(cl_raw / sum(cl_raw),
                         name = "race_age",
                         value = "prop_electorate") %>%
  mutate(count = as.integer(prop_electorate*sum(elec18_GA$total)))
usethis::use_data(catalist18_GA, overwrite = TRUE)
