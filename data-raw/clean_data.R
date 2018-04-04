library(tidyverse)
library(haven)

# download NCES Common Core data for 2000-01
rawd <- read_sas("sc001aai.zip")
usschools <- select(rawd,
  state = LSTATE00, district = LEAID,
  school = NCESSCH, level = LEVEL00, total = MEMBER00,
  native = AM00, asian = ASIAN00, hisp = HISP00,
  black = BLACK00, white = WHITE00
) %>%
  # only primary schools
  filter(level == 1) %>%
  na.omit() %>%
  filter(state %in% c("AL", "CO", "CT")) %>%
  mutate(check = native + asian + hisp + black + white) %>%
  # only schools with >100 students
  filter(total == check, total > 100) %>%
  select(-total, -check, -level) %>%
  gather(key = race, value = n, -state, -district, -school) %>%
  mutate_at(vars(state, district, school, race), as_factor) %>%
  filter(n > 0) %>%
  # sort
  arrange(state, district, school, race)

save(usschools, file = "../data/usschools.rda")
