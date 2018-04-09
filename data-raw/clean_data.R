library(tidyverse)
library(haven)

prepare <- function(d) {
    d %>%
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
}

# download NCES Common Core data for 2000-01
rawd <- read_sas("sc001aai.zip")
usschools00 <- select(rawd,
    state = LSTATE00, district = LEAID,
    school = NCESSCH, level = LEVEL00, total = MEMBER00,
    native = AM00, asian = ASIAN00, hisp = HISP00,
    black = BLACK00, white = WHITE00
) %>% prepare
save(usschools00, file = "../data/usschools00.rda")

# download NCES Common Core data for 2005-06
rawd <- read_sas("sc051aai_sas.zip")
usschools05 <- select(rawd,
    state = LSTATE05, district = LEAID,
    school = NCESSCH, level = LEVEL05, total = MEMBER05,
    native = AM05, asian = ASIAN05, hisp = HISP05,
    black = BLACK05, white = WHITE05
) %>% prepare
save(usschools05, file = "../data/usschools05.rda")
