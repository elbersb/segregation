library(tidyverse)
library(haven)

set.seed(179463)

prepare <- function(d) {
    d %>%
    # only primary schools
    filter(level == 1) %>%
    na.omit() %>%
    filter(state %in% c("AL", "CO", "CT")) %>%
    mutate(state = recode(state, AL = "A", "CO" = "B", "CT" = "C")) %>%
    mutate(check = native + asian + hisp + black + white) %>%
    # only schools with >100 students
    filter(total == check, total > 100) %>%
    select(-total, -check, -level) %>%
    gather(key = race, value = n, -state, -district, -school) %>%
    filter(n > 0) %>%
    # sort
    arrange(state, district, school, race)
}

# NCES Common Core data for 2000-01
rawd <- read_sas("sc001aai.zip")
schools00 <- select(rawd,
    state = LSTATE00, district = LEAID,
    school = NCESSCH, level = LEVEL00, total = MEMBER00,
    native = AM00, asian = ASIAN00, hisp = HISP00,
    black = BLACK00, white = WHITE00
) %>% prepare
schools00$n <- round(rnorm(nrow(schools00), schools00$n, sd=sqrt(schools00$n)))
schools00 <- filter(schools00, n > 0)


# NCES Common Core data for 2005-06
rawd <- read_sas("sc051aai_sas.zip")
schools05 <- select(rawd,
    state = LSTATE05, district = LEAID,
    school = NCESSCH, level = LEVEL05, total = MEMBER05,
    native = AM05, asian = ASIAN05, hisp = HISP05,
    black = BLACK05, white = WHITE05
) %>% prepare
schools05$n <- round(rnorm(nrow(schools05), schools05$n, sd=sqrt(schools05$n)))
schools05 <- filter(schools05, n > 0)

# create new school and district identifiers
district <- bind_rows(
        select(schools00, state, district), 
        select(schools05, state, district)) %>%
    distinct() %>%
    group_by(state) %>%
    mutate(district_new=paste0(state, 1:n())) %>%
    select(district, district_new)
schools00 <- left_join(schools00, district)
schools05 <- left_join(schools05, district)

school <- bind_rows(select(schools00, district_new, school), 
    select(schools05, district_new, school)) %>%
    distinct() %>%
    group_by(district_new) %>%
    mutate(school_new=paste0(district_new, '_', 1:n())) %>%
    ungroup() %>%
    select(school, school_new)
schools00 <- left_join(schools00, school)
schools05 <- left_join(schools05, school)

schools00 <- schools00 %>%
    select(state, district = district_new, school = school_new, 
        race, n) %>%
    mutate_at(vars(state, district, school, race), as_factor)

schools05 <- schools05 %>%
    select(state, district = district_new, school = school_new, 
        race, n) %>%
    mutate_at(vars(state, district, school, race), as_factor)

print(schools00)
save(schools00, file = "../data/schools00.rda")
print(schools05)
save(schools05, file = "../data/schools05.rda")
