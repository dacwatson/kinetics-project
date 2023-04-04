library(ggplot2)
library(dplyr)
library(stringr)
library(here)

x030_152 %>%
    ungroup %>%
    select(hours, grp, fnorm_value) %>%
    mutate(hours = as.numeric(hours) / 3600) %>%
    group_by(grp) %>%

    pivot_wider(
        names_from = grp,
        values_from = fnorm_value
    ) %>%
    pivot_longer(
        cols = !hours, # nolint
        names_to = "grp",
        values_to = "fnorm_value"
    ) %>%
    replace_na(list(fnorm_value = 1)) %>%
    pivot_wider(
        names_from = grp,
        values_from = fnorm_value
    ) %>%
    filter(hours <= hours(48)) %>%
    write_tsv(
        file = paste0(here(), "/data", "/roundTwo", "/rawData", "/030_152.txt")
    )

# y - 