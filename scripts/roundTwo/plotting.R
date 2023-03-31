library(ggplot2)
library(dplyr)
library(stringr)
library(here)

# Source data
source(here("scripts", "roundTwo", "main.R"))

# Creates a png image with name provided by string `filter` in the
# `folder` path supplied, filtering `grp` for the string provided in `filter`.
printplot <- function(filter, folder, data) { # nolint: object_usage_linter.

    p <- data %>%
    ungroup %>%
    filter(str_detect(grp, fixed(filter))) %>% # nolint
    filter(grp, contains("25 PrLDm")) %>%

    ggplot(aes(x = hours / 3600, y = fnorm_value, color = grp)) + # nolint
    geom_point() +
    geom_line() +
    labs(
        x = "Time (hours)",
        y = "Fluorescence",
        title = paste0("Fluorescence of reactions containing ", filter),
        color = "Reaction Group"
        ) +
    coord_cartesian(xlim = c(0, 48)) +
    theme(plot.title = element_text(hjust = 0.5))

    filename <- paste0(here(), "/plots/roundTwo", folder, filter, ".png")
    ggsave(filename, plot = p, width = 8, height = 4, dpi = 240)
}
printplot("00 aSf", "/x030_152/aSf/", x030_152)
printplot("01 aSf", "/x030_152/aSf/", x030_152)
printplot("02 aSf", "/x030_152/aSf/", x030_152)
printplot("04 aSf", "/x030_152/aSf/", x030_152)

printplot("00 PrLDm", "/x030_152/PrLDm/", x030_152)
printplot("05 PrLDm", "/x030_152/PrLDm/", x030_152)
printplot("10 PrLDm", "/x030_152/PrLDm/", x030_152)
printplot("15 PrLDm", "/x030_152/PrLDm/", x030_152)
printplot("20 PrLDm", "/x030_152/PrLDm/", x030_152)
printplot("25 PrLDm", "/x030_152/PrLDm/", x030_152)
