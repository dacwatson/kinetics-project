library(ggplot2)
library(dplyr)
library(stringr)
library(here)

# Source data
source()

# View the plots separately
printplot <- function(filter, folder, data) { # nolint: object_usage_linter.

    p <- data %>%
    filter(str_detect(grp, fixed(filter))) %>%

    ggplot(aes(x = hours, y = fnorm.value, color = id)) +
    geom_point() +
    geom_line() +
    labs(x = "Time", y = "Fluorescence", title = filter) +
    theme(aspect.ratio = 0.5)

    filename <- paste0(here(), "/plots/roundTwo", folder, filter, ".png")
    ggsave(filename, plot = p, width = 6, height = 4, dpi = 300)
}
printplot("00 aSf", "/aSf/", x030_152_fnorm)
printplot("01 aSf", "/aSf/", x030_152_fnorm)
printplot("02 aSf", "/aSf/", x030_152_fnorm)
printplot("04 aSf", "/aSf/", x030_152_fnorm)

printplot("00 PrLDm", "/PrLDm/", x030_152_fnorm)
printplot("05 PrLDm", "/PrLDm/", x030_152_fnorm)
printplot("10 PrLDm", "/PrLDm/", x030_152_fnorm)
printplot("15 PrLDm", "/PrLDm/", x030_152_fnorm)
printplot("20 PrLDm", "/PrLDm/", x030_152_fnorm)
printplot("25 PrLDm", "/PrLDm/", x030_152_fnorm)
