library(ggplot2)
library(dplyr)
library(stringr)
library(here)

# Source data
source(here("scripts", "roundTwo", "main.R"))

printplot_temp <- function(df, filter = TRUE, group = grp) {
    p <- df %>%
    ungroup %>%
    { if(filter == TRUE) . 
          else filter(str_detect(grp, stringr::fixed(filter))) } %>%

    ggplot(aes(x = hours / 3600, y = value, color = grp)) +
    geom_point() +
    geom_line() +
    labs(
        x = "Time (hours)",
        y = "Fluorescence",
        title = paste0("Fluorescence of reactions containing ", filter),
        color = "Reaction Group"
        ) +
    coord_cartesian(xlim = c(0, 48)) +
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)

    return(p)
}
# Creates a png image with name provided by string `filter` in the
# `folder` path supplied, filtering `grp` for the string provided in `filter`.
save_plot <- function(p, filter, folder) {

    filename <- paste0(here(), "/plots", folder, filter, ".png")
    ggsave(filename, plot = p, width = 8, height = 4, dpi = 240)
}
printplot <- function(df, folder, filter) {

    p <- df %>%
        printplot_temp(filter)

    p %>% save_plot(filter, folder)
}

save_plot("/hybrid/.png")

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



