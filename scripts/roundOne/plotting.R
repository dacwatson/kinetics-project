library(ggplot2)
library(tidyverse)

# initialize variables

plots <- list()
groups <- unique(alldata_fnorm$grp)

alldata_plot <- alldata_fnorm %>%
    filter(!id %in% c(
        "0 aSf 05 PrLDm_030_139",
        "0 aSf 05 PrLDm_030_143",
        "0 aSf 05 PrLDm_030_149",

        "0 aSf 10 PrLDm_030_128",
        "0 aSf 10 PrLDm_030_139",
        "0 aSf 10 PrLDm_030_143",

        "0 aSf 15 PrLDm_030_128",
        "0 aSf 15 PrLDm_030_139",
        "0 aSf 15 PrLDm_030_143",
        "0 aSf 15 PrLDm_030_126",
        "0 aSf 15 PrLDm_030_127",

        "0 aSf 20 PrLDm_030_128",
        "0 aSf 20 PrLDm_030_139",
        "0 aSf 20 PrLDm_030_143",

        "0 aSf 25 PrLDm_030_139",
        "0 aSf 25 PrLDm_030_143",

        "0 aSf 30 PrLDm_030_128",
        "0 aSf 30 PrLDm_030_127",
        "0 aSf 30 PrLDm_030_139",

        "0.1 aSf 0 PrLDm_030_143",
        "0.1 aSf 0 PrLDm_030_145",
        "0.1 aSf 0 PrLDm_030_148",
        "0.1 aSf 0 PrLDm_030_149",

        "0.1 aSf 5 PrLDm_030_139",
        "0.1 aSf 5 PrLDm_030_148",

        "0.1 aSf 10 PrLDm_030_139",

        "0.1 aSf 15 PrLDm_030_139",
        "0.1 aSf 15 PrLDm_030_148",

        "0.1 aSf 20 PrLDm_030_139",
        "0.1 aSf 20 PrLDm_030_143",

        "0.1 aSf 25 PrLDm_030_143",
        "0.1 aSf 25 PrLDm_030_139",

        "0.1 aSf 20 PrLDm_030_139",
        "0.1 aSf 20 PrLDm_030_143",

        "0.5 aSf 0 PrLDm_030_143",
        "0.5 aSf 0 PrLDm_030_145",
        "0.5 aSf 0 PrLDm_030_148",
        "0.5 aSf 0 PrLDm_030_149",

        "0.5 aSf 5 PrLDm_030_139",
        "0.5 aSf 5 PrLDm_030_143",

        "0.5 aSf 10 PrLDm_030_139",
        "0.5 aSf 10 PrLDm_030_143",

        "0.5 aSf 15 PrLDm_030_139",
        "0.5 aSf 15 PrLDm_030_143",
        "0.5 aSf 15 PrLDm_030_149",

        "0.5 aSf 20 PrLDm_030_139",
        "0.5 aSf 20 PrLDm_030_143",

        "0.5 aSf 25 PrLDm_030_139",
        "0.5 aSf 25 PrLDm_030_143",

        "0.5 aSf 30 PrLDm_030_139",
        "0.5 aSf 30 PrLDm_030_143",

        "01 aSf 5 PrLDm_030_139",
        "01 aSf 5 PrLDm_030_149",

        "01 aSf 10 PrLDm_030_128",
        "01 aSf 10 PrLDm_030_149",
        "01 aSf 10 PrLDm_030_139",
        "01 aSf 10 PrLDm_030_126",
        "01 aSf 10 PrLDm_030_127",

        "01 aSf 15 PrLDm_030_128",
        "01 aSf 15 PrLDm_030_139",
        "01 aSf 15 PrLDm_030_149",

        "01 aSf 20 PrLDm_030_143",
        "01 aSf 20 PrLDm_030_128",
        "01 aSf 20 PrLDm_030_139",

        "01 aSf 25 PrLDm_030_139",
        "01 aSf 25 PrLDm_030_149",

        "01 aSf 30 PrLDm_030_128",
        "01 aSf 30 PrLDm_030_139",
        "01 aSf 30 PrLDm_030_143",
        "01 aSf 30 PrLDm_030_126",

        "2 aSf 5 PrLDm_030_143",

        "2 aSf 10 PrLDm_030_128",
        "2 aSf 10 PrLDm_030_143",

        "2 aSf 15 PrLDm_030_128",
        "2 aSf 15 PrLDm_030_143",

        "2 aSf 20 PrLDm_030_128",
        "2 aSf 20 PrLDm_030_143",

        "2 aSf 25 PrLDm_030_139",
        "2 aSf 25 PrLDm_030_143",

        "2 aSf 30 PrLDm_030_128",
        "2 aSf 30 PrLDm_030_139",
        "2 aSf 30 PrLDm_030_143",

        "4 aSf 5 PrLDm_030_139",

        "4 aSf 10 PrLDm_030_128",
        "4 aSf 10 PrLDm_030_143",

        "4 aSf 15 PrLDm_030_126",
        "4 aSf 15 PrLDm_030_128",
        "4 aSf 15 PrLDm_030_139",

        "4 aSf 20 PrLDm_030_128",
        "4 aSf 20 PrLDm_030_139",
        "4 aSf 20 PrLDm_030_143",

        "4 aSf 25 PrLDm_030_139",
        "4 aSf 25 PrLDm_030_143",

        "4 aSf 30 PrLDm_030_126",
        "4 aSf 30 PrLDm_030_128",
        "4 aSf 30 PrLDm_030_139",
        "4 aSf 30 PrLDm_030_143"
        )
    )

# View the plots separately
printplot <- function(i) { # nolint: object_usage_linter.

    p <- alldata_plot %>%
    filter(grp == groups[i]) %>%

    ggplot(aes(x = hours, y = fnorm.value, color = id)) +
    geom_point() +
    geom_line() +
    labs(x = "Time", y = "Fluorescence", title = groups[i]) +
    theme(aspect.ratio = 0.5)

    plots[[i]] <- p

    filename <- paste0("plots\\plot_", groups[i], ".png")
    ggsave(filename, plot = p, width = 6, height = 4, dpi = 300)
}
printplot(11)


for (i in 1:length(groups)) {
    printplot(i)
}
