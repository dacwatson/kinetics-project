library(dplyr)
library(ggplot2)
library(purrr)
library(broom)

# Fit the Boltzmann function to each group in the data

fit_boltzmann <- function(df) {
    nls(
        fnorm_value ~ high + ((low - high) / (1 + exp((hours - xmid) / dt))),
        data = df,
        start = list(
            low = 0.2,
            high = 0.8,
            xmid = 10,
            dt = 1.9
        )
    )
}

fits <- x030_152 %>%
    ungroup() %>%
    filter(!str_detect(grp, "00 aSf 00 PrLDm|01 aSf 00 PrLDm")) %>%
    group_by(id, grp) %>%
    mutate(hours = as.numeric(hours) / 3600) %>%
    nest() %>%
    mutate(fit = map(data, ~ fit_boltzmann(.))) %>%
    mutate(coef = map(fit, tidy)) %>%
    mutate(.fitted = map2(fit, data, ~ augment(.x, newdata = .y)))
fits

table_boltzmann_fits <- function(filter) {
    boltzmann_fits_table_ <- fits %>%
        filter(str_detect(grp, filter)) %>%
        ungroup() %>%
        unnest(c(data, .fitted), names_repair = "minimal") %>%
        select(hours, grp, fnorm_value, .fitted) %>%
        group_by(grp)

    return(boltzmann_fits_table_)
}

plot_boltzmann_fits <- function(filter) {
    plot_fits_ <- table_boltzmann_fits(filter)

    p <- plot_fits_ %>%
        ggplot() + # nolint
        geom_point(aes(x = hours, y = fnorm_value, color = grp)) +
        geom_line(aes(x = hours, y = .fitted, color = grp)) +
        coord_cartesian(xlim = c(0, 48)) +
        labs(
            x = "Time (hours)",
            y = "Fluorescence",
            title = paste0("Fluorescence of reactions containing ", filter),
            color = "Reaction Group"
        ) +
        theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)

    return(p)
}

plot_boltzmann_fits("00 aSf") %>% save_plot("00 aSf", "/x030_152/fits/aSf/")
plot_boltzmann_fits("01 aSf") %>% save_plot("01 aSf", "/x030_152/fits/aSf/")
plot_boltzmann_fits("02 aSf") %>% save_plot("02 aSf", "/x030_152/fits/aSf/")
plot_boltzmann_fits("04 aSf") %>% save_plot("04 aSf", "/x030_152/fits/aSf/")

plot_boltzmann_fits("05 PrLDm") %>% save_plot("05 PrLDm", "/x030_152/fits/PrLDm/") # nolint
plot_boltzmann_fits("10 PrLDm") %>% save_plot("10 PrLDm", "/x030_152/fits/PrLDm/") # nolint
plot_boltzmann_fits("15 PrLDm") %>% save_plot("15 PrLDm", "/x030_152/fits/PrLDm/") # nolint
plot_boltzmann_fits("20 PrLDm") %>% save_plot("20 PrLDm", "/x030_152/fits/PrLDm/") # nolint
