library(dplyr)
library(ggplot2)
library(purrr)
library(broom)

# Fit the Boltzmann function to each group in the data

boltzmann_fit <- function(df) {
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
table_boltzmann_fits <- function(df, filter = NULL) {
    if (!is.null(filter)) {
        df <- df %>%
            filter(str_detect(grp, filter))
    }
    boltzmann_fits_table_ <- df %>%
        ungroup() %>%
        filter(!str_detect(grp, "00 aSf 00 PrLDm|01 aSf 00 PrLDm")) %>%
        group_by(id, grp) %>%
        mutate(hours = as.numeric(hours) / 3600) %>%
        nest() %>%

        mutate(
            fit = map(data, ~ boltzmann_fit(.)),

            .fitted = map2(fit, data, ~ augment(.x, newdata = .y))
        ) %>%

        unnest(c(data, .fitted)) %>%
        select(hours, grp, fnorm_value, .fitted) %>%
        group_by(grp)

    return(boltzmann_fits_table_)
}
plot_boltzmann_fits <- function(df, filter) {
    plot_fits_ <- df %>%
        table_boltzmann_fits(filter)

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


fits <- x030_152 %>%

    mutate(fit = map(data, ~ boltzmann_fit(.))) %>%
    mutate(
    )
fits %>% table_boltzmann_fits()

test <- x030_152 %>%
    ungroup() %>%
    filter(!str_detect(grp, "00 aSf 00 PrLDm|01 aSf 00 PrLDm")) %>%
    group_by(id, grp, grp_asf, grp_prldm) %>%
    mutate(hours = as.numeric(hours) / 3600) %>%
    nest() %>%

    mutate(
        fit = map(data, ~ boltzmann_fit(.)),
        coef = map(fit, tidy)
        # .fitted = map2(fit, data, ~ augment(.x, newdata = .y))
    ) %>%
    unnest(coef)

test_lagtime <- test %>%
    ungroup %>%
    select(grp, grp_asf, grp_prldm, term, estimate, std.error) %>%
    filter(str_detect(term, "xmid|dt")) %>%
    pivot_wider(
        names_from = term,
        values_from = c(estimate, std.error),
        names_sep = "_"
    ) %>%
    mutate(lagtime = estimate_xmid - 2 * estimate_dt)
test_lagtime




test_lagtime %>%
    filter(grp_asf == 4) %>%
        ggplot() + # nolint
        geom_point(aes(x = grp_prldm, y = lagtime, color = grp)) +
        geom_line(aes(x = grp_prldm, y = lagtime), color = "#5050e0") +
        coord_cartesian(xlim = c(0, 20)) +
        labs(
            x = "prldm conc",
            y = "lagtime",
            title = paste0("Lagtime of reactions containing 04 asf"),
            color = "Reaction Group"
        ) +
        theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)

test_lagtime %>%
    filter(grp_prldm == 20) %>%
        ggplot() + # nolint
        geom_point(aes(x = grp_asf, y = lagtime, color = grp)) +
        geom_line(aes(x = grp_asf, y = lagtime), color = "#5050e0") +
        coord_cartesian(xlim = c(0, 4)) +
        labs(
            x = "prldm conc",
            y = "lagtime",
            title = paste0("Lagtime of reactions containing 04 asf"),
            color = "Reaction Group"
        ) +
        theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)




fits %>%
    map(
        plot_boltzmann_fits("00 aSf") %>%
            save_plot("00 aSf", "/x030_152/fits/aSf/"),
        plot_boltzmann_fits("01 aSf") %>%
            save_plot("01 aSf", "/x030_152/fits/aSf/"),
        plot_boltzmann_fits("02 aSf") %>%
            save_plot("02 aSf", "/x030_152/fits/aSf/"),
        plot_boltzmann_fits("04 aSf") %>%
            save_plot("04 aSf", "/x030_152/fits/aSf/"),

        plot_boltzmann_fits("05 PrLDm") %>%
            save_plot("05 PrLDm", "/x030_152/fits/PrLDm/"),
        plot_boltzmann_fits("10 PrLDm") %>%
            save_plot("10 PrLDm", "/x030_152/fits/PrLDm/"),
        plot_boltzmann_fits("15 PrLDm") %>%
            save_plot("15 PrLDm", "/x030_152/fits/PrLDm/"),
        plot_boltzmann_fits("20 PrLDm") %>%
            save_plot("20 PrLDm", "/x030_152/fits/PrLDm/"),
    )
