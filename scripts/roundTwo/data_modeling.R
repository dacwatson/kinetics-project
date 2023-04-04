library(dplyr)
library(ggplot2)
library(purrr)
library(broom)


df_fit <- x030_152 %>%
    ungroup %>%
    filter(str_detect(grp, "02 aSf 00 PrLDm")) %>%
    select(hours, fnorm_value) %>%
    mutate(hours = as.numeric(hours) / 3600)

df_fit %>%
    ggplot(aes(x = hours, y = fnorm_value)) +
        geom_point() +
        geom_line() +
        theme(aspect.ratio = 0.4)

fit <- df_fit %>%
    fit_boltzmann()

df_pred <- tibble(
    hours = seq(
        min(df_fit$hours),
        max(df_fit$hours),
        length.out = 100
        )
    )
df_pred$fnorm_value <- predict(fit, newdata = df_pred)

ggplot() +
  geom_point(
    data = df_fit,
    aes(x = hours, y = fnorm_value),
    color = "#FF5733") +
  geom_line(
    data = df_pred,
    aes(x = hours, y = fnorm_value),
    color = "#1E90FF") +
  theme(aspect.ratio = 0.4)

############### trying to refactor the above ################

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
print_pred <- function(df, group) {
    df_fit_ <- df %>%
        ungroup %>%
        filter(str_detect(grp, group)) %>%
        select(hours, fnorm_value) %>%
        mutate(hours = as.numeric(hours) / 3600)

    fit_ <- df_fit_ %>%
        fit_boltzmann()

    df_pred_ <- tibble(
        hours = seq(
            min(df_fit_$hours),
            max(df_fit_$hours),
            length.out = 100
            )
        )
    df_pred_$fnorm_value <- predict(fit_, newdata = df_pred_)

    p <- ggplot() +
        geom_point(
            data = df_fit_,
            aes(x = hours, y = fnorm_value),
            color = "#FF5733") +
        geom_line(
            data = df_pred_,
            aes(x = hours, y = fnorm_value),
            color = "#1E90FF") +
        theme(aspect.ratio = 0.4)

    return(p)
}
print_pred(x030_152, "01 aSf 05 PrLDm")
print_pred(x030_152, "02 aSf 05 PrLDm")



printplot_temp("01 aSf 00 PrLDm", x030_152)

########################### chat gpt code #########################


# Fit the Boltzmann function to each group in the data
fits <- x030_152 %>%
    ungroup() %>%
    filter(!str_detect(grp, "00 aSf 00 PrLDm|01 aSf 00 PrLDm")) %>%
    group_by(id, grp) %>%
    mutate(hours = as.numeric(hours) / 3600) %>%
    nest() %>%
    mutate(fit = map(data, ~ fit_boltzmann(.))) %>%
    mutate(coef = map(fit, tidy)) %>%
    mutate(.fitted = map2(fit, data, ~ augment(.x, newdata = .y)))



plot_fits %>% View()

plot_fits %>%
    ggplot() + # nolint
    geom_point(aes(x = hours, y = fnorm_value, color = grp)) +
    geom_line(aes(x = hours, y = .fitted, color = grp)) +
    coord_cartesian(xlim = c(0, 48)) +
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)



plot_boltzmann_fits <- function(filter) {
    plot_fits_ <- fits %>%
        filter(str_detect(grp, filter)) %>%
        ungroup() %>%
        unnest(data, .fitted, names_repair = "minimal") %>%
        select(hours, grp, fnorm_value, .fitted) %>%
        group_by(grp)

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

plot_boltzmann_fits("00 aSf") %>% save_plot("05 PrLDm", "/x030_152/fits/PrLDm/")
plot_boltzmann_fits("01 aSf") %>% save_plot("10 PrLDm", "/x030_152/fits/PrLDm/")
plot_boltzmann_fits("02 aSf") %>% save_plot("15 PrLDm", "/x030_152/fits/PrLDm/")
plot_boltzmann_fits("04 aSf") %>% save_plot("20 PrLDm", "/x030_152/fits/PrLDm/")
