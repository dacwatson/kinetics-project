library(dplyr)
library(ggplot2)
library(tidymodels)



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



printplot_temp("01 aSf 00 PrLDm", x030_152)

########################### chat gpt code #########################


# Fit the Boltzmann function to each group in the data
fits <- x030_152 %>%
    ungroup() %>%
    filter(!str_detect(grp, "00 aSf 00 PrLDm|01 aSf 00 PrLDm")) %>%
    group_by(grp) %>%
    mutate(hours = as.numeric(hours) / 3600) %>%
    nest() %>%
    mutate(fit = map(data, ~ fit_boltzmann(.))) %>%
    mutate(coef = map(fit, tidy))

fits[[1,4]]

fits %>% filter(grp == "00 aSf 20 PrLDm") %>% tidy_fit$term
