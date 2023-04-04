library(dplyr)
library(ggplot2)
library(tidymodels)



df_fit <- x030_152 %>%
    ungroup %>%
    filter(str_detect(grp, "02 aSf 00 PrLDm")) %>%
    select(hours, fnorm_value) %>%
    mutate(hours = as.numeric(hours) / 3600)
df_fit
any(is.na(df_fit))

ggplot(df_fit, aes(x = hours, y = fnorm_value)) +
    geom_point() +
    geom_line() +
    theme(aspect.ratio = 0.4)

fit <- nls(
    fnorm_value ~ low + ((high - low) / (1 + exp((hours - xmid) / slope))),
    data = df_fit,
    start = list(low = 0.2, high = 0.8, xmid = 21, slope = 1.9)
    )

df_pred <- tibble(
    hours = seq(
        min(df_fit$hours),
        max(df_fit$hours),
        length.out = 100)
    )
df_pred$fnorm_value <- predict(fit, newdata = df_pred)

# Plot the data and predicted values
ggplot() +
  geom_point(data = df_fit, aes(x = hours, y = fnorm_value), color = "red") +
  geom_line(data = df_pred, aes(x = hours, y = fnorm_value), color = "blue") +
  theme(aspect.ratio = 0.4)
