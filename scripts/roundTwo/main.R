library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)
library(here)

here()

reformat_raw <- function(data, exp_name) {
    d <- deparse(substitute(data)) # nolint

    data %>%
    mutate(hours = as.duration(`Time`), .before = 1) %>% # nolint
    mutate(hours = (hours - hours[1]) / 3600) %>%
    filter(hours <= 48) %>%
    select(!`Time`) %>%
    pivot_longer(c(-1), names_to = "original_col") %>%
    filter(hours <= hours(48)) %>%
    mutate(exp = exp_name)
}
rename_and_reformat_data <- function(data, exp_name) {
    if (exp_name == "x030_152") {
        data %>%
            rename(
                "01 aSf 00 PrLDm" = "H12",
                "01 aSf 05 PrLDm" = "H11",
                "01 aSf 10 PrLDm" = "H10",
                "01 aSf 15 PrLDm" = "H9",
                "01 aSf 20 PrLDm" = "H8",
                "01 aSf 25 PrLDm" = "H7",
                "02 aSf 00 PrLDm" = "H6",
                "02 aSf 05 PrLDm" = "H5",
                "02 aSf 10 PrLDm" = "H4",
                "02 aSf 15 PrLDm" = "H3",
                "02 aSf 20 PrLDm" = "H2",
                "02 aSf 25 PrLDm" = "H1",
                "04 aSf 00 PrLDm" = "G12",
                "04 aSf 05 PrLDm" = "G11",
                "04 aSf 10 PrLDm" = "G10",
                "04 aSf 15 PrLDm" = "G9",
                "04 aSf 20 PrLDm" = "G8",
                "04 aSf 25 PrLDm" = "G7",
                "00 aSf 05 PrLDm" = "G6",
                "00 aSf 10 PrLDm" = "G5",
                "00 aSf 15 PrLDm" = "G4",
                "00 aSf 20 PrLDm" = "G3",
                "00 aSf 25 PrLDm" = "G2",
                "00 aSf 00 PrLDm" = "G1"
            ) %>%
            reformat_raw(exp = exp_name)
    } else {
        print("No case found.")
    }
}
normalize_by_commonfactor <- function(data) {
    group_regex <- regex("^[0-9]\\d*(\\.\\d+)? aSf \\d+ PrLDm")

    data %>%
    mutate(grp = str_extract(original_col, group_regex)) %>%
    group_by(hours, grp) %>%
    unite(
        col = "id",
        c(original_col, exp),
        sep = "_",
        remove = FALSE) %>%

    # normalize columns by a factor f for each column such that f*colmin=grpmin

        # find the min for each individual reaction
    ungroup() %>%
    group_by(id) %>%
    mutate(min_col = min(value, na.rm = TRUE)) %>%

        # find the min for each stoichiometry
    ungroup() %>%
    group_by(grp) %>%
    mutate(min_grp = min(value, na.rm = TRUE)) %>%

        # multiply each reaction by the factor which will make
        # the min of the reaction equal the min of the stoichiometry
    ungroup() %>%
    group_by(grp, id) %>%
    mutate(factor = min_col / min_grp) %>%
    mutate(fnorm.value = value / factor) %>%

        # remove the temporary columns
    select(-min_col, -min_grp, -original_col, -factor, -value) %>%

    # zero-correct and normalize by id from 0 to 1

    group_by(id) %>%
    mutate(min_col = min(fnorm.value, na.rm = TRUE)) %>%
    mutate(fnorm.value = fnorm.value - min_col) %>%
    mutate(max_col = max(fnorm.value, na.rm = TRUE)) %>%
    mutate(fnorm.value = fnorm.value / max_col) %>%
    select(-min_col, -max_col)
}
process_experimental_data <- function(file, exp_string) {
    return_data <- read_csv(
            here("data", "roundTwo", "rawData", file),
            col_type = list(
                .default = col_double(),
                `Time` = "t"
                )
        ) %>%
        rename_and_reformat_data(exp_string) %>%
        normalize_by_commonfactor()

    return(return_data)
}

x030_152 <- process_experimental_data("030_152.csv", "x030_152")

# x030_152 <- read_csv(
# here("data", "roundTwo", "rawData", "030_152.csv"),
# col_type = list(.default = col_double(), `Time` = "t")
# )
# x030_152 <- rename_and_reformat_data("x030_152") %>%
#     normalize_by_commonfactor()