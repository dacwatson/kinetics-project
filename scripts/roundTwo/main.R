library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)
library(here)

#     # laptop
# setwd("G:\\Other computers\\Desktop\\---Actual Documents\\R Projects\\ThT Kinetics project") # nolint
#     # desktop windows
# setwd("C:/Users/landon/Desktop/---Actual Documents/R Projects/ThT Kinetics project") # nolint
#     # desktop linux
# setwd("/home/landon/kinetics-project/") # nolint

x030_152 <- read_csv(
    here("data", "roundTwo", "rawData", "030_152.csv"),
    col_type = list(.default = col_double(),
    `Time` = "t")
    )
x030_152 <- rename_and_reformat_data("x030_152")


reformat_raw <- function(data, exp) {
    d <- deparse(substitute(data)) # nolint

    data %>%
    mutate(hours = as.duration(`Time`), .before = 1) %>% # nolint
    mutate(hours = (hours - hours[1]) / 3600) %>%
    select(!`Time`) %>%
    pivot_longer(c(-1), names_to = "original_col") %>%
    filter(hours <= hours(48)) %>%
    mutate(exp = exp)
}
rename_and_reformat_data <- function(exp) {
    if (exp == "x030_152") {
        x030_152 %>%
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
            reformat_raw(exp = exp)
    } else {
        print("No case found.")
    }
}
