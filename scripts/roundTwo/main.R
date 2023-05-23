library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)
library(here)

here()

reformat_raw <- function(exp_name, data) {
    d <- deparse(substitute(data)) # nolint

    data %>%
    pivot_longer(
        cols = !`Time`, # nolint
        names_to = "grp"
    ) %>%
    mutate(
        exp = exp_name,
        grp_asf = as.numeric(str_extract(grp, "\\d+(?=\\s+aSf)")), # extract number before " aSf" # nolint
        grp_prldm = as.numeric(str_extract(grp, "\\d+(?=\\s+PrLDm)")) # extract number before " PrLDm" # nolint
    ) %>%

    drop_na(`Time`, `value`) %>% # nolint

    mutate(hours = as.duration(`Time`), .before = 1) %>% # nolint
    group_by(grp) %>% # nolint
    mutate(hours = (hours - min(hours))) %>%
    filter(hours <= hours(48))
}
rename_and_reformat <- function(data, exp_name) {
      if (exp_name == "x030_152") { # round2
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

      } else if (exp_name == "x030_157") { # hybrid1
            data %>%
                  rename(
                      "2 hf 00 asm" = "A1",
                      "2 hf 05 asm" = "A2",
                      "2 hf 10 asm" = "A3",
                      "2 hf 15 asm" = "A4",
                      "2 hf 20 asm" = "A5",
                      "0 hf 10 asm" = "A6",
                      "0 hf 15 asm" = "A7",
                      "2 hf 00 asm" = "B1",
                      "2 hf 05 asm" = "B2",
                      "2 hf 10 asm" = "B3",
                      "2 hf 15 asm" = "B4",
                      "2 hf 20 asm" = "B5",
                      "0 hf 10 asm" = "B6",
                      "0 hf 15 asm" = "B7",
                      "2 hf 00 prldm" = "C1",
                      "2 hf 05 prldm" = "C2",
                      "2 hf 10 prldm" = "C3",
                      "2 hf 15 prldm" = "C4",
                      "2 hf 20 prldm" = "C5",
                      "0 hf 10 prldm" = "C6",
                      "0 hf 15 prldm" = "C7",
                      "2 hf 00 prldm" = "D1",
                      "2 hf 05 prldm" = "D2",
                      "2 hf 10 prldm" = "D3",
                      "2 hf 15 prldm" = "D4",
                      "2 hf 20 prldm" = "D5",
                      "0 hf 10 prldm" = "D6",
                      "0 hf 15 prldm" = "D7"
                  ) %>%
                  reformat_raw(exp = exp_name)
      } else if (exp_name == "x030_156") { # hybrid1
            data %>%
                  rename(
                      "2 hf 00 asm A" = "A1",
                      "2 hf 05 asm A" = "A2",
                      "2 hf 10 asm A" = "A3",
                      "2 hf 15 asm A" = "A4",
                      "2 hf 20 asm A" = "A5",
                      "2 hf 00 asm B" = "A6",
                      "2 hf 05 asm B" = "A7",
                      "2 hf 10 asm B" = "A8",
                      "2 hf 15 asm B" = "A9",
                      "2 hf 20 asm B" = "A10",
                      "2 hf 00 prldm A" = "B1",
                      "2 hf 05 prldm A" = "B2",
                      "2 hf 10 prldm A" = "B3",
                      "2 hf 15 prldm A" = "B4",
                      "2 hf 20 prldm A" = "B5",
                      "2 hf 00 prldm B" = "B6",
                      "2 hf 05 prldm B" = "B7",
                      "2 hf 10 prldm B" = "B8",
                      "2 hf 15 prldm B" = "B9",
                      "2 hf 20 prldm B" = "B10",
                  ) %>%
                  reformat_raw(exp = exp_name)

      } else if (exp_name == "x030_157") { # hybrid2
            data %>%
                  rename(
                        "2 hf 00 asm A" = "A1",
                        "2 hf 05 asm A" = "A2",
                        "2 hf 10 asm A" = "A3",
                        "2 hf 15 asm A" = "A4",
                        "2 hf 20 asm A" = "A5",
                        "0 hf 10 asm A" = "A6",
                        "0 hf 15 asm A" = "A7",
                        "2 hf 00 asm B" = "B1",
                        "2 hf 05 asm B" = "B2",
                        "2 hf 10 asm B" = "B3",
                        "2 hf 15 asm B" = "B4",
                        "2 hf 20 asm B" = "B5",
                        "0 hf 10 asm B" = "B6",
                        "0 hf 15 asm B" = "B7",
                        "2 hf 00 prldm A" = "C1",
                        "2 hf 05 prldm A" = "C2",
                        "2 hf 10 prldm A" = "C3",
                        "2 hf 15 prldm A" = "C4",
                        "2 hf 20 prldm A" = "C5",
                        "0 hf 10 prldm A" = "C6",
                        "0 hf 15 prldm A" = "C7",
                        "2 hf 00 prldm B" = "D1",
                        "2 hf 05 prldm B" = "D2",
                        "2 hf 10 prldm B" = "D3",
                        "2 hf 15 prldm B" = "D4",
                        "2 hf 20 prldm B" = "D5",
                        "0 hf 10 prldm B" = "D6",
                        "0 hf 15 prldm B" = "D7"
                  ) %>%
                  reformat_raw(exp = exp_name)
      } else {
        print("No case found.")
    }
}
normalize_by_commonfactor <- function(data) {
    data %>%
    group_by(hours, grp) %>% # nolint
    unite(
        col = "id",
        c(grp, exp),
        sep = "_",
        remove = FALSE) %>%

    # normalize columns by a factor f for each column such that f*colmin=grpmin

        # find the min for each individual reaction
    ungroup() %>%
    group_by(id) %>%
    mutate(min_col = min(value, na.rm = TRUE)) %>% # nolint

        # find the min for each stoichiometry
    ungroup() %>%
    group_by(grp) %>%
    mutate(min_grp = min(value, na.rm = TRUE)) %>%

        # multiply each reaction by the factor which will make
        # the min of the reaction equal the min of the stoichiometry
    ungroup() %>%
    group_by(grp, id) %>%
    mutate(factor = min_col / min_grp) %>% # nolint
    mutate(fnorm_value = value / factor) %>%

        # remove the temporary columns
    select(-min_col, -min_grp, -factor, -value) %>%

    # zero-correct and normalize by id from 0 to 1

    group_by(id) %>%
    mutate(min_col = min(fnorm_value, na.rm = TRUE)) %>% # nolint
    mutate(fnorm_value = fnorm_value - min_col) %>%
    mutate(max_col = max(fnorm_value, na.rm = TRUE)) %>%
    mutate(fnorm_value = fnorm_value / max_col) %>% # nolint
    select(-min_col, -max_col)
}
process_experimental_data <- function(
      exp, normalize = TRUE, fibril = "asf", monomer = "prldm"
      ) {
            file <- paste0(exp, ".csv")
            exp_string <- paste0("x", exp)
            
            return_data <- read_csv(
                  here("data", "roundTwo", "rawData", file),
                        col_type = list(
                        .default = col_double(),
                        `Time` = "t"
                  )
            ) %>%

            rename_and_reformat(exp_string) %>%
            { if (normalize) normalize_by_commonfactor(.) else . } %>%
            filter(!str_detect(grp, "25 PrLDm"))
      
          return(return_data)

      }



x030_152 <- process_experimental_data("030_152")
x030_152

x030_156 <- process_experimental_data(
      "030_156.csv",
      "x030_156",
      normalize = FALSE,
      type = "hf"
      )
x030_156

x030_157 <- process_experimental_data(
      "030_157.csv",
      "x030_157",
      normalize = FALSE,
      type = "hf"
      )
x030_157
