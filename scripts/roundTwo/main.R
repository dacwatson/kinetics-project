library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)
library(here)
library(ggplot2)

here()


read_data <- function(exp_id) {
      file <- paste0(exp_id, ".csv")
      
      return_data <- read_csv(
            here("data", "roundTwo", "rawData", file),
            col_type = list(
                  .default = col_double(),
                  `Time` = "t"
                  )
            ) %>%
      return()
}

relabel_data <- function(data, exp_id) {
      
      exp_name <- paste0("x", exp_id)
      
      if (exp_name == "x030_152") {
            data %>%
                  rename(
                      "01 asf 00 prldm" = "H12",
                      "01 asf 05 prldm" = "H11",
                      "01 asf 10 prldm" = "H10",
                      "01 asf 15 prldm" = "H9",
                      "01 asf 20 prldm" = "H8",
                      "01 asf 25 prldm" = "H7",
                      "02 asf 00 prldm" = "H6",
                      "02 asf 05 prldm" = "H5",
                      "02 asf 10 prldm" = "H4",
                      "02 asf 15 prldm" = "H3",
                      "02 asf 20 prldm" = "H2",
                      "02 asf 25 prldm" = "H1",
                      "04 asf 00 prldm" = "G12",
                      "04 asf 05 prldm" = "G11",
                      "04 asf 10 prldm" = "G10",
                      "04 asf 15 prldm" = "G9",
                      "04 asf 20 prldm" = "G8",
                      "04 asf 25 prldm" = "G7",
                      "00 asf 05 prldm" = "G6",
                      "00 asf 10 prldm" = "G5",
                      "00 asf 15 prldm" = "G4",
                      "00 asf 20 prldm" = "G3",
                      "00 asf 25 prldm" = "G2",
                      "00 asf 00 prldm" = "G1"
                  ) %>%
                  return()
      }  # heterotypic, disregard
      else if (exp_name == "x030_157") {
            data %>%
                  rename(
                      "02 hf 00 asm A" = "A1",
                      "02 hf 05 asm A" = "A2",
                      "02 hf 10 asm A" = "A3",
                      "02 hf 15 asm A" = "A4",
                      "02 hf 20 asm A" = "A5",
                      "00 hf 10 asm A" = "A6",
                      "00 hf 15 asm A" = "A7",
                      "02 hf 00 asm B" = "B1",
                      "02 hf 05 asm B" = "B2",
                      "02 hf 10 asm B" = "B3",
                      "02 hf 15 asm B" = "B4",
                      "02 hf 20 asm B" = "B5",
                      "00 hf 10 asm B" = "B6",
                      "00 hf 15 asm B" = "B7",
                      "02 hf 00 prldm A" = "C1",
                      "02 hf 05 prldm A" = "C2",
                      "02 hf 10 prldm A" = "C3",
                      "02 hf 15 prldm A" = "C4",
                      "02 hf 20 prldm A" = "C5",
                      "00 hf 10 prldm A" = "C6",
                      "00 hf 15 prldm A" = "C7",
                      "02 hf 00 prldm B" = "D1",
                      "02 hf 05 prldm B" = "D2",
                      "02 hf 10 prldm B" = "D3",
                      "02 hf 15 prldm B" = "D4",
                      "02 hf 20 prldm B" = "D5",
                      "00 hf 10 prldm B" = "D6",
                      "00 hf 15 prldm B" = "D7"
                  ) %>%
                  return()
      } # hybrid1, 2
      else if (exp_name == "x030_156") {
            data %>%
                  rename(
                        "02 hf 00 asm A" = "A1",
                        "02 hf 05 asm A" = "A2",
                        "02 hf 10 asm A" = "A3",
                        "02 hf 15 asm A" = "A4",
                        "02 hf 20 asm A" = "A5",
                        "02 hf 00 asm B" = "A6",
                        "02 hf 05 asm B" = "A7",
                        "02 hf 10 asm B" = "A8",
                        "02 hf 15 asm B" = "A9",
                        "02 hf 20 asm B" = "A10",
                        "02 hf 00 prldm A" = "B1",
                        "02 hf 05 prldm A" = "B2",
                        "02 hf 10 prldm A" = "B3",
                        "02 hf 15 prldm A" = "B4",
                        "02 hf 20 prldm A" = "B5",
                        "02 hf 00 prldm B" = "B6",
                        "02 hf 05 prldm B" = "B7",
                        "02 hf 10 prldm B" = "B8",
                        "02 hf 15 prldm B" = "B9",
                        "02 hf 20 prldm B" = "B10",
                  ) %>%
                  return()
      } # hybrid3, 4
      else if (exp_name == "x030_158") {
            data %>%
                  rename(
                        "01 asf 05 prldm" = "A1",
                        "01 asf 10 prldm" = "A2",
                        "01 asf 15 prldm" = "A3",
                        "01 asf 20 prldm" = "A4",
                        "02 asf 05 prldm" = "A5",
                        "02 asf 10 prldm" = "A6",
                        "02 asf 15 prldm" = "A7",
                        "02 asf 20 prldm" = "A8",
                        "04 asf 05 prldm" = "A9",
                        "04 asf 10 prldm" = "A10",
                        "04 asf 15 prldm" = "A11",
                        "04 asf 20 prldm" = "A12",
                        "00 asf 00 prldm" = "B1",
                        "00 asf 05 prldm" = "B2",
                        "00 asf 10 prldm" = "B3",
                        "00 asf 15 prldm" = "B4",
                        "00 asf 20 prldm" = "B5",
                  ) %>%
                  return()
      } # heterotypic, disregard
      else if (exp_name == "x047_120") {
            data %>%
                  rename(
                        ".1 prldf 05 prldm" = "A1",
                        ".1 prldf 10 prldm" = "A2",
                        ".1 prldf 15 prldm" = "A3",
                        ".1 prldf 20 prldm" = "A4",
                        ".1 prldf 25 prldm" = "A5",
                        
                        ".5 prldf 05 prldm" = "A6",
                        ".5 prldf 10 prldm" = "A7",
                        ".5 prldf 15 prldm" = "A8",
                        ".5 prldf 20 prldm" = "A9",
                        ".5 prldf 25 prldm" = "A10",
                        
                        "01 prldf 05 prldm" = "A11",
                        "01 prldf 10 prldm" = "A12",
                        "01 prldf 15 prldm" = "B1",
                        "01 prldf 20 prldm" = "B2",
                        "01 prldf 25 prldm" = "B3",
                        
                        "02 prldf 05 prldm" = "B4",
                        "02 prldf 10 prldm" = "B5",
                        "02 prldf 15 prldm" = "B6",
                        "02 prldf 20 prldm" = "B7",
                        "02 prldf 25 prldm" = "B8",
                        
                        "04 prldf 05 prldm" = "B9",
                        "04 prldf 10 prldm" = "B10",
                        "04 prldf 15 prldm" = "B11",
                        "04 prldf 20 prldm" = "B12",
                        "04 prldf 25 prldm" = "C1",
                        
                        "00 prldf 05 prldm" = "C2",
                        "00 prldf 10 prldm" = "C3",
                        "00 prldf 15 prldm" = "C4",
                        "00 prldf 20 prldm" = "C5",
                        "00 prldf 25 prldm" = "C6",
                        
                        ".1 prldf 00 prldm" = "C7",
                        ".5 prldf 00 prldm" = "C8",
                        "01 prldf 00 prldm" = "C9",
                        "02 prldf 00 prldm" = "C10",
                        "04 prldf 00 prldm" = "C11",
                  ) %>%
                  return()
      } # prld homotypic 1
      else if (exp_name == "x047_122") {
            data %>%
                  rename(
                        ".1 prldf 05 prldm" = "F1",
                        ".1 prldf 10 prldm" = "F2",
                        ".1 prldf 15 prldm" = "F3",
                        ".1 prldf 20 prldm" = "F4",
                        ".1 prldf 25 prldm" = "F5",

                        ".5 prldf 05 prldm" = "F6",
                        ".5 prldf 10 prldm" = "F7",
                        ".5 prldf 15 prldm" = "F8",
                        ".5 prldf 20 prldm" = "F9",
                        ".5 prldf 25 prldm" = "F10",

                        "01 prldf 05 prldm" = "F11",
                        "01 prldf 10 prldm" = "F12",
                        "01 prldf 15 prldm" = "G1",
                        "01 prldf 20 prldm" = "G2",
                        "01 prldf 25 prldm" = "G3",

                        "02 prldf 05 prldm" = "G4",
                        "02 prldf 10 prldm" = "G5",
                        "02 prldf 15 prldm" = "G6",
                        "02 prldf 20 prldm" = "G7",
                        "02 prldf 25 prldm" = "G8",

                        "04 prldf 05 prldm" = "G9",
                        "04 prldf 10 prldm" = "G10",
                        "04 prldf 15 prldm" = "G11",
                        "04 prldf 20 prldm" = "G12",
                        "04 prldf 25 prldm" = "H1",

                        "00 prldf 05 prldm" = "H2",
                        "00 prldf 10 prldm" = "H3",
                        "00 prldf 15 prldm" = "H4",
                        "00 prldf 20 prldm" = "H5",
                        "00 prldf 25 prldm" = "H6",

                        ".1 prldf 00 prldm" = "H7",
                        ".5 prldf 00 prldm" = "H8",
                        "01 prldf 00 prldm" = "H9",
                        "02 prldf 00 prldm" = "H10",
                        "04 prldf 00 prldm" = "H11",
                  ) %>%
                  return()
      } # prld homotypic 2
      else if (exp_name == "x047_124") {
            data %>%
                  rename(
                        ".1 prldf 05 prldm" = "A1",
                        ".1 prldf 10 prldm" = "A2",
                        ".1 prldf 15 prldm" = "A3",
                        ".1 prldf 20 prldm" = "A4",
                        ".1 prldf 25 prldm" = "A5",
                        
                        ".5 prldf 05 prldm" = "A6",
                        ".5 prldf 10 prldm" = "A7",
                        ".5 prldf 15 prldm" = "A8",
                        ".5 prldf 20 prldm" = "A9",
                        ".5 prldf 25 prldm" = "A10",
                        
                        "01 prldf 05 prldm" = "A11",
                        "01 prldf 10 prldm" = "A12",
                        "01 prldf 15 prldm" = "B1",
                        "01 prldf 20 prldm" = "B2",
                        "01 prldf 25 prldm" = "B3",
                        
                        "02 prldf 05 prldm" = "B4",
                        "02 prldf 10 prldm" = "B5",
                        "02 prldf 15 prldm" = "B6",
                        "02 prldf 20 prldm" = "B7",
                        "02 prldf 25 prldm" = "B8",
                        
                        "04 prldf 05 prldm" = "B9",
                        "04 prldf 10 prldm" = "B10",
                        "04 prldf 15 prldm" = "B11",
                        "04 prldf 20 prldm" = "B12",
                        "04 prldf 25 prldm" = "C1",
                        
                        "00 prldf 05 prldm" = "C2",
                        "00 prldf 10 prldm" = "C3",
                        "00 prldf 15 prldm" = "C4",
                        "00 prldf 20 prldm" = "C5",
                        "00 prldf 25 prldm" = "C6",
                        
                        ".1 prldf 00 prldm" = "C7",
                        ".5 prldf 00 prldm" = "C8",
                        "01 prldf 00 prldm" = "C9",
                        "02 prldf 00 prldm" = "C10",
                        "04 prldf 00 prldm" = "C11",
                  ) %>%
                  return()
      } # prld homotypic 3
      else {
        print("No case found.")
    }
}

pivot_and_group <- function(data, exp_id) {

      exp_name <- paste0("x", exp_id)
      
      data %>%
            
      pivot_longer(cols = !`Time`, names_to = "grp") %>%
      mutate(exp = exp_id) %>%
      
      separate_wider_delim(
            grp,
            delim = " ",
            names = c(
                  "fibril_conc",
                  "fibril_type",
                  "monomer_conc",
                  "monomer_type",
                  "replicate"),
            too_few = "align_start"
            ) %>%
      unite("exp", c(exp, replicate), sep = " ", na.rm = TRUE) %>%
      unite(
            "reaction",
            c(fibril_conc, fibril_type, monomer_conc, monomer_type),
            sep = " ",
            remove = FALSE
            ) %>%
      mutate(across(contains("conc"), as.numeric)) %>%
      mutate(across(contains("conc"), ~ tibble::num(., sigfig = 2))) %>%

      unite("id", c(reaction, exp), sep = " ", remove = FALSE) %>%

      drop_na(`Time`, `value`) %>%
      mutate(hours = as.duration(`Time`), .before = 1) %>%
      group_by(fibril_type, fibril_conc, monomer_type, monomer_conc) %>%
      mutate(hours = hours - min(hours)) %>%
      filter(hours <= hours(48))
}
process_data("030_158")

normalize_by_commonfactor <- function(data, normalize = TRUE) {

      if (normalize) {
            data %>%
                  group_by(id) %>%
                        mutate(max_col = max(value, na.rm = TRUE)) %>%
                        mutate(min_col = min(value, na.rm = TRUE)) %>%
                        mutate(value = value - min_col, na.rm = TRUE) %>%
      
                  group_by(exp) %>%
                        mutate(max_exp = max(max_col)) %>%
                        mutate(min_exp = min(max_col)) %>%
                        mutate(deviation = max_exp - min_exp) %>%
      
                  ungroup() %>%
                        mutate(max_dev = max(deviation)) %>%
                        mutate(factor = deviation / max_dev) %>%
      
                  group_by(id) %>%
                        mutate(value = value / factor) %>%

                  return()
      } else {
            return(data)
      }
}

mean_and_sd <- function(data, stats = TRUE) {

      if (stats) {
            data %>%
                  group_by(hours, reaction) %>%
                  mutate(mean = mean(value)) %>%
                  mutate(sd = sd(value)) %>%
                  
                  mutate(value = mean) %>%
            return()
      } else {
            return(data)
      }
}

process_data <- function(exp_id, normalize = FALSE, stats = FALSE) {

            read_data(exp_id) %>%

            relabel_data(exp_id) %>%

            pivot_and_group(exp_id) %>%

            normalize_by_commonfactor(normalize) %>%
            
            mean_and_sd(stats) %>%

            return(return_data)
      
}
