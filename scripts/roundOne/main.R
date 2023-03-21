library("tidyverse")
library("readxl")
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(hablar)
library(ggplot2)
library(xlsx)

setwd("C:\\Users\\Landon\\Documents\\---Actual Documents\\R Projects\\ThT Kinetics project") # nolint: line_length_linter.)


# read in raw data and convert to tibble
# remove odd hours

x030_126 <- as_tibble(
  read_xlsx("data\\raw data\\030_126_raw.xlsx") %>%
    rename(
      "01 aSf 10 PrLDm" = "A1",
      "01 aSf 15 PrLDm" = "A2",
      "01 aSf 20 PrLDm" = "A3",
      "01 aSf 30 PrLDm" = "A4",
      "2 aSf 10 PrLDm" = "A5",
      "2 aSf 15 PrLDm" = "A6",
      "2 aSf 20 PrLDm" = "A7",
      "2 aSf 30 PrLDm" = "A8",
      "4 aSf 10 PrLDm" = "A9",
      "4 aSf 15 PrLDm" = "A10",
      "4 aSf 20 PrLDm" = "A11",
      "4 aSf 30 PrLDm" = "A12",
      "0 aSf 10 PrLDm" = "B1",
      "0 aSf 15 PrLDm" = "B2",
      "0 aSf 20 PrLDm" = "B3",
      "0 aSf 30 PrLDm" = "B4",
      "01 aSf 0 PrLDm" = "B5",
      "2 aSf 0 PrLDm" = "B6",
      "4 aSf 0 PrLDm" = "B7"
    )
  )
x030_126 <- x030_126 %>%
  convert(num(seq_len(ncol(x030_126)))) %>%
  mutate(across(hours, round, 3)) %>%
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_126")

x030_127 <- as_tibble(
  read_xlsx("data\\raw data\\030_127_raw.xlsx") %>%
    rename(
      "01 aSf 10 PrLDm" = "A1",
      "01 aSf 15 PrLDm" = "A2",
      "01 aSf 20 PrLDm" = "A3",
      "01 aSf 30 PrLDm" = "A4",
      "2 aSf 10 PrLDm" = "A5",
      "2 aSf 15 PrLDm" = "A6",
      "2 aSf 20 PrLDm" = "A7",
      "2 aSf 30 PrLDm" = "A8",
      "4 aSf 10 PrLDm" = "A9",
      "4 aSf 15 PrLDm" = "A10",
      "4 aSf 20 PrLDm" = "A11",
      "4 aSf 30 PrLDm" = "A12",
      "0 aSf 10 PrLDm" = "B1",
      "0 aSf 15 PrLDm" = "B2",
      "0 aSf 20 PrLDm" = "B3",
      "0 aSf 30 PrLDm" = "B4",
      "01 aSf 0 PrLDm" = "B5",
      "2 aSf 0 PrLDm" = "B6",
      "4 aSf 0 PrLDm" = "B7"
    )
  )
x030_127 <- x030_127 %>%
  convert(num(seq_len(ncol(x030_127)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_127")

x030_128 <- as_tibble(
  read_xlsx("data\\raw data\\030_128_raw.xlsx") %>%
    rename(
      "01 aSf 10 PrLDm" = "A1",
      "01 aSf 15 PrLDm" = "A2",
      "01 aSf 20 PrLDm" = "A3",
      "01 aSf 30 PrLDm" = "A4",
      "2 aSf 10 PrLDm" = "A5",
      "2 aSf 15 PrLDm" = "A6",
      "2 aSf 20 PrLDm" = "A7",
      "2 aSf 30 PrLDm" = "A8",
      "4 aSf 10 PrLDm" = "A9",
      "4 aSf 15 PrLDm" = "A10",
      "4 aSf 20 PrLDm" = "A11",
      "4 aSf 30 PrLDm" = "A12",
      "0 aSf 10 PrLDm" = "B1",
      "0 aSf 15 PrLDm" = "B2",
      "0 aSf 20 PrLDm" = "B3",
      "0 aSf 30 PrLDm" = "B4",
      "01 aSf 0 PrLDm" = "B5",
      "2 aSf 0 PrLDm" = "B6",
      "4 aSf 0 PrLDm" = "B7"
    )
  )
x030_128 <- x030_128 %>%
  convert(num(seq_len(ncol(x030_128)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_128")

x030_139 <- as_tibble(
  read_xlsx("data\\raw data\\030_139_raw.xlsx") %>%
    rename(
      "0.1 aSf 5 PrLDm" = "A1",
      "0.1 aSf 10 PrLDm" = "A2",
      "0.1 aSf 15 PrLDm" = "A3",
      "0.1 aSf 20 PrLDm" = "A4",
      "0.1 aSf 25 PrLDm" = "A5",
      "0.1 aSf 30 PrLDm" = "A6",
      "0.5 aSf 5 PrLDm" = "A7",
      "0.5 aSf 10 PrLDm" = "A8",
      "0.5 aSf 15 PrLDm" = "A9",
      "0.5 aSf 20 PrLDm" = "A10",
      "0.5 aSf 25 PrLDm" = "A11",
      "0.5 aSf 30 PrLDm" = "A12",
      "01 aSf 5 PrLDm" = "B1",
      "01 aSf 10 PrLDm" = "B2",
      "01 aSf 15 PrLDm" = "B3",
      "01 aSf 20 PrLDm" = "B4",
      "01 aSf 25 PrLDm" = "B5",
      "01 aSf 30 PrLDm" = "B6",
      "2 aSf 5 PrLDm" = "B7",
      "2 aSf 10 PrLDm" = "B8",
      "2 aSf 15 PrLDm" = "B9",
      "2 aSf 20 PrLDm" = "B10",
      "2 aSf 25 PrLDm" = "B11",
      "2 aSf 30 PrLDm" = "B12",
      "4 aSf 5 PrLDm" = "C1",
      "4 aSf 10 PrLDm" = "C2",
      "4 aSf 15 PrLDm" = "C3",
      "4 aSf 20 PrLDm" = "C4",
      "4 aSf 25 PrLDm" = "C5",
      "4 aSf 30 PrLDm" = "C6",
      "0 aSf 05 PrLDm" = "C7",
      "0 aSf 10 PrLDm" = "C8",
      "0 aSf 15 PrLDm" = "C9",
      "0 aSf 20 PrLDm" = "C10",
      "0 aSf 25 PrLDm" = "C11",
      "0 aSf 30 PrLDm" = "C12"
    )
  )
x030_139 <- x030_139 %>%
  convert(num(seq_len(ncol(x030_139)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
  filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_139")

x030_143 <- as_tibble(
  read_xlsx("data\\raw data\\030_143_raw.xlsx") %>%
    rename(
      "0.1 aSf 5 PrLDm" = "A1",
      "0.1 aSf 10 PrLDm" = "A2",
      "0.1 aSf 15 PrLDm" = "A3",
      "0.1 aSf 20 PrLDm" = "A4",
      "0.1 aSf 25 PrLDm" = "A5",
      "0.1 aSf 30 PrLDm" = "A6",
      "0.5 aSf 5 PrLDm" = "A7",
      "0.5 aSf 10 PrLDm" = "A8",
      "0.5 aSf 15 PrLDm" = "A9",
      "0.5 aSf 20 PrLDm" = "A10",
      "0.5 aSf 25 PrLDm" = "A11",
      "0.5 aSf 30 PrLDm" = "A12",
      "01 aSf 5 PrLDm" = "B1",
      "01 aSf 10 PrLDm" = "B2",
      "01 aSf 15 PrLDm" = "B3",
      "01 aSf 20 PrLDm" = "B4",
      "01 aSf 25 PrLDm" = "B5",
      "01 aSf 30 PrLDm" = "B6",
      "2 aSf 5 PrLDm" = "B7",
      "2 aSf 10 PrLDm" = "B8",
      "2 aSf 15 PrLDm" = "B9",
      "2 aSf 20 PrLDm" = "B10",
      "2 aSf 25 PrLDm" = "B11",
      "2 aSf 30 PrLDm" = "B12",
      "4 aSf 5 PrLDm" = "C1",
      "4 aSf 10 PrLDm" = "C2",
      "4 aSf 15 PrLDm" = "C3",
      "4 aSf 20 PrLDm" = "C4",
      "4 aSf 25 PrLDm" = "C5",
      "4 aSf 30 PrLDm" = "C6",
      "0 aSf 05 PrLDm" = "C7",
      "0 aSf 10 PrLDm" = "C8",
      "0 aSf 15 PrLDm" = "C9",
      "0 aSf 20 PrLDm" = "C10",
      "0 aSf 25 PrLDm" = "C11",
      "0 aSf 30 PrLDm" = "C12",
      "0.1 aSf 0 PrLDm" = "D1",
      "0.5 aSf 0 PrLDm" = "D2",
      "01 aSf 0 PrLDm" = "D3",
      "2 aSf 0 PrLDm" = "D4",
      "4 aSf 0 PrLDm" = "D5"
    )
  )
x030_143 <- x030_143 %>%
  convert(num(seq_len(ncol(x030_143)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_143")

x030_145 <- as_tibble(
  read_xlsx("data\\raw data\\030_145_raw.xlsx") %>%
    rename(
      "0.1 aSf 5 PrLDm" = "A1",
      "0.1 aSf 10 PrLDm" = "A2",
      "0.1 aSf 15 PrLDm" = "A3",
      "0.1 aSf 20 PrLDm" = "A4",
      "0.1 aSf 25 PrLDm" = "A5",
      "0.1 aSf 30 PrLDm" = "A6",
      "0.5 aSf 5 PrLDm" = "A7",
      "0.5 aSf 10 PrLDm" = "A8",
      "0.5 aSf 15 PrLDm" = "A9",
      "0.5 aSf 20 PrLDm" = "A10",
      "0.5 aSf 25 PrLDm" = "A11",
      "0.5 aSf 30 PrLDm" = "A12",
      "01 aSf 5 PrLDm" = "B1",
      "01 aSf 10 PrLDm" = "B2",
      "01 aSf 15 PrLDm" = "B3",
      "01 aSf 20 PrLDm" = "B4",
      "01 aSf 25 PrLDm" = "B5",
      "01 aSf 30 PrLDm" = "B6",
      "2 aSf 5 PrLDm" = "B7",
      "2 aSf 10 PrLDm" = "B8",
      "2 aSf 15 PrLDm" = "B9",
      "2 aSf 20 PrLDm" = "B10",
      "2 aSf 25 PrLDm" = "B11",
      "2 aSf 30 PrLDm" = "B12",
      "4 aSf 5 PrLDm" = "C1",
      "4 aSf 10 PrLDm" = "C2",
      "4 aSf 15 PrLDm" = "C3",
      "4 aSf 20 PrLDm" = "C4",
      "4 aSf 25 PrLDm" = "C5",
      "4 aSf 30 PrLDm" = "C6",
      "0 aSf 05 PrLDm" = "C7",
      "0 aSf 10 PrLDm" = "C8",
      "0 aSf 15 PrLDm" = "C9",
      "0 aSf 20 PrLDm" = "C10",
      "0 aSf 25 PrLDm" = "C11",
      "0 aSf 30 PrLDm" = "C12",
      "0.1 aSf 0 PrLDm" = "D1",
      "0.5 aSf 0 PrLDm" = "D2",
      "01 aSf 0 PrLDm" = "D3",
      "2 aSf 0 PrLDm" = "D4",
      "4 aSf 0 PrLDm" = "D5"
    )
  )
x030_145 <- x030_145 %>%
  convert(num(seq_len(ncol(x030_145)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_145")

x030_148 <- as_tibble(
  read_xlsx("data\\raw data\\030_148_raw.xlsx") %>%
    rename(
      "0.1 aSf 5 PrLDm" = "A1",
      "0.1 aSf 10 PrLDm" = "A2",
      "0.1 aSf 15 PrLDm" = "A3",
      "0.1 aSf 20 PrLDm" = "A4",
      "0.1 aSf 25 PrLDm" = "A5",
      "0.1 aSf 30 PrLDm" = "A6",
      "0.5 aSf 5 PrLDm" = "A7",
      "0.5 aSf 10 PrLDm" = "A8",
      "0.5 aSf 15 PrLDm" = "A9",
      "0.5 aSf 20 PrLDm" = "A10",
      "0.5 aSf 25 PrLDm" = "A11",
      "0.5 aSf 30 PrLDm" = "A12",
      "01 aSf 5 PrLDm" = "B1",
      "01 aSf 10 PrLDm" = "B2",
      "01 aSf 15 PrLDm" = "B3",
      "01 aSf 20 PrLDm" = "B4",
      "01 aSf 25 PrLDm" = "B5",
      "01 aSf 30 PrLDm" = "B6",
      "2 aSf 5 PrLDm" = "C7",
      "2 aSf 10 PrLDm" = "C8",
      "2 aSf 15 PrLDm" = "C9",
      "2 aSf 20 PrLDm" = "C10",
      "2 aSf 25 PrLDm" = "C11",
      "2 aSf 30 PrLDm" = "C12",
      "4 aSf 5 PrLDm" = "C1",
      "4 aSf 10 PrLDm" = "C2",
      "4 aSf 15 PrLDm" = "C3",
      "4 aSf 20 PrLDm" = "C4",
      "4 aSf 25 PrLDm" = "C5",
      "4 aSf 30 PrLDm" = "C6",
      "0 aSf 05 PrLDm" = "B7",
      "0 aSf 10 PrLDm" = "B8",
      "0 aSf 15 PrLDm" = "B9",
      "0 aSf 20 PrLDm" = "B10",
      "0 aSf 25 PrLDm" = "B11",
      "0 aSf 30 PrLDm" = "B12",
      "0.1 aSf 0 PrLDm" = "D1",
      "0.5 aSf 0 PrLDm" = "D2",
      "01 aSf 0 PrLDm" = "D3",
      "2 aSf 0 PrLDm" = "D4",
      "4 aSf 0 PrLDm" = "D5"
    )
  )
x030_148 <- x030_148 %>%
  convert(num(seq_len(ncol(x030_148)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_148")

x030_149 <- as_tibble(
  read_xlsx("data\\raw data\\030_149_raw.xlsx") %>%
    rename(
      "0.1 aSf 5 PrLDm" = "A1",
      "0.1 aSf 10 PrLDm" = "A2",
      "0.1 aSf 15 PrLDm" = "A3",
      "0.1 aSf 20 PrLDm" = "A4",
      "0.1 aSf 25 PrLDm" = "A5",
      "0.1 aSf 30 PrLDm" = "A6",
      "0.5 aSf 5 PrLDm" = "A7",
      "0.5 aSf 10 PrLDm" = "A8",
      "0.5 aSf 15 PrLDm" = "A9",
      "0.5 aSf 20 PrLDm" = "A10",
      "0.5 aSf 25 PrLDm" = "A11",
      "0.5 aSf 30 PrLDm" = "A12",
      "01 aSf 5 PrLDm" = "B1",
      "01 aSf 10 PrLDm" = "B2",
      "01 aSf 15 PrLDm" = "B3",
      "01 aSf 20 PrLDm" = "B4",
      "01 aSf 25 PrLDm" = "B5",
      "01 aSf 30 PrLDm" = "B6",
      "2 aSf 5 PrLDm" = "B7",
      "2 aSf 10 PrLDm" = "B8",
      "2 aSf 15 PrLDm" = "B9",
      "2 aSf 20 PrLDm" = "B10",
      "2 aSf 25 PrLDm" = "B11",
      "2 aSf 30 PrLDm" = "B12",
      "4 aSf 5 PrLDm" = "C1",
      "4 aSf 10 PrLDm" = "C2",
      "4 aSf 15 PrLDm" = "C3",
      "4 aSf 20 PrLDm" = "C4",
      "4 aSf 25 PrLDm" = "C5",
      "4 aSf 30 PrLDm" = "C6",
      "0 aSf 05 PrLDm" = "C7",
      "0 aSf 10 PrLDm" = "C8",
      "0 aSf 15 PrLDm" = "C9",
      "0 aSf 20 PrLDm" = "C10",
      "0 aSf 25 PrLDm" = "C11",
      "0 aSf 30 PrLDm" = "C12",
      "0.1 aSf 0 PrLDm" = "D1",
      "0.5 aSf 0 PrLDm" = "D2",
      "01 aSf 0 PrLDm" = "D3",
      "2 aSf 0 PrLDm" = "D4",
      "4 aSf 0 PrLDm" = "D5"
    )
  )
x030_149 <- x030_149 %>%
  convert(num(seq_len(ncol(x030_149)))) %>%
  mutate(across(hours, round, 3)) %>% 
  select(!`Time`) %>%
  pivot_longer(c(-1), names_to = "og_col") %>%
    filter(hours <= 48, hours %% (round(1 / 3, 3)) < 0.1) %>%
  mutate(exp = "030_149")

#########################################################

# combine all rows, extract grouping information

group_regex <- regex("^[0-9]\\d*(\\.\\d+)? aSf \\d+ PrLDm")

alldata_fnorm <- bind_rows(
    x030_127,
    x030_128,
    x030_126,
    x030_139,
    x030_143,
    x030_145,
    x030_148,
    x030_149) %>%
  mutate(grp = str_extract(og_col, group_regex)) %>%
  group_by(hours, grp) %>%
  unite(
    col = "id",
    c(og_col, exp),
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
  select(-min_col, -min_grp, -og_col, -factor, -value)

#########################################################

# zero-correct and normalize by id from 0 to 1

alldata_fnorm <- alldata_fnorm %>%
  group_by(id) %>%
  mutate(min_col = min(fnorm.value, na.rm = TRUE)) %>%
  mutate(fnorm.value = fnorm.value - min_col) %>%
  mutate(max_col = max(fnorm.value, na.rm = TRUE)) %>%
  mutate(fnorm.value = fnorm.value / max_col) %>%
  select(-min_col, -max_col)

#########################################################

# write to file

alldata_fnorm %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(row = row_number()) %>%
  select(hours, row, id, fnorm.value) %>%
  pivot_wider(names_from = "id", values_from = "fnorm.value") %>%
  as_tibble() %>%
  select(-row) %>%
  select(hours, order(colnames(.))) %>%
  write_csv("data\\alldata_normalized.csv") # nolint: line_length_linter.

alldata_fnorm %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(row = row_number()) %>%
  select(hours, row, id, fnorm.value) %>%
  pivot_wider(names_from = "id", values_from = "fnorm.value") %>%
  as_tibble() %>%
  select(-row) %>%
  select(hours, order(colnames(.))) %>%
  write.xlsx("data\\alldata_normalized.xlsx") # nolint: line_length_linter.

