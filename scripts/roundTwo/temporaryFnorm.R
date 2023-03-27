# This file will temporarily factornormalize the data for x030_152 until I can write an fnorm function.
library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)

group_regex <- regex("^[0-9]\\d*(\\.\\d+)? aSf \\d+ PrLDm")

fnorm <- function(table) {
  table %>%
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
x030_152_fnorm <- fnorm(x030_152)

# alldata_fnorm <- x030_152 %>%
#   mutate(grp = str_extract(original_col, group_regex)) %>%
#   group_by(hours, grp) %>%
#   unite(
#     col = "id",
#     c(original_col, exp),
#     sep = "_",
#     remove = FALSE) %>%

#   # normalize columns by a factor f for each column such that f*colmin=grpmin

#     # find the min for each individual reaction
#   ungroup() %>%
#   group_by(id) %>%
#   mutate(min_col = min(value, na.rm = TRUE)) %>%

#     # find the min for each stoichiometry
#   ungroup() %>%
#   group_by(grp) %>%
#   mutate(min_grp = min(value, na.rm = TRUE)) %>%

#     # multiply each reaction by the factor which will make
#     # the min of the reaction equal the min of the stoichiometry
#   ungroup() %>%
#   group_by(grp, id) %>%
#   mutate(factor = min_col / min_grp) %>%
#   mutate(fnorm.value = value / factor) %>%

#     # remove the temporary columns
#   select(-min_col, -min_grp, -original_col, -factor, -value)

# #########################################################

# # zero-correct and normalize by id from 0 to 1

# alldata_fnorm <- alldata_fnorm %>%
#   group_by(id) %>%
#   mutate(min_col = min(fnorm.value, na.rm = TRUE)) %>%
#   mutate(fnorm.value = fnorm.value - min_col) %>%
#   mutate(max_col = max(fnorm.value, na.rm = TRUE)) %>%
#   mutate(fnorm.value = fnorm.value / max_col) %>%
#   select(-min_col, -max_col)
