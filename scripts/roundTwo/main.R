library(dplyr)
library(tidyr)
library(readr)
library(hablar)
library(stringr)
library(lubridate)
library(here)

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
      
      if (exp_name == "x030_152") { # round2
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
      } else if (exp_name == "x030_157") { # hybrid1
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
      } else if (exp_name == "x030_156") { # hybrid1
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
      } else {
        print("No case found.")
    }
}

pivot_and_group <- function(data, exp_id) {
      d <- deparse(substitute(data))
      exp_name <- paste0("x", exp_id)
      
      data %>%
            
      pivot_longer(cols = !`Time`, names_to = "grp") %>%
      mutate(exp = exp_id) %>%
      
      separate(
            grp,
            into = c(
                  "fibril_conc",
                  "fibril_type",
                  "monomer_conc",
                  "monomer_type",
                  "replicate"),
            sep = " ",
            convert = TRUE,
            fill = "right",
            extra = "drop") %>%
      unite("exp", c(exp, replicate), sep = " ", remove = FALSE) %>%
      unite("id", c(grp, exp), sep = "|", remove = FALSE) %>%
      unite(
            "reaction",
            c(fibril_conc, fibril_type, monomer_conc, monomer_type),
            sep = " ",
            remove = FALSE) %>%
      
      drop_na(`Time`, `value`) %>%
      mutate(hours = as.duration(`Time`), .before = 1) %>%
      group_by(fibril_type, fibril_conc, monomer_type, monomer_conc) %>%
      mutate(hours = hours - min(hours)) %>%
      filter(hours <= hours(48))
}

normalize_by_commonfactor <- function(data) {

      
      data %>%

      # find the min for each individual reaction
      
      group_by(exp, grp) %>%
      mutate(min_col = min(value, na.rm = TRUE)) %>%
      
      # find the min for each stoichiometry
      
      group_by(fibril_conc, fibril_type, monomer_conc, monomer_type) %>%
      mutate(min_grp = min(value, na.rm = TRUE)) %>%
      
      # multiply each reaction by the factor so that min(rxn) == min(grp)
      
      group_by(exp, grp) %>%
      mutate(factor = min_col / min_grp) %>%
      mutate(fnorm_value = value / factor) %>%
      
      # zero-correct
      
      mutate(fnorm_value = fnorm_value - min_col) %>%

      # normalize from 0 to 1            

#      mutate(max_col = max(fnorm_value, na.rm = TRUE)) %>%
#      mutate(fnorm_value = fnorm_value / max_col) %>%

      # remove the temporary columns
            
      ungroup() %>%
      select(hours, Time, grp, fibril_conc, fibril_type, monomer_conc,
             monomer_type, replicate, exp, fnorm_value)
}

process_data <- function(exp_id, normalize = TRUE, filter = TRUE) {

            read_data(exp_id) %>%

            relabel_data(exp_id) %>%

            pivot_and_group(exp_id) %>%

            { if (normalize) normalize_by_commonfactor(.) else . } %>%
            { if (filter) . else filter(!str_detect(grp, filter)) } %>%

            return(return_data)

      }

x030_156 <- read_data("030_156") %>%
      relabel_data("030_156") %>%
      pivot_and_group("030_156")
x030_157 <- read_data("030_157") %>%
      relabel_data("030_157") %>%
      pivot_and_group("030_157")

bind_rows(x030_156, x030_157) %>%
      
      unite("exp", c(exp, replicate)) %>%
      
      filter(!id == "02 hf 20 prldm B|030_156") %>%
      
      group_by(id) %>%
            mutate(max_col = max(value, na.rm = TRUE)) %>%
      
      
      group_by(exp) %>%
            mutate(max_exp = max(max_col)) %>%
            mutate(min_exp = min(max_col)) %>%
            mutate(deviation = max_exp - min_exp) %>%
      
      ungroup() %>%
            mutate(max_dev = max(deviation)) %>%
            mutate(factor = deviation / max_dev) %>%# group_by(exp, factor) %>% summarise()
      
      group_by(id) %>%
            mutate(value = value / factor) %>%# View()
            
      group_by(hours, reaction) %>%
            mutate(mean = mean(value)) %>%
            mutate(sd = sd(value)) %>%
      
            mutate(value = mean) %>%
      
      filter(monomer_type == "asm") %>%
      filter(monomer_conc >= 15) %>%
      filter(hours <=hours(24)) %>%
      filter(!fibril_conc == 0) %>%
      # filter(exp == "030_157_B") %>%
      
      ggplot(aes(x = hours / 3600, y = value, group = reaction,
                 color = reaction)) +
      geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.2) +
      geom_point() +
      
      labs(title = "average") +
      # labs(title = "normalized | 15, 20 asm") +
      # labs(title = "raw data | 15, 20 asm") +
      geom_line()
