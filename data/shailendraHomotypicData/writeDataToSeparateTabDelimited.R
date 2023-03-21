library(tidyverse)
library(readxl)

# Read the xlsx file
data <- read_excel("shailendraHomotypicData\\shailendraHomotypicData.xlsx")

n <- ncol(data)
for (i in 2:n){
    col_name <- colnames(data)[i]
    data %>%
        group_by(Time) %>%
        select(Time, {{col_name}}) %>%
        write_delim(
            delim = "\t",
            col_names = TRUE,
            file = paste0("shailendraHomotypicData\\allTriplicateData\\", i, ".txt") # nolint: line_length_linter.
        )
}