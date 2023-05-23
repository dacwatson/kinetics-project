if (exp_name == "x030_152") {
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