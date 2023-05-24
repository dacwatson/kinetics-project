# Individual Data Plots

printplot_p1 <- function(df, conc, monomer, group = grp) {
      p <- df %>%
      filter(monomer_type == monomer) %>%
      filter(monomer_conc == conc) %>%
            
      ggplot(aes(x = hours / 3600, y = fnorm_value, color = id)) +
      geom_point() +
      geom_line() +
#      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
      
      labs(
            x = "Time (hours)",
            y = "Fluorescence",
            title = paste0("Fluorescence of reactions containing ", conc, " ", monomer),
            color = "Reaction Group"
            ) +
      coord_cartesian(xlim = c(0, 48)) +
      theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)
      
return(p)
}

p_1 <- bind_rows(x030_156, x030_157) %>%
      filter(!fibril_conc == 0) %>%
      group_by(monomer_type, monomer_conc, hours) %>%
      unite("id", c(grp, exp), sep = "|", remove = FALSE)
p_1

p_1n <- bind_rows(no_normal_x030_156, no_normal_x030_157) %>%
      filter(!fibril_conc == 0) %>%
      group_by(monomer_type, monomer_conc, hours) %>%
      unite("id", c(grp, exp), sep = "|", remove = FALSE) %>%
      mutate(fnorm_value = value)
p_1n


printplot_p1(p_1, 5, "prldm")
printplot_p1(p_1, 10, "prldm")
printplot_p1(p_1, 15, "prldm")
printplot_p1(p_1, 20, "prldm")
p_1 %>%
      filter(!id == "02 hf 05 asm A|030_157") %>%
      printplot_p1(5, "asm")
printplot_p1(p_1, 10, "asm")
printplot_p1(p_1, 15, "asm")
printplot_p1(p_1, 20, "asm")


printplot_p1(p_1n, 5, "prldm")
printplot_p1(p_1n, 10, "prldm")
printplot_p1(p_1n, 15, "prldm")
printplot_p1(p_1n, 20, "prldm")
p_1n %>%
      filter(!id == "02 hf 05 asm A|030_157") %>%
      printplot_p1(5, "asm")
printplot_p1(p_1n, 10, "asm")
printplot_p1(p_1n, 15, "asm")
printplot_p1(p_1n, 20, "asm")





# Error Bars of individual Reactions

printplot_p2 <- function(df, conc, monomer, group = grp) {
      p <- df %>%
            filter(monomer_type == monomer) %>%
            filter(monomer_conc == conc) %>%
            
            ggplot(aes(x = hours / 3600, y = mean)) +
            geom_point() +
            geom_line() +
            geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
            
            labs(
                  x = "Time (hours)",
                  y = "Fluorescence",
                  title = paste0("Fluorescence of reactions containing ", conc, " ", monomer),
                  color = "Reaction Group"
            ) +
            coord_cartesian(xlim = c(0, 48)) +
            theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)
      
      return(p)
}

p_2 <- p_1 %>%
      group_by(hours, fibril_conc, fibril_type, monomer_conc, monomer_type) %>%
            filter(!id == "02 hf 05 asm A|030_157") %>% #outlier data
      mutate(mean = mean(fnorm_value), sd = sd(fnorm_value)) %>%
      unite("reaction",
            c(fibril_conc, fibril_type, monomer_conc, monomer_type),
            sep = " ",
            remove = FALSE)
p_2

printplot_p2(p_2, 5, "prldm")
printplot_p2(p_2, 10, "prldm")
printplot_p2(p_2, 15, "prldm")
printplot_p2(p_2, 20, "prldm")

printplot_p2(p_2, 5, "asm")
printplot_p2(p_2, 10, "asm")
printplot_p2(p_2, 15, "asm")
printplot_p2(p_2, 20, "asm")

# Plot All Reactions

printplot_p3 <- function(df, conc) {
      p <- df %>%
            ggplot(aes(x = hours / 3600, y = mean, color = reaction)) +
            geom_point() +
            geom_line() +
            geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
            
            labs(
                  x = "Time (hours)",
                  y = "Fluorescence",
                  title = paste0("Fluorescence of reactions containing ", conc),
                  color = "Reaction Group"
            ) +
            coord_cartesian(xlim = c(0, 48)) +
            theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 0.5)
      
      return(p)
}

p_2 %>%
      filter(!monomer_conc == 0) %>%
      filter(monomer_type == "prldm") %>%
      filter(hours <= hours(24)) %>%
      filter(hours >= hours(5)) %>%
      printplot_p3("2 hf")
p_2 %>%
      filter(!monomer_conc == 0) %>%
      filter(monomer_type == "asm") %>%
      printplot_p3("2 hf")
