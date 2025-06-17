#############################################################
# Script: Phenological Potential - CWM-like Index
# Author: Laíla Arnauth > M.Sc. in Ecology – UFRJ
# Last updated: 2025-06-17 [YYYY-MM-DD]
# Description:
#   This script calculates a CWM-like phenology index (Fp) 
#   for flowering and fruiting using monthly species abundance 
#   and binary phenological indicators.
#   It performs Wilcoxon tests to compare systems (AFS vs RES) 
#   per month and generates line plots with error bars and 
#   significance asterisks.
#
# Data sources:
#   - data_fp_fruiting_cwmlike.csv
#   - data_fp_flowering_cwmlike.csv
#
# Output:
#   - cwm_fruiting_potential.jpeg
#   - cwm_flowering_potential.jpeg
#
# Required packages:
#   dplyr, ggplot2
#
# Structure:
#   1. Load and prepare data
#   2. Calculate Fp index for each system and month
#   3. Perform Wilcoxon tests for monthly comparisons
#   4. Generate and save plots
#############################################################


#### FRUITING ####

library(dplyr)
library(ggplot2)

# 1. Read data
dados_fruto <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/data_fp_fruiting_cwmlike.csv",sep = ";")

# 2. Calculate relative abundance and a CWM-like phenology index
fenologia_fruto <- dados_fruto %>%
  group_by(id) %>%
  mutate(abund_rel = abundancia / sum(abundancia, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(peso_feno = abund_rel * fruiting_indicator) %>%
  group_by(id, syst, month) %>%
  summarise(fpi_m = sum(peso_feno, na.rm = TRUE), .groups = "drop")

# 3. Order the months
ordem_meses <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fenologia_fruto <- fenologia_fruto %>%
  mutate(month = factor(month, levels = ordem_meses))

# 4. Wilcoxon Test by month
teste_pvalores_fruto <- fenologia_fruto %>%
  group_by(month) %>%
  summarise(p_value = wilcox.test(fpi_m ~ syst, exact = FALSE)$p.value) %>%
  mutate(month = factor(month, levels = ordem_meses)) %>%
  mutate(sig = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    TRUE             ~ ""
  ))

# 5. Plot
g_fruto <- ggplot(fenologia_fruto, aes(x = month, y = fpi_m, color = syst, group = syst)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Fruiting Potential Over the Year",
       x = "Month", y = "Fp (Community Weighted Fruiting Potential)",
       color = "System") +
  scale_color_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal()

# 6. Add significance asterisks
g_fruto +
  geom_text(data = teste_pvalores_fruto, aes(x = month, y = 0.06, label = sig), 
            inherit.aes = FALSE, color = "black", size = 5)

# 7. Save
ggsave("C:/Users/Name/github_repro_AFS_RES/figures/cwm_fruiting_potential.jpeg",
       width = 18, height = 10, units = "cm")


#
#
#
#
######### FLOWERING ###########
#
#
#
#

library(dplyr)
library(ggplot2)

# 1. Read data
dados_flor <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/data_fp_flowering_cwmlike.csv",sep = ";")

# 2. Calculate CWM of flowering
fenologia_flor <- dados_flor %>%
  group_by(id) %>%
  mutate(abund_rel = abundancia / sum(abundancia, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(peso_flor = abund_rel * flowering_indicator) %>%
  group_by(id, syst, month) %>%
  summarise(fpi_m = sum(peso_flor, na.rm = TRUE), .groups = "drop")

# 3. Order months
ordem_meses <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fenologia_flor <- fenologia_flor %>%
  mutate(month = factor(month, levels = ordem_meses))

# 4. Wilcoxon Test by month
teste_pvalores_flor <- fenologia_flor %>%
  group_by(month) %>%
  summarise(p_value = wilcox.test(fpi_m ~ syst, exact = FALSE)$p.value) %>%
  mutate(month = factor(month, levels = ordem_meses)) %>%
  mutate(sig = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    TRUE             ~ ""
  ))

# 5. Plot
g_flor <- ggplot(fenologia_flor, aes(x = month, y = fpi_m, color = syst, group = syst)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Flowering Potential Over the Year",
       x = "Month", y = "Fp (Community Weighted Flowering Potential)",
       color = "System") +
  scale_color_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal()

# 6. Add significance asterisks
g_flor +
  geom_text(data = teste_pvalores_flor, aes(x = month, y = 0.06, label = sig),
            inherit.aes = FALSE, color = "black", size = 5)

# 7. Save
ggsave("C:/Users/Name/github_repro_AFS_RES/figures/cwm_flowering_potential.jpeg",
       width = 18, height = 10, units = "cm")
