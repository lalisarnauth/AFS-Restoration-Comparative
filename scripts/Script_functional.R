############################################################
## FUNCTIONAL DIVERSITY ANALYSES – REPRODUCTIVE TRAITS
##
## Description:
## This script performs functional diversity analyses (FDis and CWM)
## for reproductive traits (flowers, fruits, seeds) comparing 
## Agroforestry Systems (AFS) and Ecological Restoration Sites (RES).
##
## The script is organized into blocks for:
## - FDis: using Gower distance and fdisp()
## - CWM: using functcomp()
## - Both for full species sets and subsets of native species only
##
## Metrics:
## - Functional Dispersion (FDis)
## - Community Weighted Mean (CWM)
##
## Author: Laíla Arnauth > M.Sc. in Ecology – UFRJ
## Last updated: 2025-06-17 [YYYY-MM-DD]
############################################################

## FLOWERS - POLLINATION

library(readxl)
library(writexl)

setwd("C:/Users/Name/Results")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/community.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/functional_fl.csv",row.names = 1, sep = ";")

library(FD)
library(vegan)

# dbFD : x - df of functional traits; a - matrix of abundance; w.abun - should FDis, Rao’s Q, FEve, FDiv, and CWM be weighted by the relative abundances of the species?
  # corr - quando não-euclidiana

comunidade <- as.matrix(comunidade)

library(FD)

# 1. Calculate FDis
fdis_resultados <- fdisp(gowdis(traits), comunidade)

# 2. Extract results by system
result <- data.frame(
  Sistema = rownames(comunidade),
  FDis = fdis_resultados$FDis
)

write_xlsx(result, "result_fdis.xlsx")

result <- read_excel("C:/Users/Name/result_fdis.xlsx")


### HIPOTHESIS TEST ###

# 1. Verify normality
shapiro_saf <- shapiro.test(result$FDis[result$Sistema == "AFS"]) # NORMAL
shapiro_rest <- shapiro.test(result$FDis[result$Sistema == "RES"]) # NORMAL

# 2. Select apropriate test (Student's t-test)
teste <- t.test(FDis ~ Sistema, data = result, var.equal = TRUE)  # Agora usando t-test novamente

# 3. Boxplot

grafico_fdis <- ggplot(result, aes(x = Sistema, y = FDis, fill = Sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Comparison of Functional Dispersion (FDis) of Floral Traits",
    subtitle = paste(
      "Student's t-test - p =", 
      ifelse(teste$p.value < 0.001, "< 0.001", 
             format(round(teste$p.value, 3), nsmall = 3))
    ),
    y = "FDis (Functional Dispersion)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# 4. Save Graph
setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("Flor_fdis.jpg", plot = grafico_fdis, width = 8, height = 6, dpi = 300)


#
#
#
## FRUITS - DISPERSION
#
#
#

library(readxl)
library(writexl)

setwd("C:/Users/Name/figures")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/community.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/functional_fr.seed.csv",row.names = 1, sep = ";")

library(FD)
library(vegan)

# dbFD : x - df of functional traits; a - matrix of abundance; w.abun - should FDis, Rao’s Q, FEve, FDiv, and CWM be weighted by the relative abundances of the species?
# corr - quando não-euclidiana

comunidade <- as.matrix(comunidade)

library(FD)

# 1. Calculate FDis

fdis_result <- fdisp(gowdis(traits), comunidade)

# 2. Extract results by system
result <- data.frame(
  Sistema = rownames(comunidade),
  FDis = fdis_resultados$FDis
)

write_xlsx(result, "result_fdis_fr.seed.xlsx")

result <- read_excel("C:/Users/Name/result_fdis_fr.seed.xlsx")

library(dplyr)


### HIPOTHESIS TEST ###

# 1. Verify normality
shapiro_saf <- shapiro.test(result$FDis[result$Sistema == "AFS"]) # NORMAL
shapiro_rest <- shapiro.test(result$FDis[result$Sistema == "RES"]) # NORMAL

# 2. Select the appropriate test

teste_t <- t.test(FDis ~ Sistema, data = result, var.equal = TRUE) # p-value = 0.0452

# 3. Boxplot

grafico_fdis <- ggplot(result, aes(x = Sistema, y = FDis, fill = Sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Comparison of Functional Dispersion (FDis) of Fruit and Seed Traits",
    subtitle = paste(
      "Student's t-test - p =", 
      ifelse(teste_t$p.value < 0.001, "< 0.001", 
             format(round(teste_t$p.value, 3), nsmall = 3))
    ),
    y = "FDis (Functional Dispersion)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

setwd("C:/Users/Name/figures")
ggsave("fr.seed_fdis.jpg", plot = grafico_fdis, width = 8, height = 6, dpi = 300)

## FLOWERS - POLLINATION - CWM

library(readxl)
library(writexl)
library(FD)
library(vegan)

setwd("C:/Users/Name/github_repro_AFS_RES/Results")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/community.csv", 
                       row.names = 1, header = TRUE, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/functional_fl.csv",
                   row.names = 1, sep = ";")


comunidade <- as.matrix(comunidade)

# 1. Calculate CWM
cwm_resultados <- functcomp(traits, comunidade, CWM.type = "all")

head(cwm_resultados)

cwm_df <- as.data.frame(cwm_resultados)

cwm_df$sistema <- ifelse(grepl("AFS", rownames(cwm_df)), "AFS", "RES")

# Export to Excel
write_xlsx(as.data.frame(cwm_df), "CWM_Flowers_Pollination.xlsx")

cwm_df <- read_excel("C:/Users/Name/github_repro_AFS_RES/Results/CWM_Flowers_Pollination.xlsx")

# PLOT

length_flower <- "comp_fl_cm" ## FLOWER LENGTH

shapiro.test(cwm_df$comp_fl_cm) # not normal

# Wilcoxon Test
teste_wilcox <- wilcox.test(comp_fl_cm ~ sistema, data = cwm_df)
print(teste_wilcox) # p-value = 0.02067

### PLOT ###

grafico_cwm <- ggplot(cwm_df, aes(x = sistema, y = comp_fl_cm, fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Comparison of Flower Length CWM by System",
    subtitle = "Wilcoxon test - p = 0.021",
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_cwm)

setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_length_flower.jpg", plot = grafico_cwm, width = 8, height = 6, dpi = 300)

#
#
#
## FRUITS AND SEEDS - CWM
#
#
#

library(readxl)
library(writexl)
library(FD)
library(vegan)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Name/github_repro_AFS_RES")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/community.csv", 
                       row.names = 1, header = TRUE, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/data/functional_fr.seed.csv",
                   row.names = 1, sep = ";")

comunidade <- as.matrix(comunidade)

# Select only continuous traits
selected_traits <- traits[, c("comp_fr_cm", "comp_se_cm")]

# Calculate CWM for the two continuous traits
cwm_resultados <- functcomp(selected_traits, comunidade, CWM.type = "all")
cwm_df <- as.data.frame(cwm_resultados)
cwm_df$sistema <- ifelse(grepl("AFS", rownames(cwm_df)), "AFS", "RES")

write_xlsx(as.data.frame(cwm_df), "CWM_Fruits_Seeds.xlsx")

cwm_df <- read_excel("C:/Users/Name/github_repro_AFS_RES/Results/CWM_Fruits_Seeds.xlsx")

shapiro.test(cwm_df$comp_fr_cm) # FRUIT > normal

shapiro.test(cwm_df$comp_se_cm) # SEED > normal

# List of the analysed traits 
traits_fruit_seed <- c("comp_fr_cm", "comp_se_cm")

# Transform names of the traits
correct_names <- c(
  "comp_fr_cm" = "Fruit Length (cm)",
  "comp_se_cm" = "Seed Length (cm)"
)

# Student's t Test for Fruit Length
teste_t_fruto <- t.test(comp_fr_cm ~ sistema, data = cwm_df, var.equal = TRUE)

# Formatted p-value
p_valor_formatado_fruto <- ifelse(teste_t_fruto$p.value < 0.001, "< 0.001",
                                  format(round(teste_t_fruto$p.value, 3), nsmall = 3))

# Plot (Fruit)
grafico_fruto <- ggplot(cwm_df, aes(x = sistema, y = comp_fr_cm, fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Comparison of Fruit Length (cm) CWM by System",
    subtitle = paste("Student's t-test - p =", p_valor_formatado_fruto),
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_fruto)

setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_length_fruit.jpg", plot = grafico_fruto, width = 8, height = 6, dpi = 300)


# Student's t Test for Seed Length
teste_t_seed <- t.test(comp_se_cm ~ sistema, data = cwm_df, var.equal = TRUE)

# Formatted p-value
p_value_formatted_seed <- ifelse(teste_t_seed$p.value < 0.001, "< 0.001",
                                    format(round(teste_t_seed$p.value, 3), nsmall = 3))

# Boxplot
grafico_semente <- ggplot(cwm_df, aes(x = sistema, y = comp_se_cm, fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Comparison of Seed Length (cm) CWM by System",
    subtitle = paste("Student's t-test - p =", p_value_formatted_seed),
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_semente)

setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_length_seed.jpg", plot = grafico_semente, width = 8, height = 6, dpi = 300)

#######################################
########### ONLY NATIVES ################
#######################################

## FLOWERS - POLLINATION

library(readxl)
library(writexl)

setwd("C:/Users/Name/github_repro_AFS_RES/only_natives")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/community_only_natives.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/functional_fl_only_natives.csv",row.names = 1, sep = ";")

library(FD)
library(vegan)

# dbFD : x - df of functional traits; a - matrix of abundance; w.abun - should FDis, Rao’s Q, FEve, FDiv, and CWM be weighted by the relative abundances of the species?
# corr - quando não-euclidiana

comunidade <- as.matrix(comunidade)

# 1. Calculate FDis
fdis_results <- fdisp(gowdis(traits), comunidade)

# 2. Extract results by system
results_only_natives <- data.frame(
  Sistema = rownames(comunidade),
  FDis = fdis_results$FDis
)

write_xlsx(results_only_natives, "result_fdis.xlsx")

results_only_natives <- read_excel("C:/Users/Name/github_repro_AFS_RES/only_natives/result_fdis_flower.xlsx")

library(dplyr)
library(tidyverse)

# 3. Normality Test
shapiro_saf <- shapiro.test(results_only_natives$FDis[results_only_natives$Sistema == "AFS"]) # NORMAL
shapiro_rest <- shapiro.test(results_only_natives$FDis[results_only_natives$Sistema == "RES"]) # NORMAL

# 4. Select the appropriate Test (Student's t-test)
teste <- t.test(FDis ~ Sistema, data = results_only_natives, var.equal = TRUE)

# 5. Boxplot
grafico_fdis <- ggplot(results_only_natives, aes(x = Sistema, y = FDis, fill = Sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Functional Dispersion (FDis) of Floral Traits\nConsidering Only Native Species",
    subtitle = paste(
      "Student's t-test - p =", 
      ifelse(teste$p.value < 0.001, "< 0.001", 
             format(round(teste$p.value, 3), nsmall = 3))
    ),
    y = "FDis (Functional Dispersion)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# 6. Save Plot
setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("Flower_fdis_only_natives.jpg", plot = grafico_fdis, width = 8, height = 6, dpi = 300)

#
#
#
#
## FRUITS - DISPERSION
#
#
#
#

library(readxl)
library(writexl)
library(dplyr)
library(FD)
library(vegan)

setwd("C:/Users/Name/github_repro_AFS_RES/only_natives")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/community_only_natives.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/functional_fl_only_natives.csv",row.names = 1, sep = ";")

# dbFD : x - df of functional traits; a - matrix of abundance; w.abun - should FDis, Rao’s Q, FEve, FDiv, and CWM be weighted by the relative abundances of the species?
# corr - quando não-euclidiana

comunidade <- as.matrix(comunidade) # Ensure 'comunidade' is a matrix

# 1. Calculate FDis

fdis_results <- fdisp(gowdis(traits), comunidade)

# 2. Extract results by system
results_only_natives <- data.frame(
  Sistema = rownames(comunidade),
  FDis = fdis_results$FDis
)
write_xlsx(resultados_simples, "result_fdis_fruit.seed.xlsx") # Export to Excel
results_only_natives <- read_excel("C:/Users/Name/github_repro_AFS_RES/only_natives/result_fdis_fruit.seed.xlsx") # Read it back if needed

# 3. Normality Test
shapiro_saf <- shapiro.test(results_only_natives$FDis[results_only_natives$Sistema == "AFS"]) # NOT NORMAL
shapiro_rest <- shapiro.test(results_only_natives$FDis[results_only_natives$Sistema == "RES"]) # NORMAL

# 2. Select the appropriate Statistical Test
teste_wilcox <- wilcox.test(FDis ~ Sistema, data = results_only_natives) # p-value = 0.2786

# 3. Boxplot
grafico_fdis <- ggplot(results_only_natives, aes(x = Sistema, y = FDis, fill = Sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Functional Dispersion (FDis) of Fruit and Seed Traits 
    Considering Only Native Species",
    subtitle = paste(
      "Wilcoxon test - p =", 
      ifelse(teste_wilcox$p.value < 0.001, "< 0.001", 
             format(round(teste_wilcox$p.value, 3), nsmall = 3))
    ),
    y = "FDis (Functional Dispersion)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("fruit.seed_fdis_only_natives.jpg", plot = grafico_fdis, width = 8, height = 6, dpi = 300)

#
#
#
### FLOWERS - POLLINATION - CWM - ONLY NATIVES
#
#
#

library(readxl)
library(writexl)
library(FD)
library(vegan)


setwd("C:/Users/Name/github_repro_AFS_RES/only_natives")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/community_only_natives.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/functional_fl_only_natives.csv",row.names = 1, sep = ";")


comunidade <- as.matrix(comunidade)

# 1. Calculate CWM
cwm_resultados <- functcomp(traits, comunidade, CWM.type = "all")
cwm_df <- as.data.frame(cwm_resultados) # Convert to data frame
cwm_df$sistema <- ifelse(grepl("AFS", rownames(cwm_df)), "AFS", "RES") # Add system info
write_xlsx(as.data.frame(cwm_df), "CWM_Flowers_Pollination.xlsx") # Export to Excel
cwm_df <- read_excel("C:/Users/Name/github_repro_AFS_RES/only_natives/CWM_Flowers_Pollination.xlsx") # Read it back if needed

# 2. Normality Test # Select a continuous traits (flower length)

shapiro.test(cwm_df$comp_fl_cm) # not normal

# 3. Wilcoxon Test
teste_wilcox <- wilcox.test(comp_fl_cm ~ sistema, data = cwm_df)
print(teste_wilcox) # p-value = 0.3282

# 4. Boxplot
grafico_cwm <- ggplot(cwm_df, aes(x = sistema, y = comp_fl_cm, fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Community Weighted Mean (CWM) of Flower Length\nConsidering Only Native Species",
    subtitle = "Wilcoxon test - p = 0.3282",
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_cwm)
# Save
setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_flower_length_only_natives.jpg", plot = grafico_cwm, width = 8, height = 6, dpi = 300)

#
#
#
#

## FRUITS - DISPERSION - CWM - ONLY NATIVES

#
#
#
#

library(readxl)
library(writexl)
library(FD)
library(vegan)

setwd("C:/Users/Name/github_repro_AFS_RES/only_natives")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/community_only_natives.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/functional_fr.seed_only_natives.csv",row.names = 1, sep = ";")


# 1. Keep only the first column (fruit length)
traits <- traits[, 1, drop = FALSE]

comunidade <- as.matrix(comunidade) # Ensure 'comunidade' is a matrix 

# 2. Calculate CWM
cwm_results <- functcomp(traits, comunidade, CWM.type = "all")
cwm_df <- as.data.frame(cwm_results) # Convert to data frame
cwm_df$sistema <- ifelse(grepl("AFS", rownames(cwm_df)), "AFS", "RES") # Add system info
write_xlsx(cwm_df, "CWM_Fruit_Length_Only_Natives.xlsx") # Export to Excel
cwm_df <- read_excel("C:/Users/Name/github_repro_AFS_RES/only_natives/CWM_Fruit_Length_Only_Natives.xlsx") # Read it back if needed

# 3. Normality test
shapiro.test(cwm_df[[1]])  # not normal # p-value = 0.01881

# 4. Wilcoxon test (non-parametric)
teste_wilcox <- wilcox.test(cwm_df[[1]] ~ cwm_df$sistema)
print(teste_wilcox) # p-value = 0.2786

# 5. Boxplot
grafico_cwm <- ggplot(cwm_df, aes(x = sistema, y = .data[["comp_fr_cm"]], fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Community Weighted Mean (CWM) of Fruit Length\nConsidering Only Native Species",
    subtitle = paste("Wilcoxon test - p =", round(teste_wilcox$p.value, 3)),
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_cwm)

# Save
setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_fruit_length_only_natives.jpg", plot = grafico_cwm, width = 8, height = 6, dpi = 300)

#
#
#
#

## SEEDS - DISPERSION - CWM - ONLY NATIVES

#
#
#
#

library(readxl)
library(writexl)
library(FD)
library(vegan)
library(ggplot2)
library(dplyr)


setwd("C:/Users/Name/github_repro_AFS_RES/only_natives")

comunidade <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/community_only_natives.csv", row.names = 1,header = T, sep = ";", check.names = FALSE)

traits <- read.csv("C:/Users/Name/github_repro_AFS_RES/only_natives/functional_fr.seed_only_natives.csv",row.names = 1, sep = ";")

# 1. Select 2nd column (seed length)
traits <- traits[, 2, drop = FALSE]
comunidade <- as.matrix(comunidade) # Ensure 'comunidade' is a matrix 

# 2. Calculate CWM
cwm_results <- functcomp(traits, comunidade, CWM.type = "all")
cwm_df <- as.data.frame(cwm_results) # Convert to data frame
cwm_df$sistema <- ifelse(grepl("AFS", rownames(cwm_df)), "AFS", "RES") # Add system info
write_xlsx(cwm_df, "CWM_Seed_Length_Only_Natives.xlsx") # Export df
cwm_df <- read_excel("C:/Users/laila/OneDrive/Documentos/11. iniciacao_cientifica/CWM_Seed_Length_Only_Natives.xlsx") # Read it back if needed

# 3. Normality Test
shapiro.test(cwm_df[["comp_se_cm"]])  # p-value = 0.7843 (Normal)

# 4. Student's T Test
teste_t <- t.test(cwm_df[["comp_se_cm"]] ~ cwm_df$sistema) # p-value = 0.02072

## 5. Boxplot
grafico_cwm <- ggplot(cwm_df, aes(x = sistema, y = .data[["comp_se_cm"]], fill = sistema)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.color = "red") +
  labs(
    title = "Community Weighted Mean (CWM) of Seed Length\nConsidering Only Native Species",
    subtitle = paste("t-test - p =", round(teste_t$p.value, 3)),
    y = "Community Weighted Mean (CWM)",
    x = ""
  ) +
  scale_fill_manual(values = c("AFS" = "#1b9e77", "RES" = "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_cwm)
# Save
setwd("C:/Users/Name/github_repro_AFS_RES/figures")
ggsave("CWM_seed_length_only_natives.jpg", plot = grafico_cwm, width = 8, height = 6, dpi = 300)
