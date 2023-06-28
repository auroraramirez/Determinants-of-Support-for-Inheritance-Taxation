library(Boruta)
library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)
library(readODS)
library(hrbrthemes)
set.seed(12345)

# Cargamos la base y hacemos el apoyo una variable dicotómica
survey <- read_stata("raw/encuesta/RedistributionFinal.dta") %>%
  mutate(across(starts_with("sec3_p_33")|starts_with("sec3_p_34"), ~ifelse(.x == 3, 1, 0)),
         across(starts_with("sec3_p_36"), ~ifelse(.x %in% 3:5, 1, 0)),
         sec1_p_2 = ifelse(sec1_p_2 == 2, 1, 0),
         sec5_p_55 = ifelse(sec5_p_55 == 2, 1, 0),
         social_mobility = decil_actual - decil_14anios,
         reducir_desigualdad = ifelse(sec3_p_42 == 1, 1, 0),
         aumento_bienestar = ifelse(sec3_p_43 == 1, 1, 0))


controles <- c("sec1_p_2",
               "social_mobility",
               "index_wealth",
               "sec5_p_55",
               "sec5_p_58",
               "sec5_p_59",
               "sec3_p_25_2_3",
               "sec3_p_25_3_3",
               "sec3_p_25_4_3",
               "sec3_p_25_5_3",
               "sec3_p_30_b",
               "sec3_p_30_c",
               "sec3_p_29_a",
               "sec3_p_29_b",
               "sec3_p_29_c",
               "index_trust",
               "index_poverty",
               "index_ineq",
               "index_efficiency",
               "index_social",
               "index_econ")

dependientes <- c("sec3_p_36_3_a",
                  "sec3_p_36_3_b")

survey <- survey[,c(controles,dependientes)]

borutas <- list()
for(i in seq_along(dependientes)){
  formula <- as.formula(paste0(dependientes[i], "~ ."))
  borutas[[i]] <- Boruta(formula, 
                         data = survey[,union(controles, dependientes[i])],
                         maxRuns = 500)
  print(i)
}

bases_graficos <- list()

# Base de gráficos de BORUTA ----
for(i in seq_along(dependientes)){
  nombres <- c("Female",
               "Social mobility",
               "Wealth",
               "Has inherited",
               "How much expects to bequeath",
               "How much expects to receive in inheritance",
               "Tax evasion (poor households)",
               "Tax evasion (midde class households)",
               "Tax evasion (rich households)",
               "Tax evasion (very households)",
               "Perception of wealth of the rich by inheritance",
               "Perception of wealth of the rich by corruption",
               "Proportion of poor households",
               "Proportion of rich households",
               "Proportion of very rich households",
               "Trust in government and reciprocity",
               "Perception of poverty as explainable by personal effort",
               "Preoccupation for inequality",
               "Belief in efficiency effects",
               "Social progressivism",
               "Economic progressivism")
  
  bs <- borutas[[i]][["ImpHistory"]] %>% 
    as.data.frame()
  
  colnames(bs) <- c(nombres, "Max Shadow Variable", "Median Shadow Variable", "Min Shadow Variable")
  
  bases_graficos[[i]] <- bs %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "importancia") %>%
    mutate(importancia = ifelse(is.infinite(importancia), NA, importancia)) %>%
    group_by(variable) %>%
    left_join(data.frame(variable = nombres,
                         decision = borutas[[i]][["finalDecision"]])) %>%
    mutate(decision = case_when(decision == "Confirmed" ~ "Confirmed",
                                decision == "Rejected" ~ "Rejected",
                                decision == "Tentative" ~ "Tentative",
                                is.na(decision) ~ "Shadow Variable")) %>%
    filter(!(is.na(importancia)|is.nan(importancia)|is.infinite(importancia))) %>%
    mutate(variable = fct_reorder(variable, importancia, .fun = 'mean'))
    
}

graficos_boruta <- list()

for(i in seq_along(dependientes)){
  graficos_boruta[[i]] <- ggplot(data = bases_graficos[[i]], mapping = aes(y = reorder(variable, importancia), x = importancia, fill = decision)) +
    geom_boxplot(outlier.alpha = 0.25) +
    theme_ipsum_rc(base_size = 12, axis_title_size = 12) +
    ylab("") +
    xlab("Importance") +
    scale_fill_brewer(palette = "Set2",
                      name = "Decision")
}

save(borutas,
     graficos_boruta,
     file = "outputs/borutas_modelo.RData")
