library(tidyverse)
library(RColorBrewer)
library(haven)
library(ggplot2)
library(extrafont)
library(hrbrthemes)
library(statar)
library(sandwich)
library(lmtest)  
library(readODS)

w <- 16
h <- 5

niveles_educ <- c("Sin educación",
                  "Primaria",
                  "Secundaria técnica",
                  "Secundaria general",
                  "Preparatoria técnica",
                  "Preparatoria general",
                  "Técnica o comercial",
                  "Técnica o comercial",
                  "Normal básica",
                  "Normal de licenciatura",
                  "Profesional",
                  "Posgrado")
niveles_educ2 <- c("Primaria incompleta o menos",
                   "Primaria completa",
                   "Secundaria completa",
                   "Preparatoria completa",
                   "Universitaria completa o más")
# Support plot -----
escala_azules <- RColorBrewer::brewer.pal(6, "Blues")[2:6] %>% rev()
likert <- c("Not at all",
            "A little",
            "Moderately",
            "Very much",
            "Completely")

g <- read_stata("raw/encuesta/RedistributionFinal.dta") %>%
  as.data.frame() %>%
  mutate(nivel_educ = case_when(yrs_educ %in% 0:5 ~ 0,
                                yrs_educ %in% 6:8 ~ 1,
                                yrs_educ %in% 9:11 ~ 2,
                                yrs_educ %in% 12:15 ~ 3,
                                yrs_educ %in% 16:18 ~ 4))

g$nivel_educ <- factor(g$nivel_educ, levels = 0:4, labels = niveles_educ2, ordered = TRUE)
g$sec1_p_4 <- factor(g$sec1_p_4, labels = niveles_educ)
g$sec3_p_36_3_a <- ordered(g$sec3_p_36_3_a, levels = 1:5, labels = likert)
g$sec3_p_36_3_b <- ordered(g$sec3_p_36_3_b, levels = 1:5, labels = likert)

g1 <- g %>%
  group_by(sec3_p_36_3_a) %>%
  tally() %>%
  mutate(tax = "no_exemption") %>%
  rename(categories = sec3_p_36_3_a)

g1 <- g %>%
  group_by(sec3_p_36_3_b) %>%
  tally() %>%
  mutate(tax = "10_million") %>%
  rename(categories = sec3_p_36_3_b) %>%
  bind_rows(g1) %>%
  ungroup() %>%
  group_by(tax)

g1$tax <- factor(g1$tax, levels = c("no_exemption", "10_million"), labels = c("No threshold", "> USD $1 million (PPP)"))
  
support <- ggplot(data = g1, mapping = aes(x = tax, y = n, fill = forcats::fct_rev(categories))) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  ylab("") +
  xlab("") +
  scale_fill_manual(name = "Level of support", values = escala_azules) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_family = "serif")

ggsave(filename = "figure_1.wmf",
       plot = support,
       path = "plots", 
       device = "wmf",
       width = w,
       height = h,
       units = "cm")

# Perceptions of legitimacy of inherited wealth -----
questions <- c("sec3_p_24_j", # Inherited wealth is as deserved as the self-made wealth
               "sec3_p_24_m", # It is fair to have an inheritance tax when somebody dies
               "sec3_p_24_o") # The government shouldn't interfere in transfers of wealth between parent and children

enunciados <- c("Inherited wealth is as deserved\n as self-made wealth",
                "It is fair to have an inheritance\n tax when somebody dies",
                "The government should not interfere\n in transfers of wealth between\n parents and children")
names(enunciados) <- questions

for(var in questions){
  gx <- g %>%
    ungroup() %>%
    group_by(.data[[var]]) %>%
    tally() %>%
    mutate(question = enunciados[[var]]) %>%
    rename(categories = .data[[var]])
  
  if(var == "sec3_p_24_j") g3 <- gx else g3 <- bind_rows(g3,gx)
  rm(gx)
}

g3$question <- factor(g3$question, levels = enunciados, labels = enunciados)
g3$categories <- ordered(g3$categories, levels = 1:5, labels = likert)

legitimacy <- ggplot(data = g3, mapping = aes(x = question, y = n, fill = forcats::fct_rev(categories))) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  ylab("") +
  xlab("") +
  scale_fill_manual(name = "Level of agreement", values = escala_azules) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_family = "serif")

ggsave(filename = "figure_2.wmf",
       plot = legitimacy,
       path = "plots", 
       device = "wmf",
       width = w,
       height = h,
       units = "cm")

# Boruta ----
load(file = "outputs/borutas_modelo.RData")
for(i in seq_along(dependientes)){
  nombres <- c("Female",
               "Social mobility",
               "Wealth",
               "Has inherited",
               "How much expects to leave in inheritance",
               "How much expects to receive in inheritance",
               "Tax evasion (poor households)",
               "Tax evasion (midde class households)",
               "Tax evasion (rich households)",
               "Tax evasion (very rich households)",
               "Origins of wealth of the rich (inheritance)",
               "Origins of wealth of the rich (corruption)",
               "Proportion of poor households",
               "Proportion of rich households",
               "Proportion of very rich households",
               "Trust in government and reciprocity",
               "Perceptions of poverty",
               "Concern about inequality",
               "Belief in efficiency effects",
               "Social progressivism",
               "Economic progressivism")
  
  bs <- borutas[[i]][["ImpHistory"]] %>% 
    as.data.frame()
  
  colnames(bs) <- c(nombres, "Max. shadow variable", "Median shadow variable", "Min shadow variable")
  
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
    mutate(variable = fct_reorder(variable, importancia, .fun = 'mean')) %>%
    filter(!is.na(importancia))
}

plots_boruta <- list()

n <- c("a", "b")
for(i in seq_along(dependientes)){
  
  if(i == 1) impuesto <- "approval_everyone" else impuesto <- "approval_rich"
    
  plots_boruta[[i]] <- ggplot(data = bases_graficos[[i]], mapping = aes(y = reorder(variable, importancia), x = importancia, fill = decision)) +
    geom_boxplot(outlier.colour = "grey") +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(legend.position="none")+
    ylab("") +
    xlab("Importance") +
    xlim(-4,35) +
    scale_fill_brewer(palette = "Set2",
                      name = "Decision")
  
  ggsave(filename = sprintf("figure_3_%s.wmf", n[i]),
         plot = plots_boruta[[i]],
         path = "plots", 
         device = "wmf",
         width = 13.17,
         height = 7.71,
         units = "cm")
}
