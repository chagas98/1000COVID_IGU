#########################################Libraries#########################################
library(dplyr)    
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyverse)
library(tidylog)
library(ggpubr)
library(rmarkdown)
library(gtsummary)
library(xlsx)
library(webshot)

#Delete previous variables
rm(list=ls())

#Setting File path - select and set your own file path as the example here. 
work_dir <- "/Users/samue/OneDrive/�rea de Trabalho/Samuel/Projeto de Pesquisa/Levantamento Wel"   
setwd(work_dir)
setwd
getwd

data0 <-
  read.csv("datacovid_1000_oficial.csv",
           stringsAsFactors = F,
           sep = ",")
data0 <- data0[!grepl("mes", data0$Idade),]
data0 <- data0[!grepl("meses", data0$Idade),]
data0$Idade <- as.numeric(as.character(data0$Idade))

data <- filter(data0, Idade > 10)
paged_table(data)


#Setting comorbidities data

comorbidades <- data %>%
  mutate(diabetes = case_when(
    str_detect(Comorb, "Diabetes") == TRUE ~ TRUE,
    str_detect(Comorb, "Diabetes") == FALSE ~ FALSE
  )) %>%
  mutate(hipertension = case_when(
    str_detect(Comorb, "Hipertensão") == TRUE ~ TRUE,
    str_detect(Comorb, "Hipertensão") == FALSE ~ FALSE
  )) %>%
  mutate(d_cardio = case_when(
    str_detect(Comorb, "Doença cardiovascular") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença cardiovascular") == FALSE ~ FALSE
  )) %>%
  mutate(renal = case_when(
    str_detect(Comorb, "Doença Renal") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Renal") == FALSE ~ FALSE
  )) %>%
  mutate(pulmonar = case_when(
    str_detect(Comorb, "Doença Pulmonar") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Pulmonar") == FALSE ~ FALSE
  )) %>%
  mutate(neopla = case_when(
    str_detect(Comorb, "Neoplasia") == TRUE ~ TRUE,
    str_detect(Comorb, "Neoplasia") == FALSE ~ FALSE
  )) %>%
  mutate(immunodef = case_when(
    str_detect(Comorb, "Imunodeficiência") == TRUE ~ TRUE,
    str_detect(Comorb, "Imunodeficiência") == FALSE ~ FALSE
  )) %>%
  mutate(hiv = case_when(
    str_detect(Comorb, "Infecção HIV") == TRUE ~ TRUE,
    str_detect(Comorb, "Infecção HIV") == FALSE ~ FALSE
  )) %>%
  mutate(neuro = case_when(
    str_detect(Comorb, "Doença Neurológica") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Neurológica") == FALSE ~ FALSE
  )) %>%
  mutate(down = case_when(
    str_detect(Comorb, "Síndrome de Down") == TRUE ~ TRUE,
    str_detect(Comorb, "Síndrome de Down") == FALSE ~ FALSE
  )) %>%
  mutate(obesity = case_when(
    str_detect(Comorb, "Obesidade") == TRUE ~ TRUE,
    str_detect(Comorb, "Obesidade") == FALSE ~ FALSE
  )) %>%
  mutate(others = case_when(
    str_detect(Comorb, "Outras") == TRUE ~ TRUE,
    str_detect(Comorb, "Outras") == FALSE ~ FALSE
  )) %>%
  mutate(none_comor = case_when(
    str_detect(Comorb, "Sem morbidades") == TRUE ~ TRUE,
    str_detect(Comorb, "Sem morbidades") == FALSE ~ FALSE
  )) %>%
  mutate(comor_9 = case_when(
    str_detect(Comorb, "Ignorado") == TRUE ~ TRUE,
    str_detect(Comorb, "Ignorado") == FALSE ~ FALSE
  ))

#########################################Figure 1#########################################

#Setting Data
df_age_outcome <- data %>%
  mutate(
    desfec = case_when(
      Desfecho == "Domicílio após coleta exame" ~ "Homecare",
      Desfecho == "UTI COVID" ~ "ICU",
      Desfecho == "Unidade de Internação COVID (Enfermaria, UCE, UTDI)" ~ "Hospitalization",
      Desfecho == "PA/PS  Respiratório" ~ "PA-PS",
      Desfecho == "Óbito" ~ "Death"
    )
  )

#Sorting by levels
df_age_outcome$desfec <-
  factor(df_age_outcome$desfec,
         levels = c("Homecare", "PA-PS", "Hospitalization", "ICU", "Death"))

#Plotting
plot1 <-
  ggplot(df_age_outcome, aes(x = desfec, y = Idade, fill = desfec)) +
  geom_violin(
    trim = FALSE,
    size = 1,
    fill = "#EAECEE",
    colour = "#566573"
  ) +
  geom_boxplot(
    width = 0.02,
    outlier.shape = NA,
    fill = "#EAECEE",
    color = "black"
  ) +
  labs(x = " ", y = "Age") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 0.6 * 20),
    axis.title.y = element_text(size = 0.6 * 20),
    axis.text.y = element_text(size = 0.6 * 17)
  )

plot1

###############################Figure 2################################################3
#Graph colors
colors <- c("#ABB2B9",
            "#808B96",
            "#566573",
            "#2C3E50",
            "#17202A")

#Translation

df_geral <- comorbidades %>%
  mutate(sum = rowSums(.[37:48])) %>%
  mutate(cat_comor = cut(
    sum,
    breaks = c(seq(-1, 2, 1), 10),
    labels = c("No comorbidities", "1 comorbidity", "2 comorbidities", "\u2265 3 comorbidities"),
    include.lowest = TRUE
  )) %>%
  mutate(
    desfec = case_when(
      Desfecho == "Domicílio após coleta exame" ~ "Homecare",
      Desfecho == "UTI COVID" ~ "Hospital admission",
      Desfecho == "Unidade de Internação COVID (Enfermaria, UCE, UTDI)" ~ "Hospital admission",
      Desfecho == "PA/PS  Respiratório" ~ "Hospital admission",
      Desfecho == "Óbito" ~ "Death"
    )
  ) %>%
  mutate(
    rac = case_when(
      Raca == "�\u008dndio" ~ "Indigenous",
      Raca == "Parda " ~ "Brown",
      Raca == "parda" ~ "Brown",
      Raca == "Parda" ~ "Brown",
      Raca == "Branca" ~ "White",
      Raca == "Indio" ~ "Indigenous",
      Raca == "Negra" ~ "Black",
      Raca == "" ~ "Missing",
      Raca == "Ignorado" ~ "Missing",
      Raca == "Amarela" ~ "Asian"
    )
  ) %>%
  mutate(sex = case_when(
    Sexo == "M" ~ "Male",
    Sexo == "F" ~ "Female",
    Sexo == "" ~ "Missing"
  )) %>%
  mutate(age_bin = cut(
    Idade,
    breaks = c(seq(0, 90, 20), 160),
    labels = c("10 to 29",
               "30 to 49",
               "50 to 69",
               "70 to 89",
               "90+"),
    include.lowest = TRUE
  )) %>%
  mutate(sex = case_when(
    Sexo == "M" ~ "Male",
    Sexo == "F" ~ "Female",
    Sexo == "" ~ "Missing"
  )) %>%
  mutate(
    desfec1 = case_when(
      Desfecho == "Domicílio após coleta exame" ~ "Homecare",
      Desfecho == "UTI COVID" ~ "ICU",
      Desfecho == "Unidade de Internação COVID (Enfermaria, UCE, UTDI)" ~ "Hospital admission",
      Desfecho == "PA/PS  Respiratório" ~ "PA-PS",
      Desfecho == "Óbito" ~ "Death"
    )
  )

#A. Stratified by sex 
plot2 <- df_geral %>%
  filter(sex != "Missing") %>%
  filter(desfec == "Hospital admission") %>%
  ggplot(., aes(x = age_bin, fill = sex)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           position = position_dodge(preserve = "single"),
           color = "black") +
  scale_y_continuous(
    labels = function(x)
      paste0(x * 100, "%"),
    limits = c(0, 0.3)
  ) +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[2]
    ),
    name = ""
  ) +
  labs(x = " ",
       y = "In-Hospital (%)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )


#B. Stratified by hospital units
df_geral$desfec1 <-
  factor(df_geral$desfec1, levels = c("PA-PS", "Hospital admission", "ICU"))

plot3 <- df_geral %>%
  filter(desfec1 != "Missing") %>%
  filter(desfec == "Hospital admission") %>%
  ggplot(., aes(x = age_bin, fill = desfec1)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           position = position_dodge(preserve = "single"),
           color = "black") +
  scale_y_continuous(
    labels = function(x)
      paste0(x * 100, "%"),
    limits = c(0, 0.3)
  ) +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[2],
      colors[3],
      colors[4]
    ),
    name = ""
  ) +
  labs(x = " ",
       y = " ") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )

#C. Stratified by N. of comorbidities
plot4 <- df_geral %>%
  filter(comor_9 != TRUE) %>%
  filter(desfec == "Hospital admission") %>%
  ggplot(., aes(x = age_bin, fill = cat_comor)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           position = position_dodge(),
           color = "black") +
  scale_y_continuous(
    labels = function(x)
      paste0(x * 100, "%"),
    limits = c(0, 0.3)
  ) +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[2],
      colors[3],
      colors[4],
      colors[5]
    ),
    name = ""
  ) +
  labs(x = "Age bin (Years)",
       y = "In-Hospital (%)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )

#D. Stratified by self-reported race

df_geral$rac <-
  factor(df_geral$rac,
         levels = c("Brown", "White", "Black", "Indigenous", "Asian", "Missing"))
plot5 <- df_geral %>%
  filter(rac != "Missing") %>%
  filter(desfec == "Hospital admission") %>%
  ggplot(., aes(x = age_bin, fill = rac)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           position = position_dodge(preserve = "single"),
           color = "black") +
  scale_y_continuous(
    labels = function(x)
      paste0(x * 100, "%"),
    limits = c(0, 0.3)
  ) +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[2],
      colors[3],
      colors[4],
      colors[5]
    ),
    name = ""
  ) +
  labs(x = "Age bin (Years)",
       y = " ") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  )

#Combine and plot
combined_plot <-
  ggpubr::ggarrange(
    plot2,
    plot3,
    plot4,
    plot5,
    heights = c(2, 2),
    nrow = 2,
    ncol = 2,
    labels = c("A", "B", "C", "D"),
    font.label = list(size = 14, face = "bold", color ="black"),
    align = "hv"
  )
combined_plot


#################################Figure 3##################################################
#A. Proportion of symptoms reported 
symp <- df_geral %>% select(
  Epigastralgia = Epigastralgia,
  Chills = Calafrios,
  Vomit = vomito,
  Diarrhoea = diarreia,
  Arthralgia = artralgia,
  Anosmia = anosmia,
  Ageusia = ageusia,
  Odinophagy = odino,
  Headache = cefaleia,
  Adynamia = adinamia,
  Cough = tosse,
  Hemoptysis = hemop,
  Coryza = coriza, 
  "Nasal Congestion" = congest_nasal,
  Dyspnoea = dispneia,
  Myalgia = mialgia,
  Prostration = prostra,
  Asthenia = astenia,
  Inapetence = inapet,
  Fatigue = fadiga,
  Nausea = nausea,
  "Conjunctive congestion" = congest_conj)

symp <- map_df(symp, ~ ifelse(.x == 2, 0, ifelse(.x == 9, NA_integer_, .x)))

symp <- gather(symp, key = "Sintoma", value = "Valor") %>% 
  group_by(Sintoma, Valor) %>% 
  tally() 

symp <- symp %>%
  ungroup() %>%
  mutate(Valor = case_when(
    is.na(Valor) ~ "Missing",
    Valor == 1 ~ "Yes",
    Valor == 0 ~ "No"
  ))

# Calculate the proportion of patients with each symptom
symp <- symp %>%
  group_by(Sintoma) %>% 
  mutate(total = sum(n),
         prop = n / total) 

# Order for the plot to be stacked in
order <- symp %>% 
  ungroup() %>%
  filter(Valor == "Yes") %>% 
  arrange(desc(prop)) %>% 
  mutate(order = 1:nrow(.))

symp <- symp %>% left_join(order %>% select(Sintoma, order))
symp <- symp %>% mutate(Valor = factor(Valor, levels = c("Missing","No", "Yes")))
plot6 <- ggplot(symp) +
  geom_col(aes(x = prop*100, y = reorder(Sintoma, -order), fill = Valor), color = "black") +
  labs(x = "Proportion of patients 
with symptoms (%)", 
       y = "",
       fill = "") +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[2],
      colors[3],
      name = ""
    ))+
  theme_classic()


#B. Proportion of comorbidities reported
plot7 <- df_geral %>% select(
  Diabetes = diabetes,
  Hipertension = hipertension,
  "Cardiovascular disease" = d_cardio,
  Nephritis = renal,
  "Lung Disease" = pulmonar,
  Neoplasm = neopla,
  Immunosuppression = immunodef,
  HIV = hiv,
  "Neurological Disease" = neuro,
  "Down Syndrome" = down,
  Obesity = obesity,
  Others = others)

plot7[1:12] <- map_df(plot7[1:12], ~ ifelse(.x == FALSE, 0, ifelse(.x == 9, NA_integer_, .x)))

# Long format
plot7 <- gather(plot7, key = "comorb", value = "Valor") %>% 
  group_by(comorb, Valor) %>% 
  tally() 

plot7 <- plot7 %>%
  ungroup() %>%
  mutate(Valor = case_when(
    is.na(Valor) ~ "Missing",
    Valor == TRUE ~ "Yes",
    Valor == FALSE ~ "No"
  ))

# Calculate the proportion of patients with each comorbidity 
plot7 <- plot7 %>%
  group_by(comorb) %>% 
  mutate(total = sum(n),
         prop = n / total) 

order <- plot7 %>% 
  ungroup() %>%
  filter(Valor == "Yes") %>% 
  arrange(desc(prop)) %>% 
  mutate(order = 1:nrow(.))

plot7 <- plot7 %>% 
  left_join(order %>% select(comorb, order))

plot7 <- plot7 %>% mutate(Valor = factor(Valor, levels = c("Missing","No", "Yes")))

plot7c <- ggplot(plot7) +
  geom_col(aes(x = prop*100, y = reorder(comorb, -order), fill = Valor), color = "black") +
  labs(x = "Proportion of patients 
with comorbidities (%)", 
       y = "",
       fill = "") +
  scale_fill_manual(
    values = c(
      colors[1],
      colors[3],
      name = ""
    )) +
  ggtitle("") +
  theme_classic()


combined_plot1 <-
  ggpubr::ggarrange(
    plot6,
    plot7c,
    nrow = 1,
    ncol = 2,
    align = "hv",
    common.legend = TRUE,
    legend = "bottom",
    labels = c("A", "B"),
    font.label = list(size = 14, face = "bold", color ="black")
  )
combined_plot1


######################################TABLE1##############################################

#Data Input and Filtration
data_table <-
  read.csv("datacovid_1000_oficial.csv",
           stringsAsFactors = F,
           sep = ",")

data_table <- data_table[!grepl("mes", data_table$Idade),]
data_table <- data_table[!grepl("meses", data_table$Idade),]
data_table$Dias_sint <- as.numeric(as.character(data_table$Dias_sint))
data_table$Idade <- as.numeric(as.character(data_table$Idade))
data_tab <- filter(data_table, Idade > 10)

#Classification and Translation
df_data <- data_tab %>% 
  mutate(age_bin = cut(
    Idade,
    breaks = c(seq(10, 90, 10), 150),
    labels = c(
      "10 to 19",
      "20 to 29",
      "30 to 39",
      "40 to 49",
      "50 to 59",
      "60 to 69",
      "70 to 79",
      "80 to 89",
      "90+"
    ),
    include.lowest = TRUE
  )) %>% 
  mutate(
    desfec = case_when(
      Desfecho == "Domicílio após coleta exame" ~ "No admission",
      Desfecho == "UTI COVID" ~ "Admission",
      Desfecho == "Unidade de Internação COVID (Enfermaria, UCE, UTDI)" ~ "Admission",
      Desfecho == "PA/PS  Respiratório" ~ "Admission",
      Desfecho == "Óbito" ~ "Death"
    )
  ) %>% 
  mutate(sex = case_when(
    Sexo == "M" ~ "Male",
    Sexo == "F" ~ "Female",
    Sexo == "" ~ NA_character_)) %>% 
  mutate(
    rac = case_when(
      Raca == "�\u008dndio" ~ "Indigenous",
      Raca == "Parda " ~ "Brown",
      Raca == "parda" ~ "Brown",
      Raca == "Parda" ~ "Brown",
      Raca == "Branca" ~ "White",
      Raca == "Indio" ~ "Indigenous",
      Raca == "Negra" ~ "Black",
      Raca == "" ~ NA_character_,
      Raca == "Ignorado" ~ NA_character_,
      Raca == "Amarela" ~ "Asian"
    )
  ) %>% 
  mutate(diabetes = case_when(
    str_detect(Comorb, "Diabetes") == TRUE ~ TRUE,
    str_detect(Comorb, "Diabetes") == FALSE ~ FALSE
  )) %>%
  mutate(hipertensao = case_when(
    str_detect(Comorb, "Hipertensão") == TRUE ~ TRUE,
    str_detect(Comorb, "Hipertensão") == FALSE ~ FALSE
  )) %>%
  mutate(d_cardio = case_when(
    str_detect(Comorb, "Doença cardiovascular") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença cardiovascular") == FALSE ~ FALSE
  )) %>%
  mutate(renal = case_when(
    str_detect(Comorb, "Doença Renal") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Renal") == FALSE ~ FALSE
  )) %>%
  mutate(pulmonar = case_when(
    str_detect(Comorb, "Doença Pulmonar") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Pulmonar") == FALSE ~ FALSE
  )) %>%
  mutate(neoplasia = case_when(
    str_detect(Comorb, "Neoplasia") == TRUE ~ TRUE,
    str_detect(Comorb, "Neoplasia") == FALSE ~ FALSE
  )) %>%
  mutate(imunodef = case_when(
    str_detect(Comorb, "Imunodeficiência") == TRUE ~ TRUE,
    str_detect(Comorb, "Imunodeficiência") == FALSE ~ FALSE
  )) %>%
  mutate(hiv = case_when(
    str_detect(Comorb, "Infecção HIV") == TRUE ~ TRUE,
    str_detect(Comorb, "Infecção HIV") == FALSE ~ FALSE
  )) %>%
  mutate(neuro = case_when(
    str_detect(Comorb, "Doença Neurológica") == TRUE ~ TRUE,
    str_detect(Comorb, "Doença Neurológica") == FALSE ~ FALSE
  )) %>%
  mutate(down = case_when(
    str_detect(Comorb, "Síndrome de Down") == TRUE ~ TRUE,
    str_detect(Comorb, "Síndrome de Down") == FALSE ~ FALSE
  )) %>%
  mutate(obesidade = case_when(
    str_detect(Comorb, "Obesidade") == TRUE ~ TRUE,
    str_detect(Comorb, "Obesidade") == FALSE ~ FALSE
  )) %>%
  mutate(outras = case_when(
    str_detect(Comorb, "Outras") == TRUE ~ TRUE,
    str_detect(Comorb, "Outras") == FALSE ~ FALSE
  )) %>%
  mutate(sem_comor = case_when(
    str_detect(Comorb, "Sem morbidades") == TRUE ~ TRUE,
    str_detect(Comorb, "Sem morbidades") == FALSE ~ FALSE
  )) %>%
  mutate(comor_9 = case_when(
    str_detect(Comorb, "Ignorado") == TRUE ~ TRUE,
    str_detect(Comorb, "Ignorado") == FALSE ~ FALSE
  )) %>%
  mutate(sum = rowSums(.[41:52])) %>%
  mutate(cat_comor = cut(
    sum,
    breaks = c(seq(-1, 2, 1), 10),
    labels = c("No comorbidities", "1 comorbidity", "2 comorbidities", "\u2265 3 comorbidities"),
    include.lowest = TRUE
  ))

#Missing values definied as NA values
df_data$cat_comor[df_data$comor_9 == TRUE] <- NA_character_

df_data[, c(10:35)] <- map2_df(df_data[, c(10:34)], names(df_data[, c(10:34)]), ~ case_when(
  .x == 1 ~ .y,
  .x == 2 ~ NA_character_,
  .x == 3 ~ "Ex",
  .x == 9 ~ NA_character_
))



df_data2 <- df_data %>%
  select("Symptoms Appearance" = Dias_sint,
         Sex = sex,
         Age = Idade,
         "Age bin" = age_bin,
         "Self-reported Race" = rac,
         Outcome = desfec,
         "Num. of Comorbidities" = cat_comor,
         Diabetes = diabetes,
         Hipertension = hipertensao,
         "Cardiovascular Disease" = d_cardio,
         Nephritis = renal,
         "Lung Disease" = pulmonar,
         "Neoplasm" = neoplasia,
         "Immunosupression" = imunodef,
         HIV = hiv,
         "Neurological Disease" = neuro,
         "Down Sydrome" = down,
  )

#Plotting table as .html file.

table1 = df_data2 %>% 
  tbl_summary (by = Outcome,
               missing= "no",
               type = all_continuous() ~ "continuous2",
               statistic = list(all_categorical() ~ "{n} ({p}%)",
                                all_continuous() ~ c("{mean} ({sd})",
                                                     "{median} ({p25}-{p75})", 
                                                     "{min}, {max}")
               )) %>% 
  add_overall() %>%
  bold_labels %>% 
  add_n() %>% 
  as_gt()# %>%
  gt::gtsave(filename = "tabela.html")

table1