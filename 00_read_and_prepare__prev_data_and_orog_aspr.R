library(rmapshaper)
library(patchwork)
library(tidyverse)
library(DemoTools)
library(cowplot)
library(readxl)
library(scales)
library(ggpubr)
library(furrr)
library(mgcv)
library(sf)

# municipality names
mun_names <- read_excel("MunicipiosCAE_2024_17_01.xlsx", 
                        sheet = "EAE", 
                        skip = 9,
                        col_names = FALSE) %>% 
  set_names(c("SEC_PROV", "SEC_MUNI", "SEC_MUNI_D"))

# EU standard of population 2013
st <- data.frame(matrix(c(
  0,	0.05,
  5,	0.055,
  10,	0.055,
  15,	0.055,
  20,	0.06,
  25,	0.06,
  30,	0.065,
  35,	0.07,
  40,	0.07,
  45,	0.07,
  50,	0.07,
  55,	0.065,
  60,	0.06,
  65,	0.055,
  70,	0.05,
  75,	0.04,
  80,	0.025,
  85,	0.025), 
  ncol = 2, 
  byrow = TRUE)) |>
  rename(age = 1, 
         standard = 2)

# graduate the standard 5-year into 1 year age
st1 <- tibble(
  standard = graduate_sprague(st$standard, st$age, OAG = TRUE),
  age      = 0:85)

# read and prepare the initial prevalence data
all_data <- list()

for (yr in 2016:2023) {
  
  yri <- read_delim(paste0("datos/agregados ", yr, "12 capv.csv"), 
                    delim = ";",
                    col_types ="cccddddddddddd") |> 
    mutate(year = yr, .before = 1)
  all_data[[as.character(yr)]] <- yri
  
}

# initial preparation of prevalence data
prev <- bind_rows(all_data) |> 
  mutate(age = parse_number(edadcat)) |> 
  arrange(year, codseccion, sex, age) |>
  # remove missing
  filter(!is.na(codseccion)) |> 
  # separate codeseccion in geographical units
  mutate(
    codseccion = str_pad(codseccion, 10, side = "left", pad = "0"),
    SEC_PROV   = substr(codseccion, 1, 2),
    SEC_MUNI   = substr(codseccion, 3, 5),
    SEC_DIST   = substr(codseccion, 6, 7),
    SEC_SECC   = substr(codseccion, 8, 10)) |>
  # add municip names
  left_join(mun_names, by = join_by(SEC_PROV, SEC_MUNI)) |>
  pivot_longer(ic:cron, 
               names_to  = "condition", 
               values_to = "count") 

# full causes of deaths names
causes <- tibble(condition  = unique(prev$condition), 
                 condition1 = c("cerebrovascular",
                                "cardiopatía isquémica",
                                "patología crónica",
                                "depresión",
                                "diabetes mellitus",
                                "pulmonar obstructiva crónica",
                                "hipertensión",
                                "insuficiencia cardiaca",
                                "insuficiencia renal crónica",
                                "pluripatológicos")) %>% 
  mutate(condition1 = str_to_title(condition1))

# calculate standardized prevalence from original data
prev_st_orig <- prev %>%
  full_join(causes, join_by(condition)) %>% 
  dplyr::select(-condition) %>% 
  rename(condition = condition1) %>%
  left_join(st, by = join_by(age)) |> 
  group_by(SEC_PROV, SEC_MUNI, SEC_MUNI_D, SEC_DIST, 
           SEC_SECC, year, sex, condition) %>% 
  summarize(aspr = sum(count / pob * standard), .groups = "drop")