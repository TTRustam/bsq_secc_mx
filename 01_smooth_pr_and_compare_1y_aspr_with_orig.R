source("00_read_and_prepare__prev_data_and_orog_aspr.R")

# smoothing function with additive effects
# negative binomial with log population offset
# 2-D tensor product spline on age and year
# with cubic regression spline with shrinkage by SEC_SECC
# the additive effect of SEC_SECC
# increased number of iterations

smooth_ungroup <- function(.data) {
  
  # Fit the GAM model
  data  <- .data
  data$SEC_SECC <- as.factor(data$SEC_SECC)
  
  model <- mgcv::gam(
    count ~ te(age, year, bs = "cs", by = SEC_SECC) + SEC_SECC,
    offset = log(pob),
    data   = data,
    family = mgcv::nb(),
    control = list(maxit = 500),
    select = FALSE,
    method = "REML"
  )
  
  # Create new data for prediction and predict in one step
  new_data <- expand_grid(
    age      = 0:85,
    year     = unique(data$year),
    SEC_SECC = unique(data$SEC_SECC)
  )
  
  # Add predictions
  new_data$mx <- predict(model, 
                         newdata = new_data, 
                         type    = "response")
  
  return(new_data)
  
}

# original aspr for comparison
compr <- prev_st_orig %>%
  filter(SEC_PROV == 20,
         SEC_MUNI_D == "Tolosa") %>% 
  mutate(type = "original") %>% 
  select(-c(SEC_MUNI))

# multicore calculation of smooth pr (Takes time)
future::plan(future::multisession, workers = 7)
prev1y <- prev %>%
  full_join(causes, join_by(condition)) %>%
  dplyr::select(-condition) %>%
  rename(condition = condition1) %>% 
  filter(SEC_PROV == 20,
         SEC_MUNI_D == "Tolosa") %>%
  select(-c(edadcat, codseccion, SEC_MUNI)) %>%
  group_nest(SEC_PROV, 
             SEC_MUNI_D,
             SEC_DIST,
             condition,
             sex) %>%
  mutate(data = furrr::future_map(data, ~ smooth_ungroup(.x)))  %>%
  unnest(data) %>% 
  mutate(type = "smooth")

# final wrangling for smooth 1-y prev
prev_final_1y <- prev1y %>%
  left_join(st1, by = join_by(age)) |>
  group_by(SEC_PROV, SEC_MUNI_D,
           SEC_SECC, SEC_DIST,
           year, sex, condition) %>% 
  summarize(aspr = sum(mx * standard), .groups = "drop") %>% 
  mutate(type = "smooth")

# join original and smooth data
test_data <- prev_final_1y %>%
  full_join(compr) %>% 
  dplyr::select(-c(SEC_PROV)) %>% 
  mutate(sex = str_to_title(sex),
         type = str_to_title(type)) 

# check the fit for chosen cause of death-
test_data %>%
  filter(condition == "Cardiopatía Isquémica") %>%
  ggplot(aes(x = year, y = aspr, lty = type, color = sex)) +
  geom_line() +
  facet_wrap(SEC_SECC ~ SEC_DIST, ncol = 7) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
