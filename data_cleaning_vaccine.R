# Read and process vaccine data ------------------------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(truncreg)
library(gtsummary)
library(oxthema)


## Read vaccine data ----
vaccine <- read_xlsx(
  file = "data/vaccine.xlsx", sheet = 1, rows = 1:295, cols = 1:29
)


## Process vaccine data ----

vaccine_clean <- vaccine |>
  rename(
    id = D,
    sex = Gender,
    comorbid = BirthComor,
    bmi = `BMI measure`,
    bmi_class = BMI,
    vacc_status = Vacstat,
    vacc1_date = `Date 1`,
    vacc2_date = `Date 2`,
    age = Age,
    class = Class,
    inf_prior_vacc = `Infected prior to vaccination`,
    qvac_1 = `Quantivac at 28 days`,
    q_1 = `Q 28 Result`,
    od_1 = ODQ28Days,
    ncp_1 = `NCPat28 days`,
    n_1 = N28Results,
    qvac_4 = `Quantivacat3 months`,
    q_4 = `Q3 Result`,
    od_4 = ODQ4months,
    ncp_4 = NCPat3months,
    n_4 = N3Result,
    qvac_7 = Quantivacat7months,
    q_7 = Q7Result,
    od_7 = ODQ7months,
    ncp_7 = NCPat7months,
    n_7 = N7Result,
    time = Time,
    status = Status,
    truncated_titer = TruncatedTiter
  ) |>
  pivot_longer(
    cols = qvac_1:n_7, names_to = c(".value", "months"), names_sep = "_"
  ) |>
  mutate(
    sex = ifelse(sex == "M", "Male", "Female"),
    comorbid = case_when(
      comorbid == "NA" ~ NA_character_,
      comorbid == "Y" ~ "Yes",
      comorbid == "N" ~ "No"
    ),
    bmi_class = sub(pattern = " Weight", replacement = "", x = bmi_class) |>
      factor(levels = c("Normal", "Underweight", "Overweight", "Obese")),
    vacc_status = ifelse(
      vacc_status == "Non Vaccinated", "Not vaccinated", "Vaccinated"
    ),
    age = factor(age, levels = 11:16),
    inf_prior_vacc = ifelse(inf_prior_vacc == "N", "No", "Yes"),
    months = paste("Month", months, sep = " ")
  )


## Examples of summaries and visualisation ----

### Mean optical density for each round of observation ----
vaccine_clean |>
  summarise(
    mean_od = mean(od),
    .by = months
  )

### Five number summary for each round of observation ----
vaccine_clean |>
  summarise(
    min_od = min(od),
    first_quartile_od = quantile(od, probs = 0.25),
    median_od = median(od),
    mean_od = mean(od),
    third_quartile_od = quantile(od, probs = 0.75),
    max_od = max(od),
    .by = months
  )

### Summary table using gtsummary ----

vaccine_clean |>
  tbl_strata(
    strata = c(months, vacc_status), .tbl_fun = tbl_summary,
    label = list(
      sex = "Sex", 
      age = "Age (years)", 
      comorbid = "Known comorbidity in childhood",
      bmi = "Body mass index (kg/m2)",
      bmi_class = "Body mass index (category)",
      inf_prior_vacc = "COVID infection prior to study/vaccination",
      od = "Optical density"
    ),
    include = c(sex, age, comorbid, bmi, bmi_class, inf_prior_vacc, od)
  ) |>
  bold_labels()


### Boxplot of optical density by observation round ----
vaccine_clean |>
  ggplot(mapping = aes(x = vacc_status, y = od)) +
  geom_boxplot(
    colour = get_oxford_colour("Oxford blue"),
    fill = get_oxford_colour("Oxford blue"),
    linewidth = 1, alpha = 0.3
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 0.5)) +
  labs(
    title = "Optical density by vaccination status per observation month",
    x = "Vaccination status", y = "Optical Density"
  ) +
  facet_wrap(. ~  months, ncol = 3) +
  theme_oxford(grid = "Y") +
  theme(
    panel.border = element_rect(
      colour = get_oxford_colour("Oxford blue"), fill = NA, linewidth = 1
    )
  )


### Violin plot of optical density by observation round ----
vaccine_clean |>
  ggplot(mapping = aes(x = vacc_status, y = od)) +
  geom_violin(
    colour = get_oxford_colour("Oxford blue"),
    fill = get_oxford_colour("Oxford blue"),
    linewidth = 1, alpha = 0.3
  ) +
  geom_jitter(
    width = 0.2, 
    colour = get_oxford_colours("Oxford green"), 
    fill = get_oxford_colour("Oxford green"),
    size = 2
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 0.5)) +
  labs(
    title = "Optical density by vaccination status per observation month",
    x = "Vaccination status", y = "Optical Density"
  ) +
  facet_wrap(. ~ months, ncol = 3) +
  theme_oxford(grid = "Y") +
  theme(
    panel.border = element_rect(
      colour = get_oxford_colour("Oxford blue"), fill = NA, linewidth = 1
    )
  )


## Truncated regression analysis ----

vacc_trunc_model1 <- truncreg(
  od ~ vacc_status + months + sex + bmi + age + inf_prior_vacc, 
  data = vaccine_clean,
  point = 3.5, direction = "right"
)

summary(vacc_trunc_model1)

vacc_trunc_model2 <- truncreg(
  od ~ vacc_status + sex + bmi + age + inf_prior_vacc, 
  data = vaccine_clean,
  point = 3.5, direction = "right"
)

summary(vacc_trunc_model2)

pchisq(
  -2 * (logLik(vacc_trunc_model2) - logLik(vacc_trunc_model1)), 
  df = 2, lower.tail = FALSE
)

