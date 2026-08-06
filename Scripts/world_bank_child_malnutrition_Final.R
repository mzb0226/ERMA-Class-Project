############################################################
# WORLD BANK CHILD MALNUTRITION PROJECT
# Outcomes:
#   1. Stunting
#   2. Severe wasting
#   3. Underweight
#
# Main predictor:
#   Current health expenditure (% of GDP)
#
# Controls:
#   GNI per capita, female secondary enrollment,
#   urban population, basic sanitation, adolescent fertility,
#   and year.
#
# Comparisons:
#   - Income-group ANOVA and Tukey tests
#   - Developed vs. developing t-tests
#   - Regional ANOVA and Tukey tests
#   - MANOVA for the three outcomes together
#   - Health expenditure × development-status interactions
############################################################


############################################################
# 0. INSTALL PACKAGES ONCE, THEN LOAD THEM
############################################################

required_packages <- c(
  "tidyverse",
  "WDI",
  "broom",
  "stargazer"
)

packages_to_install <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

options(scipen = 999)


############################################################
# 1. READ THE DOWNLOADED WORLD BANK CSV
############################################################

# Select the downloaded World Bank CSV when the file window opens.
raw_data <- read.csv(
  file.choose(),
  check.names = FALSE,
  fileEncoding = "Windows-1252",
  stringsAsFactors = FALSE,
  na.strings = c("..", "")
)

# Confirm the original column names.
names(raw_data)


############################################################
# 2. RENAME THE IDENTIFICATION COLUMNS
############################################################

required_columns <- c(
  "Series Name",
  "Series Code",
  "Country Name",
  "Country Code"
)

missing_columns <- setdiff(
  required_columns,
  names(raw_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "These required columns were not found:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

raw_data <- raw_data %>%
  rename(
    Series.Name = `Series Name`,
    Series.Code = `Series Code`,
    Country.Name = `Country Name`,
    Country.Code = `Country Code`
  )


############################################################
# 3. DEFINE THE INDICATORS AND SHORT VARIABLE NAMES
############################################################

indicator_lookup <- tribble(
  ~Series.Code,          ~Variable,
  "SH.STA.STNT.ZS",      "Stunting",
  "SH.XPD.CHEX.GD.ZS",   "Health_Expenditure",
  "NY.GNP.PCAP.CD",      "GNI_per_capita",
  "SE.SEC.ENRR.FE",      "Female_Secondary_Enrollment",
  "SP.URB.TOTL.IN.ZS",   "Urban_Population",
  "SH.STA.BASS.ZS",      "Basic_Sanitation",
  "SP.ADO.TFRT",         "Adolescent_Fertility",
  "SH.SVR.WAST.ZS",      "Severe_Wasting",
  "SH.STA.MALN.ZS",      "Underweight"
)

# Check which selected indicator codes are present.
indicator_check <- indicator_lookup %>%
  mutate(
    Found = Series.Code %in% unique(raw_data$Series.Code)
  )

print(indicator_check)

if (any(!indicator_check$Found)) {
  warning(
    "One or more selected indicators were not found in the downloaded file."
  )
}


############################################################
# 4. RESHAPE THE WORLD BANK DATA INTO COUNTRY-YEAR FORMAT
############################################################

panel_new <- raw_data %>%

  # Keep only the nine selected indicators.
  filter(
    Series.Code %in% indicator_lookup$Series.Code
  ) %>%

  # Remove metadata rows and retain rows with valid 3-letter codes.
  filter(
    stringr::str_detect(
      Country.Code,
      "^[A-Z]{3}$"
    )
  ) %>%

  # The World Bank file may read some year columns as character
  # and others as numeric. Convert the selected study-year columns
  # to character before combining them into one Value column.
  mutate(
    across(
      matches("^20(16|17|18|19|20|21|22) \\[YR20"),
      as.character
    )
  ) %>%

  # Turn only the 2016-2022 columns into rows.
  pivot_longer(
    cols = matches("^20(16|17|18|19|20|21|22) \\[YR20"),
    names_to = "Year",
    values_to = "Value"
  ) %>%

  mutate(
    Year = as.integer(
      stringr::str_extract(
        Year,
        "20[0-9]{2}"
      )
    ),
    Value = trimws(
      as.character(Value)
    ),
    Value = na_if(
      Value,
      ".."
    ),
    Value = suppressWarnings(
      as.numeric(Value)
    )
  ) %>%

  # Match the period used in the earlier paper.
  filter(
    Year >= 2016,
    Year <= 2022
  ) %>%

  left_join(
    indicator_lookup,
    by = "Series.Code"
  ) %>%

  select(
    Country.Name,
    Country.Code,
    Year,
    Variable,
    Value
  ) %>%

  distinct() %>%

  pivot_wider(
    names_from = Variable,
    values_from = Value
  ) %>%

  arrange(
    Country.Name,
    Year
  )

# Check the reshaped data.
dim(panel_new)
names(panel_new)
head(panel_new, 20)
View(panel_new)


############################################################
# 5. ADD WORLD BANK REGION AND INCOME CLASSIFICATIONS
############################################################

country_info <- WDI_data$country %>%

  filter(
    !is.na(iso3c),
    region != "Aggregates"
  ) %>%

  transmute(
    Country.Code = iso3c,
    Region = region,
    Income_Group = income
  )

analysis_df <- panel_new %>%

  # Inner join removes World Bank regional/income aggregates.
  inner_join(
    country_info,
    by = "Country.Code"
  ) %>%

  mutate(
    Income_Group = factor(
      Income_Group,
      levels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    ),

    # This reproduces the classification used in the earlier project.
    Development_Status = case_when(
      Income_Group == "High income" ~ "Developed",

      Income_Group %in% c(
        "Low income",
        "Lower middle income",
        "Upper middle income"
      ) ~ "Developing",

      TRUE ~ NA_character_
    ),

    Development_Status = factor(
      Development_Status,
      levels = c(
        "Developed",
        "Developing"
      )
    ),

    Log_GNI_per_capita = if_else(
      !is.na(GNI_per_capita) &
        GNI_per_capita > 0,
      log(GNI_per_capita),
      NA_real_
    )
  )

# Check the final panel.
dim(analysis_df)
names(analysis_df)
head(analysis_df)
table(
  analysis_df$Income_Group,
  useNA = "ifany"
)
table(
  analysis_df$Development_Status,
  useNA = "ifany"
)
table(
  analysis_df$Region,
  useNA = "ifany"
)


############################################################
# 6. SAVE THE CLEANED PANEL DATA
############################################################

write.csv(
  analysis_df,
  "cleaned_child_malnutrition_panel_2016_2022.csv",
  row.names = FALSE
)


############################################################
# 7. CHECK MISSING AND AVAILABLE VALUES
############################################################

variables_for_summary <- c(
  "Stunting",
  "Severe_Wasting",
  "Underweight",
  "Health_Expenditure",
  "GNI_per_capita",
  "Female_Secondary_Enrollment",
  "Urban_Population",
  "Basic_Sanitation",
  "Adolescent_Fertility"
)

missing_summary <- analysis_df %>%

  summarise(
    across(
      all_of(variables_for_summary),
      list(
        Available = ~ sum(!is.na(.)),
        Missing = ~ sum(is.na(.))
      )
    )
  ) %>%

  pivot_longer(
    cols = everything(),
    names_to = c(
      "Variable",
      ".value"
    ),
    names_pattern = "(.+)_(Available|Missing)$"
  )

print(missing_summary)

write.csv(
  missing_summary,
  "missing_data_summary.csv",
  row.names = FALSE
)


############################################################
# 8. DESCRIPTIVE STATISTICS FOR ALL VARIABLES
############################################################

descriptive_statistics <- analysis_df %>%

  select(
    all_of(variables_for_summary)
  ) %>%

  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%

  group_by(
    Variable
  ) %>%

  summarise(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Minimum = min(Value, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

print(descriptive_statistics)

write.csv(
  descriptive_statistics,
  "descriptive_statistics.csv",
  row.names = FALSE
)


############################################################
# 9. CORRELATIONS AMONG THE THREE OUTCOMES AND PREDICTORS
############################################################

correlation_variables <- analysis_df %>%
  select(
    Stunting,
    Severe_Wasting,
    Underweight,
    Health_Expenditure,
    Log_GNI_per_capita,
    Female_Secondary_Enrollment,
    Urban_Population,
    Basic_Sanitation,
    Adolescent_Fertility
  )

correlation_matrix <- cor(
  correlation_variables,
  use = "pairwise.complete.obs"
)

print(
  round(
    correlation_matrix,
    3
  )
)

write.csv(
  correlation_matrix,
  "correlation_matrix.csv"
)


############################################################
# 10. FUNCTION FOR BASIC AND ADJUSTED REGRESSION MODELS
############################################################

fit_outcome_models <- function(outcome_name) {

  model_variables <- c(
    outcome_name,
    "Health_Expenditure",
    "Log_GNI_per_capita",
    "Basic_Sanitation",
    "Urban_Population",
    "Adolescent_Fertility"
  )

  model_data <- analysis_df %>%
    drop_na(
      all_of(model_variables)
    )

  basic_formula <- as.formula(
    paste(
      outcome_name,
      "~ Health_Expenditure"
    )
  )

  adjusted_formula <- as.formula(
    paste(
      outcome_name,
      paste(
        "~ Health_Expenditure",
        "+ Log_GNI_per_capita",
        "+ Basic_Sanitation",
        "+ Urban_Population",
        "+ Adolescent_Fertility",
        "+ factor(Year)"
      )
    )
  )

  fixed_effects_formula <- as.formula(
    paste(
      outcome_name,
      paste(
        "~ Health_Expenditure",
        "+ Log_GNI_per_capita",
        "+ Basic_Sanitation",
        "+ Urban_Population",
        "+ Adolescent_Fertility",
        "+ factor(Country.Name)",
        "+ factor(Year)"
      )
    )
  )

  list(
    data = model_data,

    # The basic and adjusted models use the same complete-case sample.
    basic = lm(
      basic_formula,
      data = model_data
    ),

    adjusted = lm(
      adjusted_formula,
      data = model_data
    ),

    # Use this as a sensitivity analysis.
    fixed_effects = lm(
      fixed_effects_formula,
      data = model_data
    )
  )
}


############################################################
# 11. FIT MODELS FOR THE THREE MALNUTRITION OUTCOMES
############################################################

stunting_models <- fit_outcome_models(
  "Stunting"
)

wasting_models <- fit_outcome_models(
  "Severe_Wasting"
)

underweight_models <- fit_outcome_models(
  "Underweight"
)

# Assign shorter model names.
model_stunting_basic <- stunting_models$basic
model_stunting_adjusted <- stunting_models$adjusted
model_stunting_fixed_effects <- stunting_models$fixed_effects

model_wasting_basic <- wasting_models$basic
model_wasting_adjusted <- wasting_models$adjusted
model_wasting_fixed_effects <- wasting_models$fixed_effects

model_underweight_basic <- underweight_models$basic
model_underweight_adjusted <- underweight_models$adjusted
model_underweight_fixed_effects <- underweight_models$fixed_effects

# Print the adjusted model results.
summary(model_stunting_adjusted)
summary(model_wasting_adjusted)
summary(model_underweight_adjusted)

# Print sample sizes.
nobs(model_stunting_adjusted)
nobs(model_wasting_adjusted)
nobs(model_underweight_adjusted)

length(
  unique(
    stunting_models$data$Country.Name
  )
)

length(
  unique(
    wasting_models$data$Country.Name
  )
)

length(
  unique(
    underweight_models$data$Country.Name
  )
)


############################################################
# 12. ADD FEMALE SECONDARY ENROLLMENT IN SEPARATE MODELS
############################################################

fit_education_model <- function(outcome_name) {

  education_variables <- c(
    outcome_name,
    "Health_Expenditure",
    "Log_GNI_per_capita",
    "Basic_Sanitation",
    "Urban_Population",
    "Adolescent_Fertility",
    "Female_Secondary_Enrollment"
  )

  education_data <- analysis_df %>%
    drop_na(
      all_of(education_variables)
    )

  education_formula <- as.formula(
    paste(
      outcome_name,
      paste(
        "~ Health_Expenditure",
        "+ Log_GNI_per_capita",
        "+ Basic_Sanitation",
        "+ Urban_Population",
        "+ Adolescent_Fertility",
        "+ Female_Secondary_Enrollment",
        "+ factor(Year)"
      )
    )
  )

  list(
    data = education_data,
    model = lm(
      education_formula,
      data = education_data
    )
  )
}

stunting_education <- fit_education_model(
  "Stunting"
)

wasting_education <- fit_education_model(
  "Severe_Wasting"
)

underweight_education <- fit_education_model(
  "Underweight"
)

summary(
  stunting_education$model
)

summary(
  wasting_education$model
)

summary(
  underweight_education$model
)


############################################################
# 13. EXPORT REGRESSION TABLES
############################################################

############################################################
# COUNTRY AND YEAR FIXED-EFFECTS TABLE
############################################################

fixed_effects_models <- list(
  "Stunting" = model_stunting_fixed_effects,
  "Severe Wasting" = model_wasting_fixed_effects,
  "Underweight" = model_underweight_fixed_effects
)

fixed_effects_coef_map <- c(
  "(Intercept)" =
    "Intercept",
  
  "Health_Expenditure" =
    "Current health expenditure (% of GDP)",
  
  "Log_GNI_per_capita" =
    "Log GNI per capita",
  
  "Basic_Sanitation" =
    "Basic sanitation services (%)",
  
  "Urban_Population" =
    "Urban population (%)",
  
  "Adolescent_Fertility" =
    "Adolescent fertility rate"
)

modelsummary::modelsummary(
  fixed_effects_models,
  
  # Only these substantive coefficients will appear.
  # Country and year dummy coefficients will be hidden.
  coef_map = fixed_effects_coef_map,
  
  statistic = "({std.error})",
  
  stars = c(
    "*" = 0.05,
    "**" = 0.01,
    "***" = 0.001
  ),
  
  fmt = 3,
  
  output =
    "country_year_fixed_effects_models.html"
)

############################################################
# 14. CREATE ONE AVERAGE RECORD PER COUNTRY
#     FOR ANOVA AND T-TEST COMPARISONS
############################################################

mean_if_available <- function(x) {

  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(
    x,
    na.rm = TRUE
  )
}

country_comparison <- analysis_df %>%

  group_by(
    Country.Name,
    Country.Code,
    Income_Group,
    Development_Status,
    Region
  ) %>%

  summarise(
    Stunting = mean_if_available(
      Stunting
    ),

    Severe_Wasting = mean_if_available(
      Severe_Wasting
    ),

    Underweight = mean_if_available(
      Underweight
    ),

    Health_Expenditure = mean_if_available(
      Health_Expenditure
    ),

    .groups = "drop"
  )

dim(country_comparison)
head(country_comparison)

write.csv(
  country_comparison,
  "country_average_comparison_data.csv",
  row.names = FALSE
)


############################################################
# 15. DESCRIPTIVE COMPARISON ACROSS INCOME GROUPS
############################################################

income_summary <- country_comparison %>%

  pivot_longer(
    cols = c(
      Stunting,
      Severe_Wasting,
      Underweight
    ),
    names_to = "Outcome",
    values_to = "Prevalence"
  ) %>%

  group_by(
    Income_Group,
    Outcome
  ) %>%

  summarise(
    N = sum(!is.na(Prevalence)),
    Mean = mean(Prevalence, na.rm = TRUE),
    SD = sd(Prevalence, na.rm = TRUE),
    Minimum = min(Prevalence, na.rm = TRUE),
    Maximum = max(Prevalence, na.rm = TRUE),
    .groups = "drop"
  )

print(income_summary)

write.csv(
  income_summary,
  "income_group_descriptive_statistics.csv",
  row.names = FALSE
)


############################################################
# 16. ONE-WAY ANOVA BY WORLD BANK INCOME GROUP
############################################################

anova_stunting <- aov(
  Stunting ~ Income_Group,
  data = country_comparison
)

anova_wasting <- aov(
  Severe_Wasting ~ Income_Group,
  data = country_comparison
)

anova_underweight <- aov(
  Underweight ~ Income_Group,
  data = country_comparison
)

summary(anova_stunting)
summary(anova_wasting)
summary(anova_underweight)

# Tukey post-hoc comparisons.
tukey_stunting <- TukeyHSD(
  anova_stunting
)

tukey_wasting <- TukeyHSD(
  anova_wasting
)

tukey_underweight <- TukeyHSD(
  anova_underweight
)

print(tukey_stunting)
print(tukey_wasting)
print(tukey_underweight)

write.csv(
  as.data.frame(
    tukey_stunting$Income_Group
  ),
  "tukey_income_stunting.csv"
)

write.csv(
  as.data.frame(
    tukey_wasting$Income_Group
  ),
  "tukey_income_severe_wasting.csv"
)

write.csv(
  as.data.frame(
    tukey_underweight$Income_Group
  ),
  "tukey_income_underweight.csv"
)


############################################################
# 17. MANOVA FOR ALL THREE OUTCOMES TOGETHER
############################################################

manova_data <- country_comparison %>%
  drop_na(
    Stunting,
    Severe_Wasting,
    Underweight,
    Income_Group
  )

malnutrition_manova <- manova(
  cbind(
    Stunting,
    Severe_Wasting,
    Underweight
  ) ~ Income_Group,
  data = manova_data
)

summary(
  malnutrition_manova,
  test = "Wilks"
)

# Follow-up univariate tests from the MANOVA.
summary.aov(
  malnutrition_manova
)


############################################################
# 18. DEVELOPED VS. DEVELOPING COUNTRY COMPARISONS
############################################################

development_summary <- country_comparison %>%

  pivot_longer(
    cols = c(
      Stunting,
      Severe_Wasting,
      Underweight
    ),
    names_to = "Outcome",
    values_to = "Prevalence"
  ) %>%

  group_by(
    Development_Status,
    Outcome
  ) %>%

  summarise(
    N = sum(!is.na(Prevalence)),
    Mean = mean(Prevalence, na.rm = TRUE),
    SD = sd(Prevalence, na.rm = TRUE),
    .groups = "drop"
  )

print(development_summary)

write.csv(
  development_summary,
  "development_status_descriptive_statistics.csv",
  row.names = FALSE
)

ttest_stunting <- t.test(
  Stunting ~ Development_Status,
  data = country_comparison
)

ttest_wasting <- t.test(
  Severe_Wasting ~ Development_Status,
  data = country_comparison
)

ttest_underweight <- t.test(
  Underweight ~ Development_Status,
  data = country_comparison
)

ttest_stunting
ttest_wasting
ttest_underweight

development_ttest_table <- bind_rows(
  broom::tidy(
    ttest_stunting
  ) %>%
    mutate(
      Outcome = "Stunting"
    ),

  broom::tidy(
    ttest_wasting
  ) %>%
    mutate(
      Outcome = "Severe Wasting"
    ),

  broom::tidy(
    ttest_underweight
  ) %>%
    mutate(
      Outcome = "Underweight"
    )
) %>%

  mutate(
    Adjusted_P_Value_BH = p.adjust(
      p.value,
      method = "BH"
    )
  ) %>%

  select(
    Outcome,
    everything()
  )

print(development_ttest_table)

write.csv(
  development_ttest_table,
  "developed_vs_developing_ttests.csv",
  row.names = FALSE
)


############################################################
# 19. REGIONAL DESCRIPTIVE STATISTICS AND ANOVA
############################################################

region_summary <- country_comparison %>%

  pivot_longer(
    cols = c(
      Stunting,
      Severe_Wasting,
      Underweight
    ),
    names_to = "Outcome",
    values_to = "Prevalence"
  ) %>%

  group_by(
    Region,
    Outcome
  ) %>%

  summarise(
    N = sum(!is.na(Prevalence)),
    Mean = mean(Prevalence, na.rm = TRUE),
    SD = sd(Prevalence, na.rm = TRUE),
    .groups = "drop"
  )

print(region_summary)

write.csv(
  region_summary,
  "regional_descriptive_statistics.csv",
  row.names = FALSE
)

region_anova_stunting <- aov(
  Stunting ~ Region,
  data = country_comparison
)

region_anova_wasting <- aov(
  Severe_Wasting ~ Region,
  data = country_comparison
)

region_anova_underweight <- aov(
  Underweight ~ Region,
  data = country_comparison
)

summary(region_anova_stunting)
summary(region_anova_wasting)
summary(region_anova_underweight)

region_tukey_stunting <- TukeyHSD(
  region_anova_stunting
)

region_tukey_wasting <- TukeyHSD(
  region_anova_wasting
)

region_tukey_underweight <- TukeyHSD(
  region_anova_underweight
)

print(region_tukey_stunting)
print(region_tukey_wasting)
print(region_tukey_underweight)


############################################################
# 20. INTERACTION MODELS:
#     DOES THE HEALTH-EXPENDITURE ASSOCIATION DIFFER
#     BETWEEN DEVELOPED AND DEVELOPING COUNTRIES?
############################################################

fit_interaction_model <- function(outcome_name) {

  interaction_variables <- c(
    outcome_name,
    "Health_Expenditure",
    "Development_Status",
    "Log_GNI_per_capita",
    "Basic_Sanitation",
    "Urban_Population",
    "Adolescent_Fertility"
  )

  interaction_data <- analysis_df %>%
    drop_na(
      all_of(interaction_variables)
    )

  interaction_formula <- as.formula(
    paste(
      outcome_name,
      paste(
        "~ Health_Expenditure * Development_Status",
        "+ Log_GNI_per_capita",
        "+ Basic_Sanitation",
        "+ Urban_Population",
        "+ Adolescent_Fertility",
        "+ factor(Year)"
      )
    )
  )

  list(
    data = interaction_data,
    model = lm(
      interaction_formula,
      data = interaction_data
    )
  )
}

stunting_interaction <- fit_interaction_model(
  "Stunting"
)

wasting_interaction <- fit_interaction_model(
  "Severe_Wasting"
)

underweight_interaction <- fit_interaction_model(
  "Underweight"
)

summary(
  stunting_interaction$model
)

summary(
  wasting_interaction$model
)

summary(
  underweight_interaction$model
)

stargazer(
  stunting_interaction$model,
  wasting_interaction$model,
  underweight_interaction$model,
  type = "html",
  out = "development_status_interaction_models.html",
  column.labels = c(
    "Stunting",
    "Severe Wasting",
    "Underweight"
  ),
  dep.var.labels.include = FALSE,
  omit = "factor\\(Year\\)",
  omit.labels = "Year fixed effects",
  digits = 3
)


############################################################
# 21. FIGURE 1:
#     HEALTH EXPENDITURE AND THE THREE OUTCOMES
########################################################################################################################
# FINAL CLEAR INCOME-GROUP FIGURES
############################################################

library(tidyverse)
library(ggplot2)

income_figure_data <- country_comparison %>%
  filter(!is.na(Income_Group)) %>%
  mutate(
    Income_Group = factor(
      Income_Group,
      levels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      ),
      labels = c(
        "Low income",
        "Lower-middle income",
        "Upper-middle income",
        "High income"
      )
    )
  )

scatter_data <- analysis_df %>%

  pivot_longer(
    cols = c(
      Stunting,
      Severe_Wasting,
      Underweight
    ),
    names_to = "Outcome",
    values_to = "Prevalence"
  ) %>%

  filter(
    !is.na(Health_Expenditure),
    !is.na(Prevalence)
  ) %>%

  mutate(
    Outcome = recode(
      Outcome,
      Stunting = "Stunting",
      Severe_Wasting = "Severe Wasting",
      Underweight = "Underweight"
    )
  )

figure1 <- ggplot(
  scatter_data,
  aes(
    x = Health_Expenditure,
    y = Prevalence
  )
) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(
    ~ Outcome,
    scales = "free_y"
  ) +
  labs(
    x = "Current health expenditure (% of GDP)",
    y = "Prevalence among children under five (%)"
  ) +
  theme_minimal()

print(figure1)

ggsave(
  "Figure_1_health_expenditure_and_malnutrition.png",
  plot = figure1,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 22. FIGURE 2:
#     MALNUTRITION OUTCOMES BY INCOME GROUP
############################################################
############################################################
# FINAL CLEAR INCOME-GROUP FIGURES
############################################################

library(tidyverse)
library(ggplot2)

income_figure_data <- country_comparison %>%
  filter(!is.na(Income_Group)) %>%
  mutate(
    Income_Group = factor(
      Income_Group,
      levels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      ),
      labels = c(
        "Low income",
        "Lower-middle income",
        "Upper-middle income",
        "High income"
      )
    )
  )
make_income_plot <- function(
    data,
    outcome_variable,
    plot_title,
    file_name,
    x_max = NULL
) {
  
  plot_data <- data %>%
    filter(
      !is.na(.data[[outcome_variable]])
    )
  
  p <- ggplot(
    plot_data,
    aes(
      x = .data[[outcome_variable]],
      y = Income_Group
    )
  ) +
    
    geom_boxplot(
      width = 0.50,
      linewidth = 0.7,
      outlier.shape = NA
    ) +
    
    geom_jitter(
      height = 0.09,
      width = 0,
      size = 1.6,
      alpha = 0.50
    ) +
    
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6),
      limits = c(0, x_max),
      expand = expansion(
        mult = c(0.01, 0.06)
      )
    ) +
    
    labs(
      title = plot_title,
      x = "Prevalence in children under five (%)",
      y = NULL
    ) +
    
    theme_classic(
      base_size = 12
    ) +
    
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 14)
      ),
      
      plot.title.position = "plot",
      
      axis.title.x = element_text(
        size = 11,
        face = "bold",
        margin = margin(t = 12)
      ),
      
      axis.text.x = element_text(
        size = 10
      ),
      
      axis.text.y = element_text(
        size = 11
      ),
      
      plot.margin = margin(
        t = 18,
        r = 35,
        b = 22,
        l = 25
      )
    ) +
    
    coord_cartesian(
      clip = "off"
    )
  
  print(p)
  
  ggsave(
    filename = file_name,
    plot = p,
    width = 9.5,
    height = 5.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  return(p)
}

figure2a <- make_income_plot(
  data = income_figure_data,
  outcome_variable = "Severe_Wasting",
  plot_title = "Severe Wasting by Income Group",
  file_name = "Figure_2a_severe_wasting_income_FINAL.png",
  x_max = 7
)

figure2b <- make_income_plot(
  data = income_figure_data,
  outcome_variable = "Stunting",
  plot_title = "Stunting by Income Group",
  file_name = "Figure_2b_stunting_income_FINAL.png",
  x_max = 60
)

figure2c <- make_income_plot(
  data = income_figure_data,
  outcome_variable = "Underweight",
  plot_title = "Underweight by Income Group",
  file_name = "Figure_2c_underweight_income_FINAL.png",
  x_max = 45
)

############################################################
# 25. OPTIONAL COUNTRY COMPARISON:
#     BANGLADESH AND THE UNITED STATES
############################################################

selected_country_data <- analysis_df %>%

  filter(
    Country.Name %in% c(
      "Bangladesh",
      "United States"
    )
  ) %>%

  pivot_longer(
    cols = c(
      Stunting,
      Severe_Wasting,
      Underweight
    ),
    names_to = "Outcome",
    values_to = "Prevalence"
  ) %>%

  filter(
    !is.na(Prevalence)
  ) %>%

  mutate(
    Outcome = recode(
      Outcome,
      Stunting = "Stunting",
      Severe_Wasting = "Severe Wasting",
      Underweight = "Underweight"
    )
  )

figure5 <- ggplot(
  selected_country_data,
  aes(
    x = Year,
    y = Prevalence,
    color = Country.Name,
    group = Country.Name
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point() +
  facet_wrap(
    ~ Outcome,
    scales = "free_y"
  ) +
  labs(
    title = "Child Malnutrition Trends in Bangladesh and the United States",
    x = "Year",
    y = "Prevalence among children under five (%)",
    color = "Country"
  ) +
  theme_minimal()

print(figure5)

ggsave(
  "Figure_5_Bangladesh_United_States_comparison.png",
  plot = figure5,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 26. FINAL CHECKS
############################################################

# Number of panel observations.
nrow(analysis_df)

# Number of unique countries.
length(
  unique(
    analysis_df$Country.Name
  )
)

# Number of unique country-year combinations.
length(
  unique(
    paste(
      analysis_df$Country.Name,
      analysis_df$Year
    )
  )
)

# Available outcome observations.
analysis_df %>%
  summarise(
    Stunting_N = sum(
      !is.na(Stunting)
    ),
    Severe_Wasting_N = sum(
      !is.na(Severe_Wasting)
    ),
    Underweight_N = sum(
      !is.na(Underweight)
    )
  )
############################################################
# SAVE THE FINAL DATASET
############################################################

# Save as CSV, which can be opened in Excel
write.csv(
  analysis_df,
  "final_analysis_df.csv",
  row.names = FALSE
)

# Save as an R data file.
saveRDS(
  analysis_df,
  "final_analysis_df.rds"
)

############################################################
# END OF SCRIPT
############################################################
