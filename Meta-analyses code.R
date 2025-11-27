library(dplyr)
library(meta)

# Upload dataset
dataset <- dataset %>%
  mutate(across(
    c(mean_treat_pre, sd_treat_pre, mean_treat_post, sd_treat_post,
      mean_ctrl_pre, sd_ctrl_pre, mean_ctrl_post, sd_ctrl_post),
    ~ as.numeric(.)
  ))

# Calculate mean differences and combined standard deviations
dataset <- dataset %>%
  mutate(
    mean_treat = mean_treat_post - mean_treat_pre,
    mean_ctrl = mean_ctrl_post - mean_ctrl_pre,
    
    sd_treat_05 = sqrt(sd_treat_pre^2 + sd_treat_post^2 - (2 * 0.5 * sd_treat_pre * sd_treat_post)),
    sd_treat_07 = sqrt(sd_treat_pre^2 + sd_treat_post^2 - (2 * 0.7 * sd_treat_pre * sd_treat_post)),
    
    sd_ctrl_05 = sqrt(sd_ctrl_pre^2 + sd_ctrl_post^2 - (2 * 0.5 * sd_ctrl_pre * sd_ctrl_post)),
    sd_ctrl_07 = sqrt(sd_ctrl_pre^2 + sd_ctrl_post^2 - (2 * 0.7 * sd_ctrl_pre * sd_ctrl_post))
  )

# Perform general meta-analysis (using correlation 0.5)
meta_analysis <- metacont(
  n.e = n_treat, mean.e = mean_treat, sd.e = sd_treat_05,
  n.c = n_ctrl, mean.c = mean_ctrl, sd.c = sd_ctrl_05,
  studlab = `StudyID`,
  data = dataset,
  sm = "MD",
  method.tau = "REML",
  method.random.ci = "HK",
  common = FALSE,
  prediction = TRUE
)

# Summary
summary(meta_analysis)