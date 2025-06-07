# ------------------------------------------------------------
library(tidyverse)
library(faux)
library(ggplot2)
library(cowplot) # To stack plots
set.seed(10)

## -------------------- constants ----------------------------------------------
conds     <- c("low", "baseline", "high")
genders   <- c("female", "male")
age_bins  <- c("18-34", "35-49", "50-65")
edu_lvls  <- c("none", "highschool", "college")
trials    <- 1:10

# scenario 1 specific variables
mu_WOA_base <- c(low = .50, baseline = 0.70, high = 0.30)
mu_Trust_base <- c(low = 4.4,  baseline = 4.7, high = 4.2)

# Original
# boost_low_male_WOA   <-  +0.05
# boost_low_male_Trust <-  +0.40

# To see differences more clearly in inspect plots
boost_low_male_WOA   <-  +0.1
boost_low_male_Trust <-  +0.6

## -------------------- helper -------------------------------------------------
simulate_scenario <- function(mu_WOA_vec    = mu_WOA_base,
                              mu_Trust_vec  = mu_Trust_base,
                              beta_LM_WOA   = boost_low_male_WOA,
                              beta_LM_Trust = boost_low_male_Trust,
                              n_per_cell    = 25, # 150/(2*3)
                              sd_WOA        = 0.18) {
  
  # produces low_female, low_male, etc. 
  mu_WOA_full <- setNames(
    rep(mu_WOA_vec, each = 2),
    paste(rep(conds, each = 2), genders, sep = "_")  
  )
  
  # add boost for low_male specifically
  mu_WOA_full["low_male"] <- mu_WOA_full["low_male"] + beta_LM_WOA
  
  # core design: 150 participants × 10 trials
  df <- sim_design(
    between = list(condition = conds,
                   gender    = genders),
    within  = list(trial = trials),
    n   = n_per_cell,
    mu  = mu_WOA_full,
    sd  = sd_WOA,
    dv  = "WOA",
    id  = "pid",
    long = TRUE
  )
  
  # attach covariates & random effects per participant
  df <- df |>
    group_by(pid) |>
    mutate(
      age_cat   = sample(age_bins, 1),
      education = sample(edu_lvls, 1),
      u0_trust  = rnorm(1, 0, 0.30),       # random intercept Trust
      beta_tr   = rnorm(1, -0.01, 0.03)    # random slope Trial
    ) |>
    ungroup()
  
  # compute Trust outcome
  df |>
    mutate(
      trial_c = as.numeric(trial) - mean(trials), # centered  –4.5…+4.5
      mu_tr   = mu_Trust_vec[condition] +
        if_else(condition == "low" & gender == "male",
                beta_LM_Trust, 0),
      Trust   = rnorm(n(), mu_tr + u0_trust + beta_tr * trial_c, 1) |>
        round() |> pmin(7) |> pmax(1)
    ) |>
    select(pid, condition, gender, age_cat, education,
           trial, WOA, Trust) |>
    arrange(pid)
  
}

inspect_design <- function(df, scenario_text = "") {
  df |>
    group_by(condition) |>
    summarise(mean_WOA   = mean(WOA),
              mean_Trust = mean(Trust),
              .groups = "drop") |>
    print()
  
  df |>
    filter(condition == "low") |>
    group_by(gender) |>
    summarise(mean_Trust = mean(Trust)) |>
    print()
  
  helper_create_plot <- function(df, iv, plot_title) {
    ggplot(df, aes(x = condition, y = !!sym(iv), fill = gender, color = gender)) +
      # Version boxplot
      geom_boxplot(alpha = .6) +

      # Version violin und geom_jitter
      # geom_violin(alpha = .6) +
      
      geom_jitter(position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.9)) +
      
      labs(title = paste(plot_title, scenario_text, sep = " - "), x = "Condition", y = iv)
  }
  
  # Create plots using the function
  p1 <- helper_create_plot(df, "WOA", "WOA by condition × gender")
  p2 <- helper_create_plot(df, "Trust", "Trust by condition × gender")
  
  # Plot both p1 and p2 in one, its pretty crowded
  # p_combined = plot_grid(p1, p2, nrow=1, ncol=2)
  # print(p_combined)
  
  # Rather print it one by one
  print(p1)
  print(p2)
}

# Scenario 1 – What we expect
scenario1_df <- simulate_scenario()
inspect_design(scenario1_df, "scenario 1")

# Scenario 2 – only Trust keeps the male boost
scenario2_df <- simulate_scenario(
  mu_WOA_vec       = rep(.55, 3) |> setNames(conds),  # wording effect gone
  beta_LM_WOA      = 0,                               # remove boost in WOA
  # Trust remains as in scenario 1 (baseline highest + male boost in low)
)

inspect_design(scenario2_df, "scenario 2")

# Scenario 3 – full null on both measures
scenario3_df <- simulate_scenario(
  mu_WOA_vec       = rep(.55, 3) |> setNames(conds),
  mu_Trust_vec     = rep(4.5, 3) |> setNames(conds),
  beta_LM_WOA      = 0,
  beta_LM_Trust    = 0            # remove male boost in Trust as well
)
inspect_design(scenario3_df, "scenario 3")

# Save the scenarios
write.csv(scenario1_df, "output/scenario1.csv")
write.csv(scenario2_df, "output/scenario2.csv")
write.csv(scenario3_df, "output/scenario3.csv")

# Save the chosen scenario 1 to the 3.2 folder
write.csv(scenario1_df, "../3.2 - Data analysis/input/scenario1.csv", row.names=F)
