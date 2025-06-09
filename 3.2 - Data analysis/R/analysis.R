library(tidyverse)
library(cowplot, include.only = c("plot_grid"))
library(broom, include.only = c("tidy"))
library(emmeans, include.only("emmeans", "contrast"))
import::from(car, sigmaHat)



# Load data scenario 1
df = read_csv("input/scenario1.csv")

# Adjust types
df <- df |> mutate(
  condition = factor(condition, levels=c("low","baseline","high")),
  gender = factor(gender, levels=c("male","female")),
  age_cat = factor(age_cat, levels=c("18-34","35-49","50-65")),
  education = factor(education, levels=c("none","highschool","college")),
)

# Check distributions of values
summary(df)
df |> group_by(condition, gender, age_cat, education) |> count() |> View()

# Visualize Trust
ggplot(df, aes(x = condition, y = Trust, fill = gender, color = gender)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.9)) +
  labs(title = "Trust by condition x gender", x = "Condition", y = "Trust")

# Visualize WOA
ggplot(df, aes(x = condition, y = WOA, fill = gender, color = gender)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.9)) +
  labs(title = "WOA by condition x gender", x = "Condition", y = "WOA")

# Visualize how WOA and Trust is different for different participants
df |> 
  filter(condition == "baseline") |> 
  ggplot(aes(x=trial, y=WOA, color=pid)) +
  facet_grid(age_cat ~ education) +
  geom_point() +
  geom_line() +
  ggtitle("WOA baseline") +
  theme(legend.position="none")

df |> 
  filter(condition == "baseline") |> 
  ggplot(aes(x=trial, y=Trust, color=pid)) +
  facet_grid(age_cat ~ education) +
  geom_point() +
  geom_line() +
  ggtitle("Trust baseline") +
  theme(legend.position="none")

###
# What are the hypotheses?
# H1: The trust measured will be highest in the baseline condition, regardless of gender
# H2: In the low condition, male participants will put higher trust in the AI



# Plot the means of condition and gender
# plot connected means for both Trust and WOA
plot_connected_means <- function(data) {
  p_trust <- df |> 
    ggplot(aes(x = condition, y = Trust, group = gender, color = gender)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_woa <- df |> 
    ggplot(aes(x = condition, y = WOA, group = gender, color = gender)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_combined = plot_grid(p_trust, p_woa, nrow=2)
  print(p_combined)
}
plot_connected_means(df)

# Plot the means of condition, summarise gender
plot_connected_means <- function(data) {
  p_trust <- df |> 
    ggplot(aes(x = condition, y = Trust, group = 1)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_woa <- df |> 
    ggplot(aes(x = condition, y = WOA, group = 1)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_combined = plot_grid(p_trust, p_woa, nrow=2)
  print(p_combined)
}
plot_connected_means(df)
# We can see the increased means in the baseline condition



###
# H1: The trust measured will be highest in the baseline condition, regardless of gender
# Since we are specifically not intersted in interaction effects, we will fit a simple model without interaction
# To control for gender effects, we will have it as a simple main effect though

# Fit a linear models that incorporates condition but no interaction effects
lm_h1_woa <- lm(WOA ~ condition + gender, data = df)
lm_h1_trust <- lm(Trust ~ condition + gender, data = df)

# -> genderfemale does have a significant effect, but not of interest per se, also rather small
summary(lm_h1_woa)
summary(lm_h1_trust)

# -> Model fit is appropriate
anova(lm_h1_woa)
anova(lm_h1_trust)

# Inspect the conf intervals
tidy(lm_h1_woa, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high)
tidy(lm_h1_trust, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high)

# We are interested in the planned contrast of baseline vs any other without gender
contrast_method <- list("baseline - (low + high)/2" = c(-0.5,1,-0.5))

# WOA -> We can confirm our H1
lm_h1_woa |>
  emmeans(~ condition) |> 
  contrast(
    method=contrast_method,
    adjust = "holm"
  ) |> 
  tidy(conf.int=T) |> 
  mutate(d = estimate / sigmaHat(lm_h1_woa))

# Trust -> We can confirm our H1
lm_h1_trust |>
  emmeans(~ condition) |> 
  contrast(
    method=contrast_method,
    adjust = "holm"
  ) |> 
  tidy(conf.int=T) |> 
  mutate(d = estimate / sigmaHat(lm_h1_woa))

# Both Trust and WOA show their highest levels in the baseline condition

# What about posthoc comparisons?
analyze_glht_tukey <- function(model, label) {
  df_glht <- model |> 
    glht(mcp(condition="Tukey")) |> 
    tidy(conf.int=T)
  
  print(df_glht)
  
  df_glht |>
    ggplot(aes(x = contrast, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange() +
    expand_limits(y = 0) +
    coord_flip() +
    ylab(paste("estimate difference", label, sep = " "))
}
analyze_glht_tukey(lm_h1_woa, "WOA")
analyze_glht_tukey(lm_h1_trust, "Trust")





###
# H2: In the low condition, male participants will put higher trust in the AI
# Now we are interested in the interaction effects, so we fit new models

lm_h2_woa <- lm(WOA ~ condition * gender, data = df)
lm_h2_trust <- lm(Trust ~ condition * gender, data = df)

# -> The interaction effects are strongly significant, but low
summary(lm_h2_woa)
summary(lm_h2_trust)

# -> Model fit is appropriate
anova(lm_h2_woa)
anova(lm_h2_trust)

# Inspect the conf intervals
tidy(lm_h2_woa, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high)
tidy(lm_h2_woa, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high) |> 
  filter(term != "(Intercept)") |> 
  ggplot(aes(x=term, y=estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange() +
  coord_flip()
  
tidy(lm_h2_trust, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high)
tidy(lm_h2_trust, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high) |>
  filter(term != "(Intercept)") |> 
  ggplot(aes(x=term, y=estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange() +
  coord_flip()

# We are interested in the planned contrast of the effect of gender in the low condition
h2_woa_emmeans <- emmeans(lm_h2_woa, ~ gender | condition)
h2_trust_emmeans <- emmeans(lm_h2_trust, ~ gender | condition)

analyze_h2_contrast <- function(model, label) {
  tmp_emmean <- emmeans(model, ~ gender | condition)
  
  tmp_data <- contrast(
    tmp_emmean, 
    method = "pairwise", 
    by = NULL, 
    simple = "each", 
    combine = T) |> 
    summary(infer = T, adjust = "holm") |> 
    filter(condition != ".") |>
    select(condition, contrast, estimate, lower.CL, upper.CL) |> 
    mutate(d = estimate / sigmaHat(model))
  
  print(tmp_data)
  
  tmp_data |> 
    ggplot(aes(x=condition, y=estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange() +
    ggtitle("Difference male-female in different conditions") +
    labs(y=paste("Estimate Difference ", label, sep = " ")) +
    coord_flip()
}
analyze_h2_contrast(lm_h2_woa, "WOA")
analyze_h2_contrast(lm_h2_trust, "Trust")
