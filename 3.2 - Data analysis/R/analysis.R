library(tidyverse)
library(ggplot2)
library(cowplot)
library(broom, include.only = c("tidy"))
library(emmeans, include.only("emmeans", "contrast"))
import::from(multcomp, glht, mcp, adjusted, cld, contrMat)
import::from(car, sigmaHat)

theme_minimal <- theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 6),
  axis.text = element_text(size = 6),
  plot.title = element_text(size = 8, face = "bold")
)

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

###
# Visualize inputs
plot_inputs <- function(df) {
  
  p_woa <- ggplot(df, aes(x = condition, y = WOA, fill = gender, color = gender)) +
    geom_boxplot(alpha = .4) +
    geom_jitter(
      alpha=0.6,
      position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.7)) +
    labs(
      title = "WOA by condition x gender",
      x="Verbalized Uncertainty level of AI",
      y="WOA",
      fill= "Gender",
      color="Gender"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 10, face = "bold")
    ) +
    theme(
      legend.title.align=0.5
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  
  # Visualize Trust
  p_trust <- ggplot(df, aes(x = condition, y = Trust, fill = gender, color = gender)) +
    geom_boxplot(alpha = .4) +
    geom_jitter(
      alpha = 0.6,
      position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.7)) +
    labs(
      title = "Trust by condition x gender\nmeasured on a 7 point Likert Scale",
      x="Verbalized Uncertainty level of AI",
      y="Trust",
      fill= "Gender",
      color="Gender"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 10, face = "bold")
    ) +
    theme(legend.position = "none")
    
  # combine
  shared_legend = get_legend(p_woa)
  p_woa <- p_woa + theme(legend.position = "none")
  p_trust <- p_trust + theme(legend.position = "none")
  # Combine plots and legend
  plot_row <- plot_grid(p_trust, p_woa, nrow = 1, align = "hv")
  p_combined <- plot_grid(plot_row, shared_legend, ncol = 1, rel_heights = c(1, 0.2))
  
  return(list("p_woa" = p_woa, "p_trust" = p_trust, "p_combined" = p_combined))
}

plots_input <- plot_inputs(df)

ggsave("plot_input_woa.png", plots_input$p_woa, width = 4, height = 4, units = "in")
ggsave("plot_input_trust.png", plots_input$p_trust, width = 4, height = 4, units = "in")
ggsave("plot_input_woa_trust.png", plots_input$p_combined, width = 7, height = 4, units = "in")



###
# What are the hypotheses?
# H1: The trust measured will be highest in the baseline condition, regardless of gender
# H2: In the low condition, male participants will put higher trust in the AI



# Plot the means of condition and gender
# plot connected means for both Trust and WOA
plot_connected_means <- function(df) {
  df_long <- df |> 
    select(condition, gender, Trust, WOA) |> 
    pivot_longer(cols = c(Trust, WOA), names_to = "Measure", values_to = "Value")
  
  p <- df_long |> 
    ggplot(aes(x = condition, y = Value, group = gender, color = gender)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    facet_grid(rows = vars(Measure), scales = "free_y") +
    labs(
      title = "Trust and WOA - means by condition × gender",
      x = "Verbalized Uncertainty Level of AI",
      y = NULL,
      color = "Gender"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

ggsave("plot_means.png", plot_connected_means(df), width = 5, height = 5, units = "in")


# Plot the means of condition, summarise gender
plot_connected_means_summarised <- function(data) {
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
plot_connected_means_summarised(df)
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
contrast_h1_woa <- lm_h1_woa |>
  emmeans(~ condition) |> 
  contrast(
    method=contrast_method,
    adjust = "holm"
  ) |> 
  tidy(conf.int=T) |> 
  mutate(d = estimate / sigmaHat(lm_h1_woa))

# Trust -> We can confirm our H1
contrast_h1_trust <- lm_h1_trust |>
  emmeans(~ condition) |> 
  contrast(
    method=contrast_method,
    adjust = "holm"
  ) |> 
  tidy(conf.int=T) |> 
  mutate(d = estimate / sigmaHat(lm_h1_woa))

# Plot
plot_h1 <- function(contrast_woa, contrast_trust) {
  
  p_woa <- contrast_woa |> 
    ggplot(aes(x=contrast, y=estimate, ymin=conf.low, ymax=conf.high)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange() +
    expand_limits(y = 0) +
    coord_flip() +
    labs(
      x = "Contrast",
      y = "Estimate difference WOA",
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p_trust <- contrast_trust |> 
    ggplot(aes(x=contrast, y=estimate, ymin=conf.low, ymax=conf.high)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange() +
    expand_limits(y = 0) +
    coord_flip() +
    labs(
      x = "Contrast",
      y = "Estimate difference Trust",
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  p_combined <- plot_grid(p_woa, p_trust, nrow=1)
  return(p_combined)
}
plot_h1(contrast_h1_woa, contrast_h1_trust)

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
  
  p_out <- tmp_data |> 
    ggplot(aes(x=condition, y=estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_pointrange() +
    
    labs(
      title = sprintf("Difference in %s male-female in different conditions", label),
      x="Verbalized Uncertainty level of AI",
      y="Estimaton Difference in WOA",
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 13, face = "bold")
    ) +
    coord_flip()
  
  return(p_out)
}
p <- analyze_h2_contrast(lm_h2_woa, "WOA")
ggsave("plot_h2_woa.png", p, width = 7, height = 4, units = "in")
p <- analyze_h2_contrast(lm_h2_trust, "Trust")
ggsave("plot_h2_trust.png", p, width = 7, height = 4, units = "in")
