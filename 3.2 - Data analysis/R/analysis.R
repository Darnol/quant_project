library(tidyverse)
library(cowplot, include.only = c("plot_grid"))

# Load data scenario 1
data_df = read_csv("input/scenario1.csv")

# Adjust types
data_df <- data_df |> mutate(
  condition = factor(condition, levels=c("low","baseline","high")),
  gender = factor(gender, levels=c("male","female")),
  age_cat = factor(age_cat, levels=c("18-34","35-49","50-65")),
  education = factor(education, levels=c("none","highschool","college")),
)

# Check distributions of values
summary(data_df)
data_df |> group_by(condition, gender, age_cat, education) |> count() |> View()

# Visualize Trust
ggplot(data_df, aes(x = condition, y = Trust, fill = gender, color = gender)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.9)) +
  labs(title = "Trust by condition x gender", x = "Condition", y = "Trust")

# Visualize WOA
ggplot(data_df, aes(x = condition, y = WOA, fill = gender, color = gender)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(position = position_jitterdodge(jitter.width=0.4, jitter.height=0.2, dodge.width = 0.9)) +
  labs(title = "WOA by condition x gender", x = "Condition", y = "WOA")

# Visualize how WOA and Trust is different for different participants
data_df |> 
  filter(condition == "baseline") |> 
  ggplot(aes(x=trial, y=WOA, color=pid)) +
  facet_grid(age_cat ~ education) +
  geom_point() +
  geom_line() +
  ggtitle("WOA baseline") +
  theme(legend.position="none")

data_df |> 
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
  p_trust <- data_df |> 
    ggplot(aes(x = condition, y = Trust, group = gender, color = gender)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_woa <- data_df |> 
    ggplot(aes(x = condition, y = WOA, group = gender, color = gender)) +
    geom_point(stat = "summary", fun = mean, size = 4) +
    geom_line(stat = "summary", fun = mean) +
    theme(legend.position = "bottom")
  p_combined = plot_grid(p_trust, p_woa, nrow=2)
  print(p_combined)
}
plot_connected_means(data_df)

