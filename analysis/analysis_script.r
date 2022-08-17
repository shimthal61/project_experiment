library(tidyverse)
library(visdat)
library(performance)
library(lme4)

data <- read_csv("anonymised_data.csv")

experimental_data <- subset(data, select = c(participant_no, item_type, cond, slider.response, item_no))  %>% 
                     filter(!is.na(item_type), !is.na(cond), item_type == "E")  %>% 
                     transmute(condition = factor(cond),
                               subject = factor(participant_no),
                               slider_response = slider.response,
                               item_no = factor(item_no))

set.seed(42)
dplyr::sample_n(experimental_data, 10)
str(experimental_data)

# Descriptive stats
experimental_data  %>% 
    group_by(condition)  %>% 
    summarise(count = n(), mean = mean(slider_response), sd = sd(slider_response))

# Density Histogram
experimental_data  %>% 
    ggplot(aes(x = slider_response)) +
    geom_histogram(aes(y = ..density..),
                       binwidth = 1,
                       colour = "black", fill = "black",
                       alpha = 0.5) +
    geom_density(alpha = .5, fill = "#FF6666") +
    theme_minimal()

# Visualisation
experimental_data %>% 
    ggplot(aes(x = condition, y = slider_response, colour = condition)) +
    geom_violin(width = 0.3) +
    geom_point(alpha = 0.05, position = position_jitter(width = 0.05, seed = 42)) +
    stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 0.5) +
    theme_minimal() +
    guides(colour = 'none') +
    labs(x = "Condition",
         y = "Response")

# From the plot, it looks as participants rated the effect size as 'larger' in the truncated graphs version

# Assessing contrasts

contrasts(experimental_data$condition)

# Analysis

mixed_model <- lmer(slider_response ~ condition + (1 | subject) + (1 | item_no), data = experimental_data)

summary(mixed_model)

# Let's use a LRT to determine whether a model with our fixed effect is better than one withot

mixed_model_null <- lmer(slider_response ~ (1 | subject) + (1 | item_no), data = experimental_data)

anova(mixed_model, mixed_model_null)

# Now, let's build a model which models the slopes of our random effects
mixed_model_slopes <- lmer(slider_response ~ condition + (1 + condition | subject) + (1 + condition | item_no),
                            data = experimental_data)

summary(mixed_model_slopes)

check_model(mixed_model_slopes)

# Item_type = exp/f/ac
# q1 - q5 slider - 5 literacy questions (potential covariate)
# "thisxN" - counting the order they appear in
# "correct response" - for AC items
# "edu_slider.response" - translate
# "ageResp.text" = their age
# "textbox.text" = their comments
# "genderResp1.resp" = gender
# "cond" - the condition
# "slider.response" - outcome variable
# "participant" - change to 1, 2, 3, 4... 
# total_duration = whole experiment time
# "prop" - proportion of max stated value (0.2 - 0.4; 20% - 40%)
# "session" - someone completed twice ("reference by their session number")