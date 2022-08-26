library(tidyverse)
library(buildmer)
library(visdat)
library(performance)
library(lme4)
library(lmerTest)
library(buildmer)

data <- read_csv("anonymised_data.csv")

lit_score <- subset(data, select = c(participant_no, q1_slider.response, q2_slider.response, q3_slider.response, q4_slider.response, q5_slider.response))

lit_score <- lit_score  %>% 
            filter(!is.na(q1_slider.response),
                   !is.na(q2_slider.response),
                   !is.na(q3_slider.response),
                   !is.na(q4_slider.response),
                   !is.na(q5_slider.response))  %>% 
            add_column(literacy_score = NA)

avg <- function(q1, q2, q3, q4, q5) {
    sum((q1 + q2 + q3 + q4 + q5) / 5)
}

for (row in 1:nrow(lit_score)) {
    q1 <- lit_score[row, "q1_slider.response"]
    q2 <- lit_score[row, "q2_slider.response"]
    q3 <- lit_score[row, "q3_slider.response"]
    q4 <- lit_score[row, "q4_slider.response"]
    q5 <- lit_score[row, "q5_slider.response"]
    literacy_score <- avg(q1, q2, q3, q4, q5)

    lit_score[row, "literacy_score"] <- literacy_score
}

lit_score <- subset(lit_score, select = c(participant_no, literacy_score))

data <- full_join(data, lit_score, by = "participant_no")

experimental_data <- subset(data, select = c(participant_no, item_type, cond, slider.response, item_no, literacy_score))  %>% 
                     filter(!is.na(item_type), !is.na(cond), !is.na(literacy_score), item_type == "E")  %>% 
                     transmute(condition = factor(cond),
                            subject = factor(participant_no),
                            slider_response = slider.response,
                            item_no = factor(item_no),
                            literacy_score = literacy_score)

set.seed(42)
dplyr::sample_n(experimental_data, 10)

# Descriptive stats
experimental_data  %>% 
    group_by(condition)  %>% 
    summarise(count = n(), mean = mean(slider_response), sd = sd(slider_response))

# Density Histogram
experimental_data  %>% 
    ggplot(aes(x = slider_response)) +
    geom_histogram(aes(y = ..density..),
                       binwidth = 0.04,
                       colour = "black", fill = "black",
                       alpha = 0.5) +
    geom_density(alpha = .5, fill = "#FF6666") +
    theme_minimal()

# Visualisation
experimental_data %>% 
    ggplot(aes(x = condition, y = slider_response, colour = condition)) +
    geom_violin(width = 0.3) +
    geom_point(alpha = 0.05, position = position_jitter(width = 0.05, seed = 42)) +
    stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 1) +
    theme_minimal() +
    guides(colour = 'none') +
    labs(title = "Effect of bar graph truncation on perceived effect size",
         x = "Condition",
         y = "Response") +
    theme(text = element_text(size = 20)) +
    scale_x_discrete(labels = c("full" = "full", "trunc" = "truncated"))

# From the plot, it looks as participants rated the effect size as 'larger' in the truncated graphs version

# Assessing contrasts

contrasts(experimental_data$condition)

# Analysis

mixed_model <- lmer(slider_response ~ condition + (1 | subject) + (1 | item_no), data = experimental_data)

summary(mixed_model)

# Now, let's build a model which models the slopes of our random effects
mixed_model_slopes <- lmer(slider_response ~ condition + (1 + condition | subject) + (1 + condition | item_no),
                            data = experimental_data)

summary(mixed_model_slopes)

# Let's use a LRT to determine whether a model with our fixed effect is better than one withot

mixed_model_slopes_null <- lmer(slider_response ~ (1 + condition | subject) + (1 + condition | item_no), data = experimental_data)

summary(mixed_model_slopes_null)

anova(mixed_model_slopes, mixed_model_slopes_null)

check_model(mixed_model_slopes)

# Looking at the effect of literacy on the results

literacy_model <- lmer(slider_response ~ condition + literacy_score + (1 + condition | subject) + (1 + condition | item_no),
                            data = experimental_data)

summary(literacy_model)

# Looking at interaction effects

literacy_model_int <- buildmer(slider_response ~ condition * literacy_score + (1 + condition * literacy_score | subject) + (1 + condition * literacy_score | item_no),
                            data = experimental_data)

summary(literacy_model_int)

# DV = Magnitude rating
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

# Git Lab 
