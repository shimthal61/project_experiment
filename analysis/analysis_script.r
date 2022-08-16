library(tidyverse)

data <- read_csv("anonymised_data.csv")

experimental_data <- subset(data, select = c(participant_no, item_type, cond))  %>% 
filter(!is.na(item_type), !is.na(cond))

experimental_data



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