library(tidyverse)

# Read in the raw dataset
raw_data <- read_csv("raw_data.csv")

tidied_data <- raw_data  %>% 
    mutate(item_type = case_when(item_no <= 32 ~ "E", # Remove the UTF-8 encoding from `item_type`
           item_no > 32 & item_no < 41 ~ "F",
           item_no >= 41 ~ "AC")) %>%
    filter(session != "62f529573fa8a531029d43c5") # remove the duplicate session from the dataset


# Create a new tibble with salient attention check features, and create new column called attention pass
attention_check <- subset(tidied_data, select = c(participant, slider.response, correct_response))  %>%  # Pulls 3 columns from tidied_data
                   filter(!is.na(slider.response), !is.na(correct_response))  %>% # Removes all the NA values
                   add_column(attention_pass = NA, age = NA) # Creates new column 

# Run a for loop which works out the difference between the expected answer and the actual answer
for (row in 1:nrow(attention_check)) { # Iterates over every row 
    slider.response <- attention_check[row, "slider.response"] # Pulls their slider.response
    correct_response <- attention_check[row, "correct_response"] # Pulls the correct response
    attention_pass <- sum(slider.response - correct_response) # Works out the difference between their response and correct response


    attention_check[row, "attention_pass"] <- attention_pass # Passes this value into the attention_check df
}

# Updates the df to just contain rows where participants got the answer wrong
attention_check <- attention_check  %>% 
    filter(attention_pass != 0)  %>% 
    count(participant)

# Creates a for loop which prints which participants got 2 or more AC questions wrong
for (row in 1:nrow(attention_check)) {
    number_incorrect <- attention_check[row, "n"]

    if (number_incorrect >= 2) {
        print(paste(attention_check[row, "participant"], "got", number_incorrect, "incorrect"))
    }
}

# Remove the participants who failed the attention check
tidied_data <- tidied_data  %>% 
    filter(participant != "5ac6080e9534ba0001c74b6e",
           participant != "5c9a994ad55db20014aba191",
           participant != "5ebe79b7bd76501da28df8b1",
           participant != "60a6be72ccb15020f04ba501",
           participant != "60d13c261171b69977e81f9b",
           participant != "60e31223411f64673dd099b7",
           participant != "615b2f43cb627d1f51211d11")

# Create a new df, where each row is a distinct participant with a number beside them
unique_data <- tidied_data  %>% 
    distinct(participant)  %>% 
    add_column(participant_no = 1:150)

# Join the datasets together, then remove the participant column
anonymised_data <- full_join(tidied_data, unique_data, by = "participant")  %>% 
                    select(-participant)

# Set labels for gender responses
edu_labels <- set_names(c('No formal qualications',
                          'Secondary education (e.g. GED/GCSE)',
                          'High school diploma/A-levels',
                          'Technical/community college',
                          'Undergraduate degree (BA/BSc/other)',
                          'Graduate degree (MA/MSc/MPhil/other)',
                          'Doctorate degree (PhD/other)',
                          'Don\'t know / not applicable'),
                        seq(8,1,-1))

gender_labels <- set_names(c("Prefer not to say", 
                             "In another way:",
                             "Non-binary", 
                             "Man", 
                             "Woman"),
                           1:5)




sum((5 / 150) * 100) # binary - 3.34%
sum((86 / 150) * 100) # men - 57.34%
sum((59 / 150) * 100) # women - 39.34%



# Calculate the mean age
anonymised_data  %>% 
    filter(!is.na(ageResp.text))  %>% 
    summarise(mean = mean(ageResp.text), sd = sd(ageResp.text))

# Save the dataset to a csv file
write.csv(anonymised_data, "C:\\Users\\harve\\Documents\\RStudio Projects\\project_experiment\\project_experiment\\analysis\\anonymised_data.csv", row.names = FALSE)