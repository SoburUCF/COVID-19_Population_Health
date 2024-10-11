# Import required packages

library(tidyverse)
library(readxl)
library(scales)
library(purrr)


# Import data
df <- read_excel("data/CEAL Data Combined_.xlsx", sheet = 4)



#Selected column
dfv1 <- df %>% select(c("Q1","Q8", "Q9", "Q10", "Q20"  ,"Q27", "Q37", "Q38", "Q41", "Q43", "Q87_1", "Q87_2", "Q87_3", "Q87_4", "Q87_5", "Q87_6", "Q87_7", "Q87_8", "Q87_9", "Q87_10",
                        "Q88_1", "Q88_2", "Q88_3", "Q88_4", "Q88_5", "Q88_6", "Q107...225"))

colnames(dfv1) <- c("age","dof", "gender", "gender_sp", "Education" , "insurance" ,"vaccinated", "likely", "trust", "confidence", "Q87_1", "Q87_2", "Q87_3", "Q87_4", "Q87_5", "Q87_6", "Q87_7", "Q87_8", "Q87_9", "Q87_10",
                    "Q88_1", "Q88_2", "Q88_3", "Q88_4", "Q88_5", "Q88_6", "pregnency")

dfv1$age <- as.numeric(dfv1$age)
hist(dfv1$age)
median(dfv1$age, na.rm = TRUE)

# Calculate interquartile range (IQR)
IQR(dfv1$age, na.rm = TRUE)


#Fix gender column
dfv1$gender_sp <- sub(" .*", "", dfv1$gender_sp)
dfv1$gender_sp <- gsub("Otra", "Other", dfv1$gender_sp)
dfv1 <- dfv1[!grepl("Prefer", dfv1$gender_sp), ]

#Rename the gender column
dfv1$gender_rn <- ifelse(dfv1$gender_sp %in% c("Straight", "Heterosexual"), "Straight/Heterosexual", "Gender Minority")

#Fix vaccination column
dfv1$vaccinated <- sub(" .*", "", dfv1$vaccinated)
dfv1$vaccinated <- sub(",", "", dfv1$vaccinated)
dfv1$vaccinated <- gsub("Sí", "Yes", dfv1$vaccinated)
dfv1 <- dfv1[!grepl("Prefer", dfv1$vaccinated), ]

# Remove raws with NAs
dfv1 <- dfv1 %>% filter(!is.na(gender_rn))
dfv1 <- dfv1 %>% filter(!is.na(vaccinated))

#Calculate median age and IQR by gender
dfv1 %>%
  group_by(gender_rn) %>%
  summarize(
    median_age = median(age, na.rm = TRUE),  # Calculate the median age
    IQR_age = IQR(age, na.rm = TRUE)         # Calculate the IQR of age
  )



# Create a table of vaccinated by gender_rn
vaccination_table <- table(dfv1$gender_rn, dfv1$vaccinated)

# Convert the table to a dataframe for easier calculation
vaccination_df <- as.data.frame(vaccination_table)

# Calculate the percentage within each gender group
vaccination_df <- vaccination_df %>%
  group_by(Var1) %>% # Group by gender_rn
  mutate(Percentage = Freq / sum(Freq) * 100)

# Rename columns for clarity
colnames(vaccination_df) <- c("Gender", "Vaccinated", "Count", "Percentage")

# View the result
vaccination_df


####################################################
######### Calculate the vaccine hesitancy ##########
####################################################

# First convert the hesitancy response to all english 
# Translate the spanish responses


# Conversion function
convert_to_english <- function(x) {
  case_when(
    x == "Ni de acuerdo ni desacuerdo" ~ "Neither agree nor disagree",
    x == "De acuerdo" ~ "Agree",
    x == "Totalmente de acuerdo" ~ "Strongly agree",
    x == "en desacuerdo" ~ "Disagree",
    x == "Totalmente en desacuerdo" ~ "Strongly disagree",
    TRUE ~ as.character(x)  # Keeps the original value for all other cases (including NA)
  )
}


dfv1<- dfv1 %>%
  mutate(across(everything(), ~ convert_to_english(.)))


#Convert vaccine hesitancy response to scale for analysis

#1
dfv1 <- dfv1 %>%
  mutate(vhesi1 = case_when(
    Q87_1 == "Strongly agree" ~ 1,
    Q87_1 == "Agree" ~ 2,
    Q87_1 == "Neither agree nor disagree" ~ 3,
    Q87_1 == "Disagree" ~ 4,
    Q87_1 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))
#2
dfv1 <- dfv1 %>%
  mutate(vhesi2 = case_when(
    Q87_2 == "Strongly agree" ~ 1,
    Q87_2 == "Agree" ~ 2,
    Q87_2 == "Neither agree nor disagree" ~ 3,
    Q87_2 == "Disagree" ~ 4,
    Q87_2 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#3
dfv1 <- dfv1 %>%
  mutate(vhesi3 = case_when(
    Q87_3 == "Strongly agree" ~ 1,
    Q87_3 == "Agree" ~ 2,
    Q87_3 == "Neither agree nor disagree" ~ 3,
    Q87_3 == "Disagree" ~ 4,
    Q87_3 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))


#4
dfv1 <- dfv1 %>%
  mutate(vhesi4 = case_when(
    Q87_4 == "Strongly agree" ~ 1,
    Q87_4 == "Agree" ~ 2,
    Q87_4 == "Neither agree nor disagree" ~ 3,
    Q87_4 == "Disagree" ~ 4,
    Q87_4 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#5
dfv1 <- dfv1 %>%
  mutate(vhesi5 = case_when(
    Q87_5 == "Strongly agree" ~ 5,
    Q87_5 == "Agree" ~ 4,
    Q87_5 == "Neither agree nor disagree" ~ 3,
    Q87_5 == "Disagree" ~ 2,
    Q87_5 == "Strongly disagree" ~ 1,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#6
dfv1 <- dfv1 %>%
  mutate(vhesi6 = case_when(
    Q87_6 == "Strongly agree" ~ 1,
    Q87_6 == "Agree" ~ 2,
    Q87_6 == "Neither agree nor disagree" ~ 3,
    Q87_6 == "Disagree" ~ 4,
    Q87_6 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))


#7
dfv1 <- dfv1 %>%
  mutate(vhesi7 = case_when(
    Q87_7 == "Strongly agree" ~ 1,
    Q87_7 == "Agree" ~ 2,
    Q87_7 == "Neither agree nor disagree" ~ 3,
    Q87_7 == "Disagree" ~ 4,
    Q87_7 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#8
dfv1 <- dfv1 %>%
  mutate(vhesi8 = case_when(
    Q87_8 == "Strongly agree" ~ 1,
    Q87_8 == "Agree" ~ 2,
    Q87_8 == "Neither agree nor disagree" ~ 3,
    Q87_8 == "Disagree" ~ 4,
    Q87_8 == "Strongly disagree" ~ 5,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#9
dfv1 <- dfv1 %>%
  mutate(vhesi9 = case_when(
    Q87_9 == "Strongly agree" ~ 5,
    Q87_9 == "Agree" ~ 4,
    Q87_9 == "Neither agree nor disagree" ~ 3,
    Q87_9 == "Disagree" ~ 2,
    Q87_9 == "Strongly disagree" ~ 1,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))

#10
dfv1 <- dfv1 %>%
  mutate(vhesi10 = case_when(
    Q87_10 == "Strongly agree" ~ 5,
    Q87_10 == "Agree" ~ 4,
    Q87_10 == "Neither agree nor disagree" ~ 3,
    Q87_10 == "Disagree" ~ 2,
    Q87_10 == "Strongly disagree" ~ 1,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))


# Calculate the mean of vhesi1 to vhesi10 and store it in mean_vhesi
dfv1$mean_vhesi <- rowMeans(dfv1[, paste0("vhesi", 1:10)], na.rm = TRUE)


# Convert the VHS to Hesitancy above mean 3 
dfv1$hesitancy <- ifelse(dfv1$mean_vhesi > 3, "Yes", "No")

dfv1$hesitancy <- as.factor(dfv1$hesitancy)

table(dfv1$hesitancy)



# Vaccine hesitancy summary based on gender_rn

# Create a table of vaccinated by gender_rn
hesitancy_table <- table(dfv1$gender_rn, dfv1$hesitancy)

# Convert the table to a dataframe for easier calculation
hesitancy_df <- as.data.frame(hesitancy_table)

# Calculate the percentage within each gender group
hesitancy_df <- hesitancy_df %>%
  group_by(Var1) %>% # Group by gender_rn
  mutate(Percentage = Freq / sum(Freq) * 100)

# Rename columns for clarity
colnames(hesitancy_df) <- c("Gender", "Vaccinated", "Count", "Percentage")

# View the result
hesitancy_df




######################################################################
##### Vaccine intake and Hesitancy Chi square test and Plotting ######
######################################################################

# Select the relevant columns
test_data <- dfv1 %>% select(gender_rn, vaccinated, hesitancy)

# Conduct Chi-Square test between gender_rn and vaccinated
chisq.test(table(test_data$gender_rn, test_data$vaccinated))

# Conduct Chi-Square test between gender_rn and hesitancy
chisq.test(table(test_data$gender_rn, test_data$hesitancy))



# Create a bar plot for vaccination status
ggplot(test_data, aes(x = gender_rn, fill = vaccinated), color = "black") +
  geom_bar( color = "black", width = 0.7) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11)
  ) +
  scale_fill_manual(values = c("#E74C3C", "#26A69A")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Gender specification",
    y = "Number of Participants",
    fill = "Vaccination Status"  # Correct way to set the legend title
  )


# Create a bar plot for vaccine hesitancy
ggplot(test_data, aes(x = gender_rn, fill = hesitancy), color = "black") +
  geom_bar(color = "black", width = 0.7) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11)
  ) +
  scale_fill_manual(values = c("#0067A6", "#DE6D00")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Gender specification",
    y = "Number of Participants",
    fill = "Vaccine Hesitancy"  # Correct way to set the legend title
  )


##################################################
########### Confidence on the safety ############
##################################################

minority <- filter(dfv1, gender_rn != "Straight/Heterosexual")
minority_nv <- filter(minority, vaccinated != "Yes")
minority_v <- filter(minority, vaccinated == "Yes")


table(dfv1$gender_rn)

table(minority_v$confidence)

#Create a matrix for the contingency table

contingency_table_m <- matrix(c(34, 4, 3, 11), nrow = 2, byrow = TRUE,
                              dimnames = list("Group" = c("Yes", "No"),
                                              "Confidence" = c(" High Confidence (n=37)", "Low Confidence (n=15)")))
#Change the direction of the table
contingency_table_t <- t(contingency_table_m)


# Perform the Fisher test
fisher.test(contingency_table_t)

# Convert the matrix to a data frame
df_con <- as.data.frame(as.table(contingency_table_t))

# Plot the data with ggplot2

ggplot(df_con, aes(x = Confidence, y = Freq, fill = Group)) +
  geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +
  labs(x = "Confidence on vaccine safety", y = "Percentage of participants", fill = "Vaccination Status",
       title = "Vaccination Status by Confidence Level") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +  # Remove space on y-axis
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11)
  )


#### Vaccine hesitancy 
minority <- filter(dfv1, gender_rn != "Straight/Heterosexual")
minority_nh <- filter(minority, hesitancy != "Yes")
minority_vh <- filter(minority, hesitancy == "Yes")


table(minority$hesitancy)

table(minority_nh$confidence)
table(minority_vh$confidence)

#Create a matrix for the contingency table

contingency_table_h <- matrix(c(2, 10, 35, 5), nrow = 2, byrow = TRUE,
                              dimnames = list("Group" = c("Yes", "No"),
                                              "Confidence" = c(" High Confidence (n=37)", "Low Confidence (n=15)")))

contingency_table_th <- t(contingency_table_h)


# Perform the Fisher test
fisher.test(contingency_table_th)

# Convert the matrix to a data frame
df_con_h <- as.data.frame(as.table(contingency_table_th))

# Plot the data with ggplot2
ggplot(df_con_h, aes(x = Confidence, y = Freq, fill = Group)) +
  geom_bar(stat = "identity", position = "fill", color = "black", width = 0.7) +
  labs(x = "Confidence on vaccine safety", y = "Percentage of participant", fill = "Vaccine Hesitancy",
       title = "Vaccine Hesitancy by Confidence Level") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +  # Remove space on y-axis
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11)
  ) 



