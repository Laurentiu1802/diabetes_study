# install required libraries
install.packages("tidyverse")
install.packages("haven")   # for reading nhanes .xpt files
install.packages("janitor") # for cleaning column names

# load libraries
library(tidyverse)
library(haven)
library(janitor)

# load pima dataset and clean column names
pima <- read_csv("D:/School/Biostatistics/data/diabetes.csv") %>%
  clean_names()

# preview pima data structure
glimpse(pima)

# load nhanes datasets and clean column names
demo <- read_xpt("D:/School/Biostatistics/data/DEMO_J.xpt") %>% clean_names()
diq  <- read_xpt("D:/School/Biostatistics/data/DIQ_J.xpt") %>% clean_names()
glu  <- read_xpt("D:/School/Biostatistics/data/GLU_J.xpt") %>% clean_names()
bmx  <- read_xpt("D:/School/Biostatistics/data/BMX_J.xpt") %>% clean_names()

# merge nhanes datasets by participant id
nhanes <- demo %>%
  left_join(diq,  by = "seqn") %>%
  left_join(glu,  by = "seqn") %>%
  left_join(bmx,  by = "seqn")

# preview merged nhanes dataset
glimpse(nhanes)

# select relevant variables from nhanes and create binary diabetes variable
nhanes_selected <- nhanes %>%
  select(
    seqn,
    age = ridageyr,                # age in years
    gender = riagendr,             # 1 = male, 2 = female
    bmi = bmxbmi,                  # body mass index
    glucose = lbxglu,              # fasting glucose level
    diabetes_status = diq010       # 1 = yes, 2 = no
  ) %>%
  filter(!is.na(glucose), !is.na(bmi), !is.na(age), !is.na(diabetes_status)) %>%
  mutate(
    gender = ifelse(gender == 2, "female", "male"),       # recode gender
    diabetes_binary = ifelse(diabetes_status == 1, 1, 0)  # binary diabetes flag
  )

# select matching variables from pima dataset
pima_selected <- pima %>%
  select(
    age,
    bmi,
    glucose,
    diabetes_binary = outcome
  ) %>%
  mutate(
    gender = "female"  # all pima participants are female
  )

# show summary of cleaned datasets
summary(pima_selected)
summary(nhanes_selected)

# combine both datasets and label them
combined_glucose <- bind_rows(
  pima_selected %>% mutate(dataset = "Pima"),
  nhanes_selected %>% mutate(dataset = "NHANES")
)

# export histogram of glucose by dataset
postscript("glucose_histogram.eps", width=8, height=6, paper="special", horizontal=FALSE)
ggplot(combined_glucose, aes(x = glucose, fill = dataset)) +
  geom_histogram(bins = 30, position = "identity", color = "black", alpha = 1) +
  scale_fill_manual(values = c("Pima" = "steelblue", "NHANES" = "darkorange")) +
  labs(title = "Glucose Distribution", x = "Fasting Glucose", y = "Count") +
  theme_minimal()
dev.off()

# export boxplot of bmi by dataset
postscript("bmi_boxplot.eps", width=8, height=6, paper="special", horizontal=FALSE)
ggplot(combined_glucose, aes(x = dataset, y = bmi, fill = dataset)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Pima" = "skyblue", "NHANES" = "coral")) +
  labs(title = "BMI Comparison", x = "", y = "BMI") +
  theme_minimal()
dev.off()

# combine datasets again for diabetes prevalence plot
combined_diabetes <- bind_rows(
  pima_selected %>% mutate(dataset = "Pima"),
  nhanes_selected %>% mutate(dataset = "NHANES")
)

# export barplot of diabetes prevalence
postscript("diabetes_barplot.eps", width=8, height=6, paper="special", horizontal=FALSE)
ggplot(combined_diabetes, aes(x = dataset, fill = factor(diabetes_binary))) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(
    values = c("0" = "gray60", "1" = "firebrick"),
    name = "Diabetes",
    labels = c("No", "Yes")
  ) +
  labs(title = "Diabetes Prevalence", y = "Proportion") +
  theme_minimal()
dev.off()

# run t-tests and chi-square test, save to output file
sink("D:/School/Biostatistics/output/stat_tests.txt")

# t-tests for glucose, bmi, and age
t.test(glucose ~ dataset, data = combined_glucose)
t.test(bmi ~ dataset, data = combined_glucose)
t.test(age ~ dataset, data = combined_glucose)

# create contingency table and run chi-square test
diabetes_table <- table(combined_diabetes$dataset, combined_diabetes$diabetes_binary)
diabetes_table
chisq.test(diabetes_table)

sink()

# run logistic regression for pima dataset and save output
sink("D:/School/Biostatistics/output/logistic_models.txt")
pima_model <- glm(diabetes_binary ~ age + bmi + glucose, data = pima_selected, family = "binomial")
summary(pima_model)

# run logistic regression for nhanes dataset and save output
nhanes_model <- glm(diabetes_binary ~ age + bmi + glucose, data = nhanes_selected, family = "binomial")
summary(nhanes_model)
sink()

# show current working directory
getwd()
