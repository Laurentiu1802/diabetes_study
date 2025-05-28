# ---- Load libraries ----
install.packages("tidyverse")
install.packages("haven")   # for NHANES .XPT files
install.packages("janitor") # for cleaning column names

library(tidyverse)
library(haven)
library(janitor)

# ---- Load Pima dataset ----
pima <- read_csv("D:/School/Biostatistics/data/diabetes.csv") %>%
  clean_names()

glimpse(pima)

# ---- Load NHANES data ----
demo <- read_xpt("D:/School/Biostatistics/data/DEMO_J.xpt") %>% clean_names()
diq  <- read_xpt("D:/School/Biostatistics/data/DIQ_J.xpt") %>% clean_names()
glu  <- read_xpt("D:/School/Biostatistics/data/GLU_J.xpt") %>% clean_names()
bmx  <- read_xpt("D:/School/Biostatistics/data/BMX_J.xpt") %>% clean_names()

# ---- Merge NHANES data by SEQN ----
nhanes <- demo %>%
  left_join(diq,  by = "seqn") %>%
  left_join(glu,  by = "seqn") %>%
  left_join(bmx,  by = "seqn")

glimpse(nhanes)

# ---- Select relevant NHANES variables ----
nhanes_selected <- nhanes %>%
  select(
    seqn,
    age = ridageyr,                # Age in years
    gender = riagendr,             # 1 = Male, 2 = Female
    bmi = bmxbmi,                  # Body Mass Index
    glucose = lbxglu,              # Fasting glucose (mg/dL)
    diabetes_status = diq010       # Doctor told you have diabetes: 1=Yes, 2=No, 3=Borderline
  ) %>%
  filter(!is.na(glucose), !is.na(bmi), !is.na(age), !is.na(diabetes_status)) %>%
  mutate(
    gender = ifelse(gender == 2, "female", "male"),
    diabetes_binary = ifelse(diabetes_status == 1, 1, 0)
  )

# ---- Align Pima dataset to match ----
pima_selected <- pima %>%
  select(
    age,
    bmi,
    glucose,
    diabetes_binary = outcome
  ) %>%
  mutate(
    gender = "female"  # All Pima patients are female
  )

# ---- Preview cleaned datasets ----
summary(pima_selected)
summary(nhanes_selected)



# Combine both datasets into one long format
combined_glucose <- bind_rows(
  pima_selected %>% mutate(dataset = "Pima"),
  nhanes_selected %>% mutate(dataset = "NHANES")
)

# Glucose Histogram
postscript("glucose_histogram.eps", width=8, height=6, paper="special", horizontal=FALSE)
ggplot(combined_glucose, aes(x = glucose, fill = dataset)) +
  geom_histogram(bins = 30, position = "identity", color = "black", alpha = 1) +
  scale_fill_manual(values = c("Pima" = "steelblue", "NHANES" = "darkorange")) +
  labs(title = "Glucose Distribution", x = "Fasting Glucose", y = "Count") +
  theme_minimal()
dev.off()


# BMI Boxplot
postscript("bmi_boxplot.eps", width=8, height=6, paper="special", horizontal=FALSE)
ggplot(combined_glucose, aes(x = dataset, y = bmi, fill = dataset)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Pima" = "skyblue", "NHANES" = "coral")) +
  labs(title = "BMI Comparison", x = "", y = "BMI") +
  theme_minimal()
dev.off()


# Barplot of diabetes prevalence
combined_diabetes <- bind_rows(
  pima_selected %>% mutate(dataset = "Pima"),
  nhanes_selected %>% mutate(dataset = "NHANES")
)

# Diabetes Prevalence Barplot
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



# T-tests
sink("D:/School/Biostatistics/output/stat_tests.txt")
t.test(glucose ~ dataset, data = combined_glucose)
t.test(bmi ~ dataset, data = combined_glucose)
t.test(age ~ dataset, data = combined_glucose)

# Create contingency table
diabetes_table <- table(combined_diabetes$dataset, combined_diabetes$diabetes_binary)
diabetes_table

# Chi-square test
chisq.test(diabetes_table)
sink()


# Logistic regression: Pima
sink("D:/School/Biostatistics/output/logistic_models.txt")
pima_model <- glm(diabetes_binary ~ age + bmi + glucose, data = pima_selected, family = "binomial")
summary(pima_model)


# Logistic regression: NHANES
nhanes_model <- glm(diabetes_binary ~ age + bmi + glucose, data = nhanes_selected, family = "binomial")
summary(nhanes_model)
sink()
getwd()


