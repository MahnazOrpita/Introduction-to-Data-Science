#Import dataset
data<-read.csv("F:/loan_approval_dataset.csv",header = TRUE,sep=",") 
data 


#1#Find missing value
cat("Missing values in columns:\n") 
print(which(is.na(data$no_of_dependents)))
print(which(is.na(data$education)))
print(which(is.na(data$self_employed)))
print(which(is.na(data$income_annum)))
print(which(is.na(data$loan_amount)))
print(which(is.na(data$loan_term)))
print(which(is.na(data$cibil_score)))
print(which(is.na(data$residential_assets_value)))
print(which(is.na(data$commercial_assets_value)))
print(which(is.na(data$luxury_assets_value)))
print(which(is.na(data$bank_asset_value)))
print(which(is.na(data$loan_status)))
cat("Any missing values in the dataset?\n")
print(anyNA(data))
cat("Matrix of missing values:\n")
print(is.na(data))



#1.1#Identify missing values indices including empty strings
missing_values_indices <- lapply(data, function(x) {
  if (is.integer(x) | is.character(x)) { 
    return(which(is.na(x) | x == ""))
  } else { 
    return(NULL) }})
cat("Missing values indices:\n") 
print(missing_values_indices)



#1.2#Detect missing values
for (nm in names(data)) { 
  if (is.character(data[[nm]]) || is.factor(data[[nm]])) { 
    x <- as.character(data[[nm]]) 
    x[trimws(x) == ""] <- NA 
    data[[nm]] <- x 
  } 
} 
cat("Missing values per column:\n") 
print(colSums(is.na(data))) 
cat ("Total missing values in the dataset:\n") 
print(sum(is.na(data)))



#2#Missing values on a graph
missing_counts <- sapply(missing_values_indices, length)
barplot(
  missing_counts, 
  main = "Missing Values per Column", 
  xlab = "Columns", 
  ylab = "Count of Missing Values", 
  col = "lightblue", 
  border = "darkblue", 
  las = 2,       # rotate column names vertically
  cex.names = 0.4, 
  cex.axis = 0.4
)



#2.1#Replacing Missing Values with Mode, Mean, Median values
df <- read.csv("F:/loan_approval_dataset.csv", stringsAsFactors = FALSE)
#Mode#
get_mode <- function(v) { 
  uniqv <- na.omit(unique(v)) 
  uniqv[which.max(tabulate(match(v, uniqv)))] 
}

numeric_cols <- c("no_of_dependents", "income_annum", "loan_amount", "loan_term",
                  "cibil_score", "residential_assets_value", "commercial_assets_value",
                  "luxury_assets_value", "bank_asset_value")
for (col_name in numeric_cols) {
  df[[col_name]] <- as.numeric(df[[col_name]])
}
for (col_name in numeric_cols) { 
  column_mode <- get_mode(df[[col_name]][!is.na(df[[col_name]])]) 
  df[[col_name]][is.na(df[[col_name]])] <- column_mode 
  df[[col_name]] <- round(df[[col_name]], digits = 0)
}

numOfMissValue <- colSums(is.na(df[, numeric_cols]))
numOfMissValue

head(df[, numeric_cols])

#Mean#
numeric_cols <- c("no_of_dependents", "income_annum", "loan_amount", "loan_term",
                  "cibil_score", "residential_assets_value", "commercial_assets_value",
                  "luxury_assets_value", "bank_asset_value")

for (col_name in numeric_cols) {
  df[[col_name]] <- as.numeric(df[[col_name]])
}

for (col_name in numeric_cols) {
  mean_value <- mean(df[[col_name]], na.rm = TRUE)
  df[[col_name]][is.na(df[[col_name]])] <- mean_value
}

colSums(is.na(df[, numeric_cols]))
head(df[, numeric_cols])

#Median#
numeric_cols <- c("no_of_dependents", "income_annum", "loan_amount", "loan_term",
                  "cibil_score", "residential_assets_value", "commercial_assets_value",
                  "luxury_assets_value", "bank_asset_value")
for (col_name in numeric_cols) {
  df[[col_name]] <- as.numeric(df[[col_name]])
}
cat("Missing values before:\n")
print(colSums(is.na(df[, numeric_cols])))
for (col_name in numeric_cols) {
  median_value <- median(df[[col_name]], na.rm = TRUE)
  df[[col_name]][is.na(df[[col_name]])] <- median_value
}
cat("Missing values after:\n")
print(colSums(is.na(df[, numeric_cols])))
head(df[, numeric_cols])



#3# Outlier Detection & Handling 
numeric_cols <- sapply(df, is.numeric)

for(col in names(df)[numeric_cols]) {
  df[[col]][df[[col]] < 0] <- NA
}

for(col in names(df)[numeric_cols]) {
  df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
}
handle_outliers <- function(x) {
  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  outliers <- x[x < lower | x > upper]
  
  cat("Outliers:\n")
  print(outliers)
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  cat("\nSummary after handling:\n")
  print(summary(x))
  cat("\n====================================\n")
  
  return(x)
}
for(col in names(df)[numeric_cols]) {
  cat("Column:", col, "\n")
  df[[col]] <- handle_outliers(df[[col]])
}



#4# Numeric to Categorical
df$loan_amount_cat <- cut(df$loan_amount,
                          breaks = quantile(df$loan_amount, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
df$income_annum_cat <- cut(df$income_annum,
                           breaks = quantile(df$income_annum, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                           labels = c("Low", "Medium", "High"),
                           include.lowest = TRUE)
head(df[, c("loan_amount", "loan_amount_cat", "income_annum", "income_annum_cat")])


#4.1#Categorical to Numeric
df$education <- ifelse(trimws(df$education) == "", NA, trimws(df$education))
df$self_employed <- ifelse(trimws(df$self_employed) == "", NA, trimws(df$self_employed))
df$loan_status <- ifelse(trimws(df$loan_status) == "", NA, trimws(df$loan_status))
df$education_num <- as.integer(factor(df$education, levels = c("Not Graduate", "Graduate")))
df$self_employed_num <- as.integer(factor(df$self_employed, levels = c("No", "Yes")))
df$loan_status_num <- as.integer(factor(df$loan_status, levels = c("Rejected", "Approved")))
head(df[, c("education", "education_num", "self_employed", "self_employed_num", "loan_status", "loan_status_num")])


#5#Normalization of Continuous Attributes
numeric_cols <- c("no_of_dependents", "income_annum", "loan_amount",
                  "loan_term", "cibil_score", "residential_assets_value",
                  "commercial_assets_value", "luxury_assets_value",
                  "bank_asset_value")

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalized_data <- data
normalized_data[numeric_cols] <- lapply(data[numeric_cols], normalize)

head(normalized_data[numeric_cols])

#6# Finding and Removing Duplicate Rows
cat("Number of duplicate rows:\n")
print(sum(duplicated(data)))
duplicates <- data[duplicated(data), ]
duplicates
data_no_duplicates <- data[!duplicated(data), ]
cat("Rows before removing duplicates:\n")
print(nrow(data))
cat("Rows after removing duplicates:\n")
print(nrow(data_no_duplicates))

#7# Filtering Methods
high_income <- subset(data, income_annum > 5000000)
head(high_income)

long_term <- subset(data, loan_term > 10)
head(long_term)

good_cibil <- subset(data, cibil_score > 700)
head(good_cibil)

filtered_records <- subset(data, income_annum > 5000000 & cibil_score > 700)
head(filtered_records)

#8# Detect invalid data in the dataset and use the appropriate approach to handle those values
library(caret)
set.seed(42)
print(summary(data))
data[data == ""] <- NA
numeric_cols <- c("no_of_dependents","income_annum","loan_amount","loan_term",
                  "cibil_score","residential_assets_value","commercial_assets_value",
                  "luxury_assets_value","bank_asset_value")
for (c in numeric_cols) {
  data[[c]] <- as.numeric(data[[c]])
}
data[which(data$income_annum < 0), "income_annum"] <- NA
data[which(data$loan_amount < 0), "loan_amount"] <- NA
data[which(data$loan_term < 0), "loan_term"] <- NA
data[which(data$residential_assets_value < 0), "residential_assets_value"] <- NA
data[which(data$cibil_score < 0), "cibil_score"] <- NA

library(dplyr)
for (c in numeric_cols) {
  data[[c]][is.na(data[[c]])] <- median(data[[c]], na.rm = TRUE)
}
print(table(trimws(data$education)))
data$education <- trimws(data$education)
data$education[data$education == "Grdauate"] <- "Graduate"
data$education[data$education == "Graduate "] <- "Graduate"
print(table(data$education))
print(summary(data))

#9# We can convert the imbalanced dataset into a balanced dataset 
library(caret)
data$loan_status <- as.factor(data$loan_status)
set.seed(123)
balanced_over <- upSample(x = data[, -which(names(data) == "loan_status")],
                          y = data$loan_status)
balanced_over <- cbind(balanced_over, loan_status = balanced_over$Class)
balanced_over$Class <- NULL

table(balanced_over$loan_status)

#10# Split the dataset foe training and testing
library(caret)
set.seed(42)
index <- createDataPartition(data$loan_status, p = 0.8, list = FALSE)
train <- data[index, ]
test  <- data[-index, ]
print(nrow(train))
print(nrow(test))
print(table(train$loan_status))
print(table(test$loan_status))

#11# Calculate Descriptive Statistics and Interpret Results for Numerical Variables
library(dplyr)
numeric_cols <- c("income_annum","loan_amount","loan_term","cibil_score",
                  "residential_assets_value","commercial_assets_value",
                  "luxury_assets_value","bank_asset_value")
for (col in numeric_cols) {
  data[[col]] <- as.numeric(as.character(data[[col]]))
}
data$loan_status_num <- ifelse(tolower(data$loan_status) == "approved", 1, 0)
desc_stats <- data %>%
  group_by(loan_status_num) %>%
  summarise(
    Mean_Income = mean(income_annum, na.rm = TRUE),
    Median_Income = median(income_annum, na.rm = TRUE),
    SD_Income = sd(income_annum, na.rm = TRUE),
    Min_Income = min(income_annum, na.rm = TRUE),
    Max_Income = max(income_annum, na.rm = TRUE),
    Mean_LoanAmount = mean(loan_amount, na.rm = TRUE),
    Median_LoanAmount = median(loan_amount, na.rm = TRUE),
    SD_LoanAmount = sd(loan_amount, na.rm = TRUE),
    Min_LoanAmount = min(loan_amount, na.rm = TRUE),
    Max_LoanAmount = max(loan_amount, na.rm = TRUE)
  )
cat("Descriptive statistics of income_annum and loan_amount by loan_status (0 = Rejected, 1 = Approved):\n")
print(desc_stats)

#12# Compare the mean values of a selected numerical variable across two distinct categories of an appropriate categorical variable from your dataset.
df$loan_status <- trimws(df$loan_status)
df$loan_status <- factor(df$loan_status, levels = c("Rejected", "Approved"))
mean_comparison <- aggregate(income_annum ~ loan_status, data = df, FUN = mean, na.rm = TRUE)
cat("Mean income_annum by loan_status:\n")
print(mean_comparison)

#13# Task-13 Title: Examine and compare the variability (e.g., IQR,standard deviation, variance, or range,) of another numerical variable across the different categories or levels of a chosen categorical variable within your dataset.
library(dplyr)
df$loan_status <- trimws(df$loan_status)
df$loan_status <- factor(df$loan_status, levels = c("Rejected", "Approved"))
spread_loan <- df %>%
  group_by(loan_status) %>%
  summarise(
    Range = max(loan_amount, na.rm = TRUE) - min(loan_amount, na.rm = TRUE),
    IQR = IQR(loan_amount, na.rm = TRUE),
    Variance = var(loan_amount, na.rm = TRUE),
    SD = sd(loan_amount, na.rm = TRUE)
  )
cat("Spread of loan_amount across loan_status:\n")
print(spread_loan)









