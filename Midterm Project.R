#Load the libraries
library(dplyr)
library(ggplot2)

#loading the dataset
hospitals <- read.csv("hospitals.csv")

# a) how big is the data? 
dim(hospitals)

# b) What are the names of the columns?
colnames(hospitals)

# c) What data types are each column?
sapply(hospitals, class)

# d) Are there missing values?
is.na(hospitals)

# e) Which hospital has the lowest number of beds?
hospitals %>% filter(Beds == min(Beds)) %>%
  select(Hospital.Number)

# f) Which hospital has the lowest expense?
hospitals %>% filter(Total.Expense == min(Total.Expense)) %>%
  select(Hospital.Number)

# g) How many hospitals deliver babies?
hospitals %>% filter(Births > 0) %>% nrow()

# h) Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospitals, aes(x = Beds, y = Total.Expense)) +
  geom_point()

# i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospitals, aes(x = Admissions, y = Total.Expense)) +
  geom_point()

# j) Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
hospitals %>% filter(Births > 0) %>%
  ggplot(aes(x = Beds, y = Total.Expense)) +
  geom_point()

# k) One more question that you believe would be useful.
## What is the average cost per patient admission for each hospital, and how does it vary between populations?

# Calculate average cost per patient admission for each hospital
hospitals_cost_admission <- hospitals %>%
  mutate(Avg_Cost_Per_Admission = Total.Expense / Admissions)

# Calculate average cost per admission for each population
hospitals_cost_adm_pop <- hospitals_cost_admission %>%
  group_by(Personnel) %>%
  summarise(Avg_Cost = mean(Avg_Cost_Per_Admission, na.rm = TRUE))

print(hospitals_cost_adm_pop)

#-------------------------------------------------------------------------------------------------------
# Create a pie chart

ggplot(hospitals, aes(x = "", y = Admissions, fill = Control)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Comparison of Admissions and Control",
       fill = "Category",
       x = NULL, y = NULL) +
  theme_minimal()


# Create a bar chart
Adm_reg <- hospitals %>%
  group_by(Region) %>%
  summarise(Total_Admissions = sum(Admissions))

ggplot(Adm_reg, aes(x = Region, y = Total_Admissions)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Use a blue color for bars
  labs(title = "Total Admissions by Region",
       x = "Region", y = "Total Admissions") +  # Set axis labels
  theme_minimal()

# Create a line chart

line_data <- hospitals %>%
  group_by(Region.Number) %>%
  summarise(Total_Expense = sum(Total.Expense),
            Total_Admissions = sum(Admissions))

# Create the line chart
ggplot(line_data, aes(x = Region.Number)) +
  geom_line(aes(y = Total_Expense, color = "Total Expense")) +
  geom_line(aes(y = Total_Admissions, color = "Total Admissions")) +
  labs(title = "Comparison of Total Expense and Admissions by Region Number",
       x = "Region Number", y = "Value") +
  scale_color_manual(values = c("Total Expense" = "blue", "Total Admissions" = "green")) +
  theme_minimal()


# Perform simple linear regression
lm_model <- lm(Total.Expense ~ Admissions, data = hospitals)

# Get the summary of the regression model
summary_model <- summary(lm_model)

# Print the summary to view the regression coefficients, R-squared, p-values, etc.
print(summary_model)

# Extract the coefficients and R-squared value
coefficients <- coef(lm_model)
r_squared <- summary_model$r.squared

# Print the coefficients and R-squared value
cat("Intercept (alpha):", coefficients[1], "\n")
cat("Regression Coefficient (beta):", coefficients[2], "\n")
cat("R-squared value:", r_squared, "\n")

# Plot the scatter plot with regression line
scatter_plot <- ggplot(hospitals, aes(x = Admissions, y = Total.Expense)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot with Regression Line",
       x = "Admissions",
       y = "Total Expense")

# Print the scatter plot
print(scatter_plot)

## pvalue < .05 means there is statistical evidence to suggest that we can reject the null hypothesis

##R-squared (R²): The R-squared value indicates the proportion of variance in Total Expense that
##is explained by the linear regression model. In this case, it tells us how well Admissions predicts Total Expense.

## Interpretation of R-squared: A higher R-squared value (closer to 1) indicates that a larger proportion
## of the variance in Total Expense is explained by Admissions, making the model more reliable for predicting 
## Total Expense based on Admissions.

## P-values: The p-values associated with the coefficients indicate the statistical significance of the predictor 
## variables (in this case, Admissions) in explaining the variation in Total Expense. 
## A low p-value (typically < 0.05) suggests that the predictor variable is statistically significant in the model.

## 55 mil + 2610.9078 = 25.5930x -> 2,149,127
## 75 mil + 2610.9078 = 25.5930x -> 2,930,591

## The regressions performed are indeed useful for answering the initial question regarding Option A or Option B.
## By analyzing the impact of independent variables (such as Admissions and Beds) on Total Expense through 
## regression analysis, we can gain insights into which factors contribute more significantly to the expenses 
## incurred by the hospital.

# Perform multivariate linear regression
lm_model_multivariate <- lm(Total.Expense ~ Admissions + Beds, data = hospitals)

# Get the summary of the regression model
summary_model_multivariate <- summary(lm_model_multivariate)

# Print the summary to view the regression coefficients, R-squared, p-values, etc.
print(summary_model_multivariate)

# Extract the coefficients and R-squared value
coefficients_multivariate <- coef(lm_model_multivariate)
r_squared_multivariate <- summary_model_multivariate$r.squared

# Print the coefficients and R-squared value
cat("Intercept (alpha):", coefficients_multivariate[1], "\n")
cat("Regression Coefficient (beta) for Admissions:", coefficients_multivariate[2], "\n")
cat("Regression Coefficient (beta) for Beds:", coefficients_multivariate[3], "\n")
cat("R-squared value:", r_squared_multivariate, "\n")

# Explain R-squared and p-values
cat("R-squared measures the proportion of variance in Total Expense explained by the independent variables (Admissions and Beds) in the model.\n")
cat("P-values associated with the coefficients indicate the statistical significance of the predictor variables. A low p-value (typically < 0.05) suggests that the predictor variable is statistically significant in explaining the variation in Total Expense.\n")

##R-squared (R²): The R-squared value indicates the proportion of variance in Total Expense that is 
## explained by the multivariate linear regression model. In this case, it tells us how well Admissions and Beds 
## collectively predict Total Expense.

## Interpretation of R-squared: A higher R-squared value (closer to 1) indicates that a larger proportion of the 
## variance in Total Expense is explained by the combination of Admissions and Beds, making the model more reliable
## for predicting Total Expense based on these variables.

## P-values: The p-values associated with the coefficients for Admissions and Beds indicate their statistical 
## significance in explaining the variation in Total Expense. A low p-value (typically < 0.05) suggests that the 
## predictor variable is statistically significant in the model.

## Based on the analysis and interpretation of the multivariate regression results, you can make recommendations 
## regarding the impact of Admissions and Beds on Total Expense and their significance in predicting Total Expense.

