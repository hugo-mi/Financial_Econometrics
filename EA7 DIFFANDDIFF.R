
library(quantmod)
library(ggplot2)
library(tseries)
library(urca)
library(httr)
library(dplyr)
library(fredr)


start_date <- as.Date("2004-06-15")
end_date <- as.Date("2012-06-15")

#  Yahoo Finance
get_YF_series <- function(ticker, start_date, end_date) {
  data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  return(Cl(data))
}

# Key API FRED
fredr_set_key("9a54ab68d82273ea59014b16364b5bdd")

# Inflation FRED
get_FRED_series <- function(ticker, start_date, end_date) {
  data <- fredr_series_observations(series_id = ticker, observation_start = start_date, observation_end = end_date)
  data <- data.frame(Date = as.Date(data$date), Inflation = as.numeric(data$value))
  return(data)
}

# Datas
df_crude_oil <- get_YF_series("CL=F", start_date, end_date)
df_inflation_rate <- get_FRED_series("T10YIE", start_date, end_date)

# Log
df_crude_oil$log_price <- log(df_crude_oil[, "CL=F.Close"])

# First diff oil
df_crude_oil$diff_log_price <- diff(df_crude_oil$log_price, differences = 1)
df_crude_oil <- na.omit(df_crude_oil)

# Log
df_inflation_rate$log_inflation <- log(df_inflation_rate$Inflation)

# First diff inflation
diff_log_inflation <- diff(df_inflation_rate$log_inflation, differences = 1)
df_inflation_rate <- df_inflation_rate[-1, ]
df_inflation_rate$diff_log_inflation <- diff_log_inflation

# Transform df 
df_crude_oil <- data.frame(Date = index(df_crude_oil), df_crude_oil, row.names = NULL)
df_inflation_rate <- data.frame(Date = index(df_inflation_rate), df_inflation_rate, row.names = NULL)


df_inflation_rate <- df_inflation_rate %>% 
  select(-Date) %>% 
  rename(Date = Date.1)

# Merge
merged_data <- merge(df_crude_oil, df_inflation_rate, by = "Date", all = FALSE)
merged_data <- na.omit(merged_data)

# Convert treatment_start_date to Date type
treatment_start_date <- as.Date("2008-06-15")

# Create the treatment variable
merged_data$treatment <- ifelse(as.Date(merged_data$Date) >= treatment_start_date, 1, 0)

# Create a time variable
merged_data$time <- as.numeric(as.Date(merged_data$Date) - min(as.Date(merged_data$Date)))

# Interaction term for difference-in-differences
merged_data$interaction_diff_diff <- merged_data$treatment * merged_data$time

# Regression for difference-in-differences
model_diff_diff <- lm(log_inflation ~ log_price + treatment + time + interaction_diff_diff, data = merged_data)
summary(model_diff_diff)

# Plot Inflation (Raw Data)
ggplot(merged_data, aes(x = Date, y = Inflation)) +
  geom_line() +
  labs(title = "Raw Inflation Over Time", x = "Date", y = "Inflation") +
  theme_minimal()

# Plot log_inflation
ggplot(merged_data, aes(x = Date, y = log_inflation)) +
  geom_line() +
  labs(title = "Log of Inflation Over Time", x = "Date", y = "Log of Inflation") +
  theme_minimal()

# Plot log_price
ggplot(merged_data, aes(x = Date, y = log_price)) +
  geom_line() +
  labs(title = "Log of Crude Oil Prices Over Time", x = "Date", y = "Log of Crude Oil Prices") +
  theme_minimal()

# Plot the time variable
ggplot(merged_data, aes(x = Date, y = time)) +
  geom_line() +
  labs(title = "Time Variable Over Time", x = "Date", y = "Time") +
  theme_minimal()

# Plot the treatment variable
ggplot(merged_data, aes(x = Date, y = treatment)) +
  geom_bar(stat = "identity") +
  labs(title = "Treatment Variable Over Time", x = "Date", y = "Treatment") +
  theme_minimal()
