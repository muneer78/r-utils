# Load required libraries
library(dplyr)
library(tibble)
library(ggplot2)

# Create a DataFrame with the specified columns
moneyline_values <- c(-500, -1100, -138, -160, -110)
num_values <- length(moneyline_values) + 1

df <- tibble(
  GameNumber = 1:(num_values - 1),
  Moneyline = moneyline_values
)

# Initialize the initial investment
initial_investment <- 2.75

# Initialize variables for calculations
risk <- initial_investment
wins <- c()
returns <- c()
profits <- c()
ml_rollover_odds <- c()

# Calculate winnings, return, profit, and cumulative ML rollover odds
for (moneyline in df$Moneyline) {
  if (moneyline < 0) {
    win <- abs(risk / (moneyline / 100))
  } else {
    win <- risk * (moneyline / 100)
  }
  
  returns <- c(returns, risk + win)
  profits <- c(profits, (risk + win) - initial_investment)
  ml_rollover_odds <- c(ml_rollover_odds, ((risk + win) - initial_investment) / initial_investment * 100)
  
  risk <- returns[length(returns)]
  wins <- c(wins, win)
}

# Add calculated columns to the DataFrame
df <- df %>%
  mutate(
    Risk = c(initial_investment, returns[-length(returns)]),
    Win = wins,
    Return = returns,
    Profit = profits,
    MLRolloverOdds = round(ml_rollover_odds)
  )

# Plotting the Profit column with line and colored area
profit_plot <- ggplot(df, aes(x = GameNumber, y = Win)) +
  geom_line(aes(group = 1), color = "blue") +  # Line connecting data points
  geom_point(color = "blue") + # Data points
  geom_area(fill = "blue", alpha = 0.3) +  # Filled area under the curve
  labs(
    title = "Profit Over Games",
    x = "Game Number",
    y = "Profit"
  ) +
  theme_minimal()

# Print the plot
print(profit_plot)

# Round the values as specified and format as currency
df <- df %>%
  mutate(
    Risk = sprintf("$%.2f", round(Risk, 2)),
    Win = sprintf("$%.2f", round(Win, 2)),
    Return = sprintf("$%.2f", round(Return, 2)),
    Profit = sprintf("$%.2f", round(Profit, 2)),
    MLRolloverOdds = as.integer(MLRolloverOdds)
  )

# Display the DataFrame with calculated columns
print(df)