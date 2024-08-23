# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("openxlsx")) install.packages("openxlsx")
library("openxlsx")

# Read the CSV file
df <- read_csv('CurrBal-20240606.csv')

# Check the structure of df to identify column types
str(df)

# Convert currbal column to numeric (assuming it's the 5th column)
df <- df %>%
  mutate(currbal = as.numeric(currbal))

# Check for successful conversion
str(df)

# Bin CurrBal column into 3 equal-width intervals
df <- df %>%
  mutate(Bucket = cut_number(currbal, n = 3, labels = c("Low", "Medium", "High")))

# Print data to check the Bucket column
print(df)

# Prompt user for ticket number
ticket_number <- readline(prompt = "Enter the ticket number: ")

# Sort the data frame by the clientkey column
df <- df %>%
  arrange(clientkey)

# Generate the filename based on the ticket number with prefix "DS-"
filename <- paste0("DS-", ticket_number, ".xlsx")

# Extract the worksheet name (same as filename without extension)
worksheet_name <- paste0("DS-", ticket_number)

# Create a new workbook and add a worksheet with the extracted name
wb <- createWorkbook()
addWorksheet(wb, worksheet_name)

# Write the data to the worksheet
writeData(wb, worksheet_name, df)

# Create styles
RedStyle <- createStyle(bgFill = "red")
YellowStyle <- createStyle(bgFill = "yellow")
GreenStyle <- createStyle(bgFill = "green")

# Number of columns in the dataframe
num_cols <- ncol(df)

# # Apply conditional formatting to entire row based on column values
# conditionalFormatting(wb, sheet = "FirstSheet", cols = 1:num_cols, 
#                       rows = 2:(nrow(df) + 1),
#                       rule = "$B2 >= 1000", 
#                       style = RedStyle)

# conditionalFormatting(wb, sheet = "FirstSheet", cols = 1:num_cols, 
#                       rows = 2:(nrow(df) + 1),
#                       rule = "AND($B2 >= 500, $B2 < 1000)", 
#                       style = YellowStyle)

# conditionalFormatting(wb, sheet = "FirstSheet", cols = 1:num_cols, 
#                       rows = 2:(nrow(df) + 1),
#                       rule = "$B2 < 500", 
#                       style = GreenStyle)

# Apply conditional formatting based on bins
conditionalFormatting(wb, sheet = worksheet_name, cols = 1:num_cols, 
                      rows = 2:(nrow(df) + 1),
                      rule = '$F2 == "High"', 
                      style = GreenStyle)

conditionalFormatting(wb, sheet = worksheet_name, cols = 1:num_cols, 
                      rows = 2:(nrow(df) + 1),
                      rule = '$F2 == "Medium"', 
                      style = YellowStyle)

conditionalFormatting(wb, sheet = worksheet_name, cols = 1:num_cols, 
                      rows = 2:(nrow(df) + 1),
                      rule = '$F2 == "Low"', 
                      style = RedStyle)

# Save the workbook to an Excel file
saveWorkbook(wb, filename, overwrite = TRUE)

# Confirmation message
cat("Table exported to", filename, "\n")