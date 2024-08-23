# Load necessary libraries, install if not available
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")

# Load libraries
library(tidyverse)
library(scales)

# Function to create custom bins and bin labels
create_bins_and_labels <- function(max_value) {
  # Custom bins for the table: one bin for 0, one bin for 1-999, and the rest in intervals of 1000
  table_bins <- c(0, 1, 1000, seq(2000, max_value + 1000, by = 1000))
  
  # Bin labels for the table
  table_bin_labels <- c("0", "1-999", paste0(seq(1000, max_value, by = 1000), "-", seq(1999, max_value + 999, by = 1000)))
  
  return(list(table_bins = table_bins, table_bin_labels = table_bin_labels))
}

# Function to generate summary table
generate_summary_table <- function(df, table_bins, table_bin_labels) {
  # Create a new column with bin labels for the table
  df_table <- df %>%
    mutate(currbalamt_bin_table = cut(currbalamt, breaks = c(-Inf, table_bins[-1]), labels = table_bin_labels, right = TRUE))
  
  # Group by currbalamt_bin_table and count rows in each group
  summary_table <- df_table %>%
    group_by(currbalamt_bin_table) %>%
    summarize(row_count = n())
  
  return(summary_table)
}

# Function to generate and save a plot
generate_and_save_plot <- function(data, filename, title) {
  # Create a bar plot of the distribution
  plot <- ggplot(data, aes(x = currbalamt_bin_plot, y = row_count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = title,
         x = "currbalamt Bin",
         y = "Count of Rows") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot as a PDF
  ggsave(filename, plot, width = 10, height = 7)
}

# Read the CSV file
df <- read_csv("DS4013-20240531.csv")

# Calculate the maximum value in currbalamt
max_value <- max(df$currbalamt, na.rm = TRUE)

# Create bins and bin labels for the table
bins_and_labels <- create_bins_and_labels(max_value)
table_bins <- bins_and_labels$table_bins
table_bin_labels <- bins_and_labels$table_bin_labels

# Generate summary table
summary_table <- generate_summary_table(df, table_bins, table_bin_labels)

# Print and export summary table
print(summary_table)
write_csv(summary_table, "summary_table.csv")

# Create custom bins for the plot: one bin for 0, and the rest in 10 equal-width bins
plot_bins <- c(0, seq(1, max_value, length.out = 11))

# Bin labels for the plot
plot_bin_labels <- c("0", paste0(round(seq(1, max_value, length.out = 11)[-1], 0)[-length(seq(1, max_value, length.out = 11))],
                                 "-", round(seq(1, max_value, length.out = 11)[-1], 0)[-1]))

# Create a new column with bin labels for the plot
df_plot <- df %>%
  mutate(currbalamt_bin_plot = cut(currbalamt, breaks = table_bins, labels = table_bin_labels, include.lowest = TRUE, right = TRUE)) %>%
  group_by(currbalamt_bin_plot) %>%
  summarize(row_count = n())

# Print and save summary plot
print(df_plot)
generate_and_save_plot(df_plot, "distribution_plot.pdf", "Distribution of currbalamt Bins")
