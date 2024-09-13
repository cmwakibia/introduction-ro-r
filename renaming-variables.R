library(dplyr)

# Sample data frame
df <- data.frame(X1 = 1:5, X2 = 6:10, X3 = 11:15)

# Select columns where the column is numeric and rename them
renamed_df <- df %>%
  select(where(is.numeric)) %>%
  rename_with(~paste0("Column_", .), everything())

# View the renamed data frame
print(renamed_df)


# new comment