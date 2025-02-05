#Problem 1 
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/maria/Desktop/DataViz/crime_data.csv")

# Display the first 5 rows
head(data)

# Identify columns with missing values and their respective counts
missing_column_value <- colSums(is.na(data))

# Print the missing values in each column
# print(missing_column_value)

threshold <- nrow(data) * 0.5  # 50% of the total rows
columns_to_drop <- names(missing_column_value[missing_column_value > threshold])

# Create a new dataset without these columns
df_cleaned <- data[, !(names(data) %in% columns_to_drop)]

# Save the cleaned dataset to a CSV file
write.csv(df_cleaned, "cleaned_crimedataset.csv", row.names = FALSE)

# Convert the 'DATE OCC' column to datetime format
df_cleaned$`DATE OCC` <- as.Date(df_cleaned$`DATE OCC`, format = "%d/%m/%Y")

# Extract year, month, and day into separate columns
df_cleaned$Year <- format(df_cleaned$`DATE OCC`, "%Y")
df_cleaned$Month <- format(df_cleaned$`DATE OCC`, "%m")
df_cleaned$Day <- format(df_cleaned$`DATE OCC`, "%d")

# Create column for hour
df_cleaned$Hour <- substr(sprintf("%04d", df_cleaned$`TIME OCC`), 1, 2)

# Display the updated dataframe with new columns
head(df_cleaned[, c('DATE OCC', 'Year', 'Month', 'Day', 'TIME OCC', 'Hour')])

# Filter the dataset for crimes that occurred in 2023
df_filtered_2023 <- subset(df_cleaned, Year == 2023)

# Further filter for crimes with the description 'BURGLARY' in the 'Crm Cd Desc' column
df_filtered_burglary <- subset(df_filtered_2023, `Crm Cd Desc` == 'BURGLARY')

# Save the filtered dataset to a CSV file
write.csv(df_filtered_burglary, "burglary_2023.csv", row.names = FALSE)

# Display the filtered dataset
head(df_filtered_burglary)

# Filter the dataset for crimes that occurred in 2023
df_filtered_2023 <- subset(df_cleaned, Year == 2023)

# Further filter for crimes with the description 'BURGLARY' in the 'Crm Cd Desc' column
df_filtered_burglary <- subset(df_filtered_2023, `Crm Cd Desc` == 'BURGLARY')

# Save the filtered dataset to a CSV file
write.csv(df_filtered_burglary, "burglary_2023.csv", row.names = FALSE)

# Display the filtered dataset
head(df_filtered_burglary)

# Group the data by AREA NAME and calculate the total number of crimes and the average victim age
df_grouped <- df_cleaned %>%
  group_by(`AREA NAME`) %>%
  summarise(
    total_crimeN = n(),
    average_vict_age = mean(`Vict Age`, na.rm = TRUE)
  )

# Sort by total crimes in descending order
df_grouped_sorted <- df_grouped %>%
  arrange(desc(total_crimeN))

# Display the sorted results
head(df_grouped_sorted)





#Part 3: Further Exploration (R only)
 #Group the data by Month and count the number of crimes.
df_grouped_by_month <- df_cleaned %>%
   group_by(Month) %>%
   summarise(crime_count = n())
head(df_grouped_by_month)

# Count the number of crimes where a weapon was used (Weapon Used Cd is not null)
crime_with_weapon <- sum(!is.na(df_cleaned$`Weapon Used Cd`))

# Display the result
head(crime_with_weapon)

#Group the data by Premis Desc and count the number of crimes.
df_grouped_by_PremisDesc <- df_cleaned %>%
   group_by('Premis Desc') %>%
   summarise(crime_count = n())
head(df_grouped_by_PremisDesc) 



#Part 4
# Initialize the 'Severity Score' column with 0
df_cleaned$`Severity Score` <- 0

# Assign 5 points if a weapon was used (Weapon Used Cd is not null)
df_cleaned$`Severity Score`[!is.na(df_cleaned$`Weapon Used Cd`)] <- 5

# Assign 3 points for BURGLARY crimes
df_cleaned$`Severity Score`[df_cleaned$`Crm Cd Desc` == 'BURGLARY'] <- 3

# Assign 1 point for all other crimes
df_cleaned$`Severity Score`[is.na(df_cleaned$`Weapon Used Cd`) & df_cleaned$`Crm Cd Desc` != 'BURGLARY'] <- 1

severity_by_area <- df_cleaned %>%
  group_by(`AREA NAME`) %>%
  summarise(total_severity = sum(`Severity Score`, na.rm = TRUE)) 

head(severity_by_area)
