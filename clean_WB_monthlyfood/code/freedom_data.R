# Load necessary library
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)

# Read in the CSV file
file_path <- "../input/WLD_RTFP_mkt_2024-10-21.csv"
data <- read.csv(file_path)

# Define the list of the thirty-five largest countries by population and their largest regions
country_list <- c("China", "India", "United States", "Indonesia", "Pakistan",
              "Nigeria", "Brazil", "Bangladesh", "Russia", "Mexico",
              "Japan", "Ethiopia", "Philippines", "Egypt", "Vietnam",
              "DR Congo", "Turkey", "Iran", "Germany", "Thailand",
              "United Kingdom", "France", "Italy", "South Africa", "Tanzania",
              "Myanmar", "South Korea", "Colombia", "Kenya", "Argentina",
              "Algeria", "Sudan", "Uganda", "Iraq", "Ukraine")

  largest_region <-  c("Xinjiang", "Uttar Pradesh", "Texas", "Java", "Punjab",
                     "Lagos", "São Paulo", "Dhaka", "Sakha (Yakutia)", "Mexico City",
                     "Tokyo", "Oromia", "Metro Manila", "Cairo", "Hanoi",
                     "Kinshasa", "Istanbul", "Tehran", "Bavaria", "Bangkok",
                     "Greater London", "Île-de-France", "Lombardy", "Gauteng", "Dar es Salaam",
                     "Yangon", "Seoul", "Bogotá", "Nairobi", "Buenos Aires",
                     "Algiers", "Khartoum", "Kampala", "Baghdad", "Kyiv")



# Join with the main data to subset by country and largest region
filtered_data <- data %>%
  filter(country %in% country_list) %>%
  filter(adm1_name %in% largest_region) %>%
  filter(!(country == "Malaysia" & adm1_name == "Yangon" & adm2_name != "Yangon (South)"))

unique_countries <- unique(filtered_data$country)
print("Unique countries in the dataset:")
print(unique_countries)


ggplot(filtered_data, aes(x = o_rice)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  facet_wrap(~ country, scales = "free") +
  labs(title = "Distribution of Prices for 'o_rice'",
       x = "Price",
       y = "Frequency") +
  theme_minimal()



# Define the six target countries
target_countries <- c("Kenya", "Sudan", "Nigeria", "Myanmar", "Bangladesh", "Iraq")

# Filter data to include only the target countries
filtered_data <- data %>%
  filter(country %in% target_countries) %>%
  filter(adm1_name %in% largest_region) %>%
  filter(!(country == "Malaysia" & adm1_name == "Yangon" & adm2_name != "Yangon (South)"))





long_data <- filtered_data %>%
  pivot_longer(
    cols = 19:ncol(data),              # Start from the 19th column to the last column
    names_to = "commodity",            # Name the new column that holds the old column names
    values_to = "price"                # Name the new column that holds the values
  )


allowed_na_commodities <- long_data %>%
  group_by(commodity, country) %>%                   # Group by commodity and country
  summarize(num_nas = sum(is.na(price)), .groups = 'drop') %>%  # Count NAs for each commodity-country pair
  filter(num_nas <= 20) %>%                          # Allow only commodities with <= 20 NAs per country
  group_by(commodity) %>%                            # Group by commodity to check all countries
  filter(n_distinct(country) == length(target_countries)) %>%  # Ensure each commodity has all countries
  pull(commodity)  

# Print the result
print(allowed_na_commodities)


ggplot(filtered_data, aes(x = c_oil)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  facet_wrap(~ country, scales = "free") +
  labs(title = "Distribution of Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()

#### which commodities do we have data for?
commodity_ranking <- long_data %>%
  group_by(commodity) %>%                    # Group by commodity
  summarize(num_observations = sum(!is.na(price)), .groups = 'drop') %>%  # Count non-NA observations
  arrange(desc(num_observations)) %>%         # Sort by number of observations in descending order
  slice(1:100)                                # Select the top 100 commodities

# Add ranking column
commodity_ranking <- commodity_ranking %>%
  mutate(rank = row_number())                 # Add rank from 1 to 100

# View the result
print(commodity_ranking, n= 100)



library(stringr)

# Function to check for any common 4-character substring
get_unique_commodities <- function(commodities) {
  selected_commodities <- character()
  for (commodity in commodities) {
    # Check if commodity contains any 4-character substring present in already selected commodities
    is_duplicate <- any(sapply(selected_commodities, function(selected) {
      any(str_detect(commodity, str_sub(selected, 1, 4)))
    }))
    
    # Add commodity to selected list if it's not a duplicate
    if (!is_duplicate) {
      selected_commodities <- c(selected_commodities, commodity)
    }
  }
  return(selected_commodities)
}

# Use the function to select unique commodities
top_100_commodities <- commodity_ranking %>%
  pull(commodity) %>%
  get_unique_commodities()

# Filter long data to only include the selected commodities
filtered_data_top_100 <- long_data %>%
  filter(commodity %in% top_100_commodities)

selected_regions <- filtered_data_top_100 %>%
  group_by(country) %>%
  sample_n(1, replace = FALSE) %>%
  select(country, adm2_name) %>%
  ungroup()

# Filter the main dataset to keep only the selected regions for each country
filtered_data_single_region <- filtered_data_top_100 %>%
  semi_join(selected_regions, by = c("country", "adm2_name"))


# Create a histogram for each selected commodity
for (commodity_name in top_100_commodities) {
  # Filter data for the current commodity
  commodity_data <- filtered_data_single_region %>%
    filter(commodity == commodity_name)
  
  # Generate the plot
  p <- ggplot(commodity_data, aes(x = price)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black") +
    facet_wrap(~ country, scales = "free") +
    labs(title = paste("Distribution of Prices for", commodity_name),
         x = "Price",
         y = "Frequency") +
    theme_minimal()
  
  # Save the plot to the output folder
  ggsave(filename = paste0("../output/", commodity_name, ".png"), plot = p, width = 8, height = 6)
}

