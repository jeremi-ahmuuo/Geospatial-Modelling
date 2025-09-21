# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)

# Load shapefile
kenya_counties <- st_read("D:/Documents/work/Jacinta/kenya counties shapefile/County.shp")

# Load Excel data
excel_data <- read_excel("D:/Documents/work/Jacinta/counties_new-2016-2020.xlsx")

# Join the shapefile with the Excel data based on county name
kenya_map <- kenya_counties %>%
  left_join(excel_data, by = "COUNTY")

# Create a new column for customized legend labels in the Excel data
kenya_map <- kenya_map %>%
  mutate(legend_label = paste(COUNTY_ID, " - ", COUNTY, sep = ""))

# Plot the map
ggplot(data = kenya_map) +
  geom_sf(aes(fill = COUNTY_ID)) +  # Use COUNTY_ID as the fill variable
  scale_fill_viridis_c(option = "D", name = "Legend") +  # Color palette for the counties
  geom_sf_text(aes(label = COUNTY_ID), size = 3, color = "white", 
               position = position_nudge(y = 0.05), na.rm = TRUE) +  # Add labels to counties
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(label = kenya_map$legend_label)))  # Customize legend labels


#correcting errors

# Transform coordinates if necessary to avoid st_point_on_surface warnings (e.g., to WGS84)
kenya_map <- st_transform(kenya_map, crs = 4326)

# Create the custom legend label column only if join was successful and data rows align
if (nrow(kenya_map) == nrow(kenya_counties)) {
  kenya_map <- kenya_map %>%
    mutate(legend_label = paste(COUNTY_ID, " - ", COUNTY, sep = ""))
} else {
  warning("Row mismatch after join; verify that COUNTY column names are compatible")
}

# Plot the map
ggplot(data = kenya_map) +
  geom_sf(aes(fill = COUNTY_ID)) +
  scale_fill_viridis_c(option = "D", name = "County ID") +  # Adjust legend title as needed
  geom_sf_text(aes(label = COUNTY_ID), size = 0.5, color = "white", na.rm = TRUE) +  # Add county ID labels
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(label = kenya_map$legend_label)))  # Display customized legend labels


# more corrections
# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)

# Load shapefile
kenya_counties <- st_read("D:/Documents/work/Jacinta/kenya counties shapefile/County.shp")

# Load Excel data
excel_data <- read_excel("D:/Documents/work/Lynda/mapping/counties.xlsx")
View(excel_data)

# Ensure consistent column names for the join
names(excel_data) <- toupper(names(excel_data))  # Convert to uppercase for compatibility
kenya_map <- kenya_counties %>%
  left_join(excel_data, by = "COUNTY")  # Join based on COUNTY column

# Check if join was successful and inspect the dimensions
print(paste("Rows in shapefile:", nrow(kenya_counties)))
print(paste("Rows in Excel data:", nrow(excel_data)))
print(paste("Rows after join:", nrow(kenya_map)))

# Transform coordinates if necessary to avoid warnings
kenya_map <- st_transform(kenya_map, crs = 4326)

# Plot without custom legend labels initially
ggplot(data = kenya_map) +
  geom_sf(aes(fill = COUNTY_ID)) +
  scale_fill_viridis_c(option = "D", name = "County ID") +
  geom_sf_text(aes(label = COUNTY_ID), size = 0.5, color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(legend.position = "right")


#corrections 3, adding combined legend

# Create a new column for combined labels
kenya_map <- kenya_map %>%
  mutate(legend_label = paste(COUNTY_ID, " - ", COUNTY, sep = ""))

# Plot the map
ggplot(data = kenya_map) +
  geom_sf(aes(fill = legend_label)) +  # Use the combined label for fill
  scale_fill_viridis_d(name = "County ID - Name") +  # Adjust legend title
  geom_sf_text(aes(label = COUNTY_ID), size = 3, color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(legend.position = "right")

#Removing NA in legend_label column

# Create a new column for combined labels, handling NAs
kenya_map <- kenya_map %>%
  mutate(legend_label = ifelse(is.na(COUNTY_ID) | is.na(COUNTY), 
                               NA, 
                               paste(COUNTY_ID, " - ", COUNTY, sep = "")))

# Plot the map
ggplot(data = kenya_map) +
  geom_sf(aes(fill = legend_label)) +  # Use the combined label for fill
  scale_fill_viridis_d(name = "County ID - Name") +  # Adjust legend title
  geom_sf_text(aes(label = COUNTY_ID), size = 3, color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 8),  # Adjust size here (smaller font)
    legend.title = element_text(size = 10)  # Optionally adjust title size
  )
# Save the map to a file
ggsave("D:\\Documents\\work\\Jacinta\\kenya_county_map.png", plot = map_plot, width = 10, height = 8, dpi = 300)


# changing legend_label to follow order

# Arrange kenya_map by COUNTY_ID to ensure it follows numeric order
kenya_map <- kenya_map %>%
  arrange(as.numeric(COUNTY_ID)) %>%
  mutate(
    legend_label = ifelse(is.na(COUNTY_ID) | is.na(COUNTY), 
                          NA, 
                          paste(COUNTY_ID, " - ", COUNTY, sep = "")),
    # Convert legend_label to a factor with levels in numeric order of COUNTY_ID
    legend_label = factor(legend_label, levels = unique(legend_label))
  )

# Plot the map with the ordered legend
map_plot <- ggplot(data = kenya_map) +
  geom_sf(aes(fill = legend_label)) +  # Use the ordered legend_label for fill
  scale_fill_viridis_d(name = "County ID - Name") +  # Adjust legend title
  geom_sf_text(aes(label = COUNTY_ID), size = 3, color = "white", na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Map of Kenya by County") +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 8),  # Adjust size here (smaller font)
    legend.title = element_text(size = 10)  # Optionally adjust title size
  ) +
annotate("text", x = Inf, y = -Inf, label = "Drawn using coordinates", hjust = 2.5, vjust = -1, size = 3)
map_plot
# Save the ordered map
ggsave("D:/Documents/work/Lynda/kenya_county_map_ordered.png", plot = map_plot, width = 10, height = 8, dpi = 300)

