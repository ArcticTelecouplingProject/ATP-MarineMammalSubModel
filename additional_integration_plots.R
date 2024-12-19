# Title: Additional Plots from Master Integration Script
# Author: Sydney Waloven
# Date: 2024-12-19

# Create timeline of collection years for each data source_file -----
p5 <- scripts_focal %>% 
  st_drop_geometry() %>%
  group_by(source_file) %>%
  summarize(
    # Create collection periods with start and end dates for each data source_file
    start_date = min(date),
    end_date = max(date),
    length_years = as.numeric(difftime(max(date), min(date), units = "days")) / 365.25,
    length_days = as.numeric(difftime(max(date), min(date), units = "days")),
    
    # Drop grouping
    .groups = "drop"
  ) %>%
  
  # Reorder source files by collection periods 
  mutate(source_file = fct_reorder(source_file, length_years)) %>% 
  
  # Plot collection periods
  ggplot(aes(x = start_date, y = source_file, xend = end_date, yend = source_file)) +
  geom_segment(linewidth = 3, color = "royalblue2") +
  geom_text(aes(label = paste0(format(start_date, "%Y"), " -")),
            vjust = -0.5,
            hjust = 1, 
            size = 3,
            color = "black") +
  geom_text(aes(x = end_date, label = paste0(" ", format(end_date, "%Y"))), 
            vjust = -0.5, 
            hjust = 0,
            size = 3, 
            color = "black") +
  labs(title = "Collection Period by Source File",
       x = "Year",
       y = "File Name Abbr.") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 17),
        axis.title.y = element_text(face = "bold", size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12))

ggsave("../Figures/final/collection_period_source_file.png", plot = p5, width = 10, height = 8)




#	Display the number of occurrences to compare between data sets that focused on one species vs multiple species -----
p6 <- scripts_focal %>% 
  # mutate(
  #   # Group GBIF occurrences by species query instead of individual "Collected by"
  #   plot_source = case_when(
  #     str_detect(source, "GBIF Walrus Query") ~ "GBIF Walrus Query",
  #     str_detect(source, "GBIF Spotted Seal Query") ~ "GBIF Spotted Seal Query",
  #     str_detect(source, "GBIF Bearded Seal Query") ~ "GBIF Bearded Seal Query",
  #     TRUE ~ source),
  # ) %>% 
  st_drop_geometry() %>% 
  group_by(source, scientificName) %>%
  summarize(
    # Sum total number of occurrences for each data source
    num_obs = n(),
    
    # Drop grouping
    .groups = "drop"
  ) %>% 
  
  # Plot occurrences by data source
  ggplot(aes(x = num_obs, y = source, fill = scientificName)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ scientificName) +
  labs(title = "Marine Mammal Occurrences by Data Source",
       x = "Occurrences",
       y = "Data Source",
       fill = "Scientific Name") +
  scale_fill_manual(values = c("slategray4", "burlywood4", "lightblue2")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 17),
        axis.title.y = element_text(face = "bold", size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 12), # facet headers
        legend.title = element_text(face = "bold"))

# Save plot
ggsave("../Figures/final/occurrences_by_source_facet_species.png", plot = p6, width = 12, height = 8)

