# Reproducible_data
# # Load the necessary libraries
library(dplyr)
# Read the dataset
tza_data <- read_csv("C:/Users/Zakho/Documents/World Bank course/Reproducible Data/Reproducible Data Analytics/Analysis-Hands-on/Data/Final/TZA_CCT_analysis.csv")
# summary statistics 
summary_stats <- tza_data %>%
  group_by(district) %>%
  summarise(
    mean_hh_size = mean(hh_size, na.rm = TRUE),
    sd_hh_size = sd(hh_size, na.rm = TRUE),
    mean_n_child_5 = mean(n_child_5, na.rm = TRUE),
    sd_n_child_5 = sd(n_child_5, na.rm = TRUE),
    mean_food_cons = mean(food_cons_usd_w, na.rm = TRUE),
    sd_food_cons = sd(food_cons_usd_w, na.rm = TRUE),
    mean_area_acre = mean(area_acre_w, na.rm = TRUE),
    sd_area_acre = sd(area_acre_w, na.rm = TRUE),
    mean_crop_damage = mean(crop_damage, na.rm = TRUE),
    sd_crop_damage = sd(crop_damage, na.rm = TRUE)
  )
# Export summary statistics 
write_csv(summary_stats, "C:/Users/Zakho/Documents/World Bank course/Reproducible Data/Reproducible Data Analytics/Analysis-Hands-on/Data/Final/summary_stats.csv")

# Exercise 2 Simple Regression 

1. # Basic regression models on the datasets 
model1 <- lm(food_cons_usd_w ~ treatment, data = tza_data)
summary(model1)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage, data = tza_data)
summary(model2)
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = tza_data)
summary(model3)

# Exercise 3 Create a Histogram 

# Step 1: Filter treatment group
treatment_group <- tza_data %>% filter(treatment == 1)

# Step 2: Create histogram
histogram_plot <- ggplot(treatment_group, aes(x = area_acre_w)) +
  geom_histogram(binwidth = 0.5, fill = "#2C77B8", color = "black") +
  labs(
    title = "Cultivated Area (Acres) - Treatment Group",
    x = "Area in Acres",
    y = "Number of Households"
  ) +
  theme_minimal()

# Step 4: Display the plot
print(histogram_plot)

# Step 5: Save plot as PNG
ggsave("area_histogram_treatment.png", plot = histogram_plot, width = 8, height = 6)


# To view saved output 
getwd()


