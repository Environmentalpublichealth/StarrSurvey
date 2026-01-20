setwd("~/Desktop/Jiali/TAMU/environment/starr/Survey/")
# Load required libraries
library(ggplot2)
library(sf)
library(tigris)
library(dplyr)

# Get Texas county data
tx_counties <- counties(state = "TX", cb = TRUE) %>%
  st_transform(4326)

# Create vector of counties to highlight
highlight_counties <- c("DeWitt", "Duval", "Brooks", "Starr", 
                        "Zapata", "Nueces", "Kenedy", "Kleberg")

# Add a column for highlighting
tx_counties$highlight <- tx_counties$NAME %in% highlight_counties

# Create the map
ggplot(data = tx_counties) +
  geom_sf(aes(fill = highlight), color = "gray", size = 0.2) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#6495ED"),
                    labels = c("Other Counties", "Highlighted Counties"),
                    name = "County Status") +
  geom_sf_text(data = filter(tx_counties, NAME %in% highlight_counties),
               aes(label = NAME),
               size = 1,
               fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_text(size = 4),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x="",
    y="",
    title = "Texas Counties Map",
    caption = "Data source: US Census Bureau TIGER/Line Shapefiles"
  ) +
  coord_sf(crs = st_crs(4326))
ggsave("img/TOWN samples.pdf",height = 5, width = 5)


tapdata <- data.frame(
  category = c("cooking", "drinking", "coffee", "ice","other"),
  value = c(14, 3, 5, 4, 5)
)

bottledata <- data.frame(
  category = c("cooking", "drinking", "coffee", "ice","other"),
  value = c(8, 17, 13, 10, 1)
)

drinkdata <- data.frame(
  category = c("Tap", "Refrigerator dispenser", "Bottles"),
  value = c(3,1,15)
)

basic_pie <- ggplot(drinkdata, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Most water I drink from")
basic_pie
ggsave("Drinking water.pdf", height = 3, width = 3)

basic_pie <- ggplot(bottledata, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "I used bottle water for")
basic_pie
ggsave("img/bottle water.pdf", height = 3, width = 3)
