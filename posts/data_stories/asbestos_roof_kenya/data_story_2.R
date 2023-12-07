# A roof over your head
# By @willyokech
# Data: rKenyaCensus

# 1) National

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_roof <- V4_T2.12

View(df_roof)

# Table 1 for National Analysis
table_1 <- df_roof[1:3,]
View(table_1)

glimpse(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1_select <- table_1 %>%
  select(-c(conventional_households, admin_area, not_stated))

View(table_1_select)
glimpse(table_1_select)


# 5) ggplot2 visualization

# Treemap

#install.packages("treemapify")
library(treemapify)

table_1_select_tidy <- table_1_select %>%
  pivot_longer(c(grass_twigs:shingles), 
               names_to = "roof_type", values_to = "percentage") %>%
  mutate(roof_type = ifelse(roof_type == "grass_twigs", "Grass/Twigs",
                            ifelse(roof_type == "makuti_thatch", "Makuti",
                                   ifelse(roof_type == "dung_mud", "Dung/Mud",
                                          ifelse(roof_type == "ironsheets", "Iron Sheets",
                                                 ifelse(roof_type == "tincans", "Tin Cans",
                                                        ifelse(roof_type == "asbestos_sheets", "Asbestos ",
                                                               ifelse(roof_type == "concrete_cement", "Concrete",
                                                                      ifelse(roof_type == "tiles", "Tiles",
                                                                             ifelse(roof_type == "canvas_tents", "Canvas/Tents",
                                                                                    ifelse(roof_type == "decra_versatile", "Decra",
                                                                                           ifelse(roof_type == "nylon_cartons_cardboard", "Nyl_Cart_Card",
                                                                                                  ifelse(roof_type == "shingles", "Shingles", roof_type))))))))))))) 

table_1_select_tidy

table_1_select_tidy$roof_type

# National

table_1_select_tidy_national <- table_1_select_tidy %>%
  filter(sub_county == "KENYA")


# Remember that treemap is not a ggplot

ggplot(table_1_select_tidy_national, 
       aes(area = percentage, fill = roof_type, 
           label = roof_type)) +
  geom_treemap() +
  labs(caption = "Visualization @willyokech | Source:rKenyaCensus") +
  labs(fill = "Roof Type") +
  theme(legend.position = "bottom") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = TRUE) + 
  scale_fill_brewer(palette = "Spectral")

# 2 County

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_roof <- V4_T2.12

View(df_roof)

# Table 1 for County and Subcounty Analysis
table_1 <- df_roof[4:395,]
View(table_1)

glimpse(table_1)

table_1 <- table_1 %>%
  clean_names()

table_1

table_1_select <- table_1 %>%
  select(c(county, sub_county, admin_area, asbestos_sheets))

View(table_1_select)
glimpse(table_1_select)


table_1_select_county <- table_1_select %>%
  filter(admin_area == "County")

table_1_select_subcounty <- table_1_select %>%
  filter(admin_area == "SubCounty")


# 5) Load the packages required for the maps

#install.packages("sf")
library(sf) # simple features

#install.packages("tmap") #Thematic maps 
library(tmap)

#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)

# Load the shapefiles that are downloaded from online source
KenyaSHP <- read_sf("posts/series_2/new_post_2/kenyan-counties/County.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)


# To easily view the shapefile in RStudio View pane, you can drop the geometry column and view the rest of the data.

View(KenyaSHP %>% st_drop_geometry())

# Shapefile Data Inspection

print(KenyaSHP[5:9], n = 6)

colnames(KenyaSHP)

class(KenyaSHP)

# Look at the variable data types

glimpse(KenyaSHP)

# View the geometry column

KenyaSHP_geometry <- st_geometry(KenyaSHP)

### View one geometry entry
KenyaSHP_geometry[[1]]

# View the classes of the geometry columns

class(KenyaSHP_geometry) #sfc, the list-column with the geometries for each feature

class(KenyaSHP_geometry[[1]]) #sfg, the feature geometry of an individual simple feature


# Change the projection of the shapefiles (if necessary)

KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(KenyaSHP)


# 6) Clean the data, so that the counties match those in the shapefile

### Inspect the county names in the asbestoss dataset
table_1_select_county_unique <- unique(table_1_select_county$county)
table_1_select_county_unique

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(COUNTY) %>% 
  pull() %>%
  unique()

counties_KenyaSHP

### Convert the table_1_select_county county names to title case
table_1_select_county <- table_1_select_county %>% 
  ungroup() %>% 
  mutate(county = tools::toTitleCase(tolower(county)))

### Inspect the county names of the asbestos data again 
table_1_select_county_unique <- unique(table_1_select_county$county)


### Inspect the county names that are different in each of the datasets
unique(table_1_select_county$county)[which(!unique(table_1_select_county$county) %in% counties_KenyaSHP)]


table_1_select_county <- table_1_select_county %>% 
  mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
                         ifelse(county == "Tharaka-Nithi", "Tharaka",
                                ifelse(county == "Elgeyo/Marakwet", "Keiyo-Marakwet",
                                       ifelse(county == "Nairobi City", "Nairobi", county)))))

# Check again for unique datasets
unique(table_1_select_county$county)[which(!unique(table_1_select_county$county) %in% counties_KenyaSHP)]

# 7) Join the shapefile and the data

### Rename the COUNTY variable, to match the variable name in the shapefile data
table_1_select_county <- table_1_select_county %>% 
  rename(COUNTY = county)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
table_1_select_county$COUNTY <- trimws(table_1_select_county$COUNTY)

### Merge the data
merged_df <- left_join(KenyaSHP, table_1_select_county, by = "COUNTY")

### Sort the data so that the County variable appears first
merged_df <- merged_df %>% 
  select(COUNTY, everything())


# 8) Inspect the merged data

# View the data
View(merged_df)
View(merged_df %>% st_drop_geometry())

### Class of the merged data
class(merged_df)

### Column names
colnames(merged_df)

# Glimpse
glimpse(merged_df)



# 9) Visualize the data

#install.packages("ggbreak")
library(ggbreak)

library(patchwork)

barplot <- table_1_select_county %>%
  ggplot(aes(x = reorder(COUNTY, asbestos_sheets), y = asbestos_sheets, fill = asbestos_sheets)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  coord_flip() + 
  scale_fill_gradient(low = "darkred", high = "yellow") + 
  theme_classic()+
  labs(x = "County", 
       y = "Percentage(%) of households with asbestos-based roofs", 
       title = "",
       subtitle = "",
       caption = "",
       fill = "Percentage (%)\nof households")+
  theme(axis.title.x =element_text(size = 14),
        axis.title.y =element_text(size = 14),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("Helvetica",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12),
        panel.background = element_rect(fill = "white", colour = "white")) + 
  geom_hline(yintercept = 1.4, color = "black") +
  geom_text(aes(x = 20 , y = 1.4, label = "Average (Kenya)"), 
            size = 3, 
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 0.9, label = "Average (Rural)"), 
            size = 3,
            angle=90, vjust = 1.5) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "black") +
  geom_text(aes(x = 20 , y = 2.2, label = "Average (Urban)"), 
            size = 3,
            angle=90, vjust = 1.5)

barplot 


# Plot a base plot / map.

plot(KenyaSHP$geometry, lty = 5, col = "green")


#  ggplot2()

# Legend in map is silenced because the bar graph has one

map <- ggplot(data = merged_df)+
  geom_sf(aes(geometry = geometry, fill = asbestos_sheets))+
  theme_void()+
  labs(title = "",
       caption = "Visualization @willyokech | Source:rKenyaCensus",
       fill = "")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "darkred", high = "yellow")

map

# Save the plot

barplot + map


# 3) Subcounty

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_1 <- V4_T2.12
View(df_1)

# 4) Preliminary filtering and cleanup

# Subcounty table
table_2_sc <- df_1[4:395,] %>%
  filter(AdminArea != "County")

# Remove unnecessary columns
table_2_sc_new <- table_2_sc %>%
  select(County, SubCounty, AsbestosSheets, ConventionalHouseholds)

top_subcounty_raw <- table_2_sc_new %>%
  unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
  mutate(AffectedHouseholds = round((AsbestosSheets/100) * ConventionalHouseholds)) %>%
  arrange(desc(AffectedHouseholds)) %>%
  slice(1:10)

View(top_subcounty_raw)

top_subcounty_raw_plot <- top_subcounty_raw %>%
  ggplot(aes(x = reorder(county_sub, AffectedHouseholds), y = AffectedHouseholds)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "darkorange") + 
  coord_flip() + 
  theme_classic()+
  labs(x = "Subcounty", 
       y = "Number of households with asbestos-based roofs", 
       title = "",
       caption = "Visualization @willyokech | Source:rKenyaCensus") +
  theme(axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15),
        plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
        legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12)) 

top_subcounty_raw_plot
