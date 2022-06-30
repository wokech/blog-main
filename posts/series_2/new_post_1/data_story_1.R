# Does Kenya have more men than women?
# By @willyokech
# Data: rKenyaCensus

# 1. The national human sex ratio

# Load the required libraries
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)
library(scales)

# Obtain the data required to plot the national human sex ratio
df_pop <- V1_T2.2
df_pop_national <- df_pop[1,]
df_pop_national_tidy <- df_pop_national %>%
pivot_longer(!County, names_to = "Gender", values_to = "Number")
df_pop_national_tidy <- df_pop_national_tidy
df_pop_national_tidy <- df_pop_national_tidy[1:2,]

# Plot of the national human sex ratio
ggplot(df_pop_national_tidy, aes(County, Number, fill = Gender)) +
geom_bar(position="stack", stat="identity", width = 2) +
coord_flip() +
theme_void() +
scale_fill_manual(values = c("yellow", "blue")) +
labs(x = "",
y = "",
title = "Figure 1: The number of males and females in Kenya",
subtitle = "",
caption = "Visualization: @willyokech | Source: rKenyaCensus",
fill = "") +
geom_text(aes(label=comma(Number)),color="black",size=10,position=position_stack(vjust=0.5)) +
theme(axis.title.x =element_text(size = 14),
axis.title.y =element_text(size = 14),
plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
legend.title = element_text("Helvetica",size = 8, vjust = 1),
legend.position = "right",
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10),
panel.background = element_rect(fill = "white", colour = "white"))

ggsave("posts/series_2/new_post_1/images/national_sex_ratio.png", width = 8, height = 2)

# 2. The human sex ratio by age

df_age_sex_ratio <- V3_T2.2
df_age_sex_ratio_filter <- df_age_sex_ratio %>%
filter(!grepl('0-|5-|100|Total|Not', Age))
df_age_sex_ratio_filter$Age <- as.numeric(as.character(df_age_sex_ratio_filter$Age))
df_age_sex_ratio_filter_select <- df_age_sex_ratio_filter %>%
mutate(m_f_ratio = round(Male *100/Female)) %>%
select(Age, m_f_ratio)

# Plot of the national human sex ratio by age
df_age_sex_ratio_plot <- df_age_sex_ratio_filter_select%>%
ggplot(aes(Age, m_f_ratio)) +
geom_line(color = "darkblue", size = 1.5) +
theme_classic() +
labs(x = "Age (yrs)",
y = "Number of males per 100 females",
title = "Figure 2; The human sex ratio as a function of age",
caption = "Visualization: @willyokech | Source: rKenyaCensus") +
theme(axis.title.x =element_text(size = 14),
axis.title.y =element_text(size = 14, vjust = 2),
axis.text.x =element_text(size = 14),
axis.text.y =element_text(size = 14),
plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10)) +
geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
geom_text(aes(x = 75 , y = 100, label = "Male:Female ratio = 1:1"), size = 4, angle=0, vjust = -1)
df_age_sex_ratio_plot

ggsave("posts/series_2/new_post_1/images/age_sex_ratio.png", width = 8, height = 6)


# 3. The county human sex ratio

# Obtain the required data
df_1 <- V1_T2.2

# Calculate the male:female ratio per 100
df_1_ratio <- df_1 %>%
mutate(m_f_ratio = Male/Female,
m_f_ratio_100 = round(m_f_ratio*100, 0))

# Keep the County, Total, and ratio columns
df_1_ratio_only <- df_1_ratio %>%
select(County, m_f_ratio_100, Total)

# Remove the "Total" row
df_1_ratio_only_county <- df_1_ratio %>%
select(County, m_f_ratio_100, Total) %>%
filter(County != "Total")

# Load the packages required for the maps

#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
# Load the shapefiles that are downloaded from online source
KenyaSHP <- read_sf("kenyan-counties/County.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)
# Change the projection of the shapefiles (if necessary)
KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

# Clean the data, so that the counties match those in the shapefile
# Inspect the county names in the male/female ratio dataset
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)
df_1_ratio_only_county_unique
# Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>%
st_drop_geometry() %>%
select(COUNTY) %>%
pull() %>%
unique()
# Convert the m_f_ratio county names to title case
df_1_ratio_only_county <- df_1_ratio_only_county %>%
ungroup() %>%
mutate(County = tools::toTitleCase(tolower(County)))
# Inspect the county names of the m_f_ratio data again
df_1_ratio_only_county_unique <- unique(df_1_ratio_only_county$County)
# Inspect the county names that are different in each of the datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]
df_1_ratio_only_county <- df_1_ratio_only_county %>%
mutate(County = ifelse(County == "Taita/Taveta", "Taita Taveta",
ifelse(County == "Tharaka-Nithi", "Tharaka",
ifelse(County == "Elgeyo/Marakwet", "Keiyo-Marakwet",
ifelse(County == "Nairobi City", "Nairobi", County)))))
# Check again for unique datasets
unique(df_1_ratio_only_county$County)[which(!unique(df_1_ratio_only_county$County) %in% counties_KenyaSHP)]

# Join the shapefile and the data
# Rename the COUNTY variable, to match the variable name in the shapefile data
df_1_ratio_only_county <- df_1_ratio_only_county %>%
rename(COUNTY = County)
# Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$COUNTY <- trimws(KenyaSHP$COUNTY)
df_1_ratio_only_county$COUNTY <- trimws(df_1_ratio_only_county$COUNTY)
# Merge the data
merged_df <- left_join(KenyaSHP, df_1_ratio_only_county, by = "COUNTY")
# Sort the data so that the County variable appears first
merged_df <- merged_df %>%
select(COUNTY, everything())

# Visualize the data
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)

barplot <- df_1_ratio_only_county %>%
ggplot(aes(x = reorder(COUNTY, m_f_ratio_100), y = m_f_ratio_100, fill = m_f_ratio_100)) +
geom_bar(stat = "identity", width = 0.5) +
coord_flip() +
scale_fill_gradient(low = "blue", high = "yellow") +
scale_y_break(c(7.5, 80)) +
theme_classic()+
labs(x = "County",
y = "Number of males per 100 females",
title = "",
subtitle = "",
caption = "",
fill = "Number of males\nper 100 females")+
theme(axis.title.x =element_text(size = 14),
axis.title.y =element_text(size = 14),
axis.text.x =element_text(size = 12),
axis.text.y =element_text(size = 12),
plot.title = element_text(family = "URW Palladio L, Italic",size = 14, hjust = 0.5),
plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
legend.title = element_text("URW Palladio L, Italic",size = 10, vjust = 1),
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10),
panel.background = element_rect(fill = "white", colour = "white")) +
geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
geom_text(aes(x = 14 , y = 100, label = "Male:Female ratio = 1:1"), size = 4, angle=90, vjust = 1.5)

# Map
# Legend in map is silenced because the bar graph has one
map <- ggplot(data = merged_df)+
geom_sf(aes(geometry = geometry, fill = m_f_ratio_100))+
theme_void()+
labs(title = "",
caption = "Visualization: @willyokech | Source: rKenyaCensus",
fill = "Number of males\nper 100 females")+
theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 20, hjust = 0.5),
legend.title = element_blank(),
legend.position = "none",
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10))+
scale_fill_gradient(low = "blue", high = "yellow")

barplot + map + plot_annotation(
title = "Figure 3: The human sex ratio in different counties",
theme = theme(plot.title = element_text(size = 26, hjust = 0.5)))

ggsave("posts/series_2/new_post_1/images/barplot_map.png", width = 12, height = 10)

# 4. The subcounty human sex ratio

# Load the required libraries
library(patchwork)
library(tidyverse)
library(ggbreak)

# Obtain the required subcounty sex census data and clean
df_2 <- V1_T2.5
df_2

# Calculate the male:female ratio per 100
df_2_ratio <- df_2 %>%
mutate(m_f_ratio = Male/Female,
m_f_ratio_100 = round(m_f_ratio*100, 0))

# Remove the "Total" row and include the "Subcounty"
df_2_ratio_subcounty <- df_2_ratio %>%
filter(AdminArea == "SubCounty") %>%
select(County, SubCounty, m_f_ratio_100, Total) %>%
filter(County != "Total")

# Find the top 10 subcounties
top_subcounty <- df_2_ratio_subcounty %>%
unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
arrange(desc(m_f_ratio_100)) %>%
slice(1:20)

top_subcounty_plot <- top_subcounty %>%
ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100)) +
geom_bar(stat = "identity", width = 0.5, fill = "lightblue") +
coord_flip() +
scale_y_break(c(25, 75)) +
scale_y_continuous(breaks=seq(0,160,20)) +
theme_classic()+
labs(x = "Sub-county",
y = "Number of males per 100 females",
title = "Top 20 Sub-counties",
caption = "") +
theme(axis.title.x =element_text(size = 14),
axis.title.y =element_text(size = 14),
axis.text.x =element_text(size = 12),
axis.text.y =element_text(size = 12),
plot.title = element_text(family = "URW Palladio L, Italic",size = 14, hjust = 0.5),
plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10)) +
geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
geom_text(aes(x = 5 , y = 100, label = "Male:Female ratio = 1:1"), size = 4, angle=90, vjust = -1.5)

# Find the bottom 10 subcounties
bottom_subcounty <- df_2_ratio_subcounty %>%
unite(col = "county_sub", c("SubCounty", "County"), sep = ", ", remove = TRUE) %>%
arrange(m_f_ratio_100) %>%
slice(1:20)

bottom_subcounty_plot <- bottom_subcounty %>%
ggplot(aes(x = reorder(county_sub, m_f_ratio_100), y = m_f_ratio_100)) +
geom_bar(stat = "identity", width = 0.5, fill = "blue") +
coord_flip() +
scale_y_break(c(25, 75)) +
scale_y_continuous(breaks=seq(0,120,20)) +
theme_classic()+
labs(x = "Sub-county",
y = "Number of males per 100 females",
title = "Bottom 20 Sub-counties",
caption = "Visualization: @willyokech | Source: rKenyaCensus") +
theme(axis.title.x =element_text(size = 14),
axis.title.y =element_text(size = 14),
axis.text.x =element_text(size = 12),
axis.text.y =element_text(size = 12),
plot.title = element_text(family = "URW Palladio L, Italic",size = 14, hjust = 0.5),
plot.subtitle = element_text(family = "URW Palladio L, Italic",size = 10, hjust = 0.5),
legend.title = element_text("URW Palladio L, Italic",size = 8, vjust = 1),
plot.caption = element_text(family = "URW Palladio L, Italic",size = 10)) +
geom_hline(yintercept = 100, linetype="dashed", color = "purple", size=0.5) +
geom_text(aes(x = 5 , y = 100, label = "Male:Female ratio = 1:1"), size = 4, angle=90, vjust = -1.5)

top_subcounty_plot + bottom_subcounty_plot + plot_annotation(
title = "Figure 4: The human sex ratio at the sub-county level",
theme = theme(plot.title = element_text(size = 26, hjust = 0.5)))

ggsave("posts/series_2/new_post_1/images/top_bottom_plot.png", width = 12, height = 8)

