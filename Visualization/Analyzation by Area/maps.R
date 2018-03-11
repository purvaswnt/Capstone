install.packages(c("maps", "mapdata"))

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
#########################################################################################################
# Create DF to import states

states <- map_data("state")
#Select states that are relevant here - NY, CT, NJ

north_east <- states %>% filter(region %in% c("new york","new jersey", "connecticut"))

#Plot map based on north_east DF
ggplot(data = north_east) + 
  geom_polygon(aes(x = long, y = lat,group = group), fill = "palegreen", color = "black") + coord_fixed(1.3)

#Create DF to import counties
counties <- map_data("county")

#Import counties in states in the north_east DF
north_east_counties <- counties %>% filter(region %in% c("new york","new jersey", "connecticut"))

#Plot basic map of the three staes in Northeast
ny_base <- ggplot(data = north_east, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ny_base + theme_nothing()

ny_base + theme_nothing() + 
  geom_polygon(data = north_east_counties, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

#Convert areas to lower case
Area_counts <- Area_counts %>% mutate(Area = tolower(Area))
Area_counts_Carmel <- Area_counts_Carmel %>% mutate(Area = tolower(Area))
Area_counts_Dial7 <- Area_counts_Dial7 %>% mutate(Area = tolower(Area))

#merge Non-Uber data with north east counties

merged_data <- merge(north_east_counties,Area_counts,by.x = "subregion", by.y = "Area")
merged_data_Carmel <- merge(north_east_counties,Area_counts_Carmel,by.x = "subregion", by.y = "Area")
merged_data_Dial7 <- merge(north_east_counties,Area_counts_Dial7,by.x = "subregion", by.y = "Area")
 
non_uber_data <- rbind(merged_data,merged_data_Carmel,merged_data_Dial7)

##############################################################################################################

non_uber_data <- distinct(non_uber_data)

##################################################################################################################

#Plot map of NY and data collected from Non-Uber companies with blue

eb1 <- ny_base + geom_polygon(data = non_uber_data, aes(fill = n), color = "blue") +
 # geom_polygon(data = merged_data_Carmel, aes(fill = n), color = "pink") +
  #geom_polygon(data = merged_data_Dial7, aes(fill = n), color = "orange") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() 

# Add the rainbow color gradient for the numbers across areas
eb2 <- eb1 + 
  scale_fill_gradientn(colours = rev(rainbow(5)),
                       breaks = c(2, 4, 10, 100, 1000, 10000),
                       trans = "log10")

#Zoom the map
eb2 + coord_fixed(xlim = c(-80, -72),  ylim = c(40, 45), ratio = 1.3)


#############################################################################################


#Convert areas to lower case
Area_counts_Uber_jul <- Area_counts_Uber_jul %>% mutate(Area = tolower(Area))
Area_counts_Uber_aug <- Area_counts_Uber_aug %>% mutate(Area = tolower(Area))
Area_counts_Uber_Sep <- Area_counts_Uber_Sep %>% mutate(Area = tolower(Area))

#merge Uber data with north east counties
uber_july <- merge(north_east_counties,Area_counts_Uber_jul,by.x = "subregion", by.y = "Area")
uber_aug <- merge(north_east_counties,Area_counts_Uber_aug,by.x = "subregion", by.y = "Area")
uber_sep <- merge(north_east_counties,Area_counts_Uber_Sep,by.x = "subregion", by.y = "Area")

uber_data <- rbind(uber_july,uber_aug,uber_sep)

uber_data <- distinct(uber_data)


#Plot map of NY and data collected from Uber companies with blue

u_eb1 <- ny_base + geom_polygon(data = uber_data, aes(fill = n), color = "blue") +
  # geom_polygon(data = merged_data_Carmel, aes(fill = n), color = "pink") +
  #geom_polygon(data = merged_data_Dial7, aes(fill = n), color = "orange") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() 

# Add the rainbow color gradient for the numbers across areas
u_eb2 <- u_eb1 + 
  scale_fill_gradientn(colours = rev(rainbow(5)),
                       breaks = c(2, 4, 10, 100, 1000, 10000),
                       trans = "log10")

#Zoom the map
u_eb2 + coord_fixed(xlim = c(-80, -72),  ylim = c(40, 45), ratio = 1.3)

########################################################################################
#Identify the areas where Uber is not servicing currently

non_uber_service <- merge(non_uber_data,uber_data,by = "subregion",all.x = "TRUE")
non_uber_service <- non_uber_service %>% filter(is.na(region.y))
unique(non_uber_service$subregion)
unique(non_uber_data$subregion)


