# Read csv file into dataframe

df_uber_Sep14 <- read.csv("uber-raw-data-sep14.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate Min max values for longitude and latitude
Temp_Values_Sep <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
Lat_Values_Bracket_Sep <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)

# Find Min and Max values for the Sep dataset
BracketMin_Sep <- data.frame(Lon = NA)
BracketMax_Sep <- data.frame(Lon = NA)
LatValue_Sep <- data.frame(Lat = NA)

MinLat_Sep <- min(df_uber_Sep14$Lat)
MaxLat_Sep <- max(df_uber_Sep14$Lat)

MinLon_Sep <- min(df_uber_Sep14$Lon)
MaxLon_Sep <- max(df_uber_Sep14$Lon)

#Calculate and store Min Max Lon, Lat values based on increment

for(MinLat_Sep in seq(MinLat_Sep,MaxLat_Sep + 0.3000, by = 0.3000)){
  
  while(MinLon_Sep < MaxLon_Sep){
    
    BracketMin_Sep <- MinLon_Sep
    BracketMax_Sep <- MinLon_Sep + 0.3000
    LatValue_Sep <- MinLat_Sep
    
    Temp_Values_Sep <- c(BracketMin_Sep, 
                     BracketMax_Sep,
                     LatValue_Sep
    )
    Lat_Values_Bracket_Sep <- rbind(Lat_Values_Bracket_Sep, Temp_Values_Sep)
    BracketMin_Sep <- BracketMax_Sep
    MinLon_Sep <- BracketMax_Sep
    
    if(MinLon_Sep > MaxLon_Sep){
      Temp_Values_Sep <- c(MinLon_Sep, 
                       MinLon_Sep + 0.3000,
                       LatValue_Sep
      )
      Lat_Values_Bracket_Sep <- rbind(Lat_Values_Bracket_Sep, Temp_Values_Sep)
    }
  }
  
  MinLon_Sep <- min(df_uber_Sep14$Lon)
}


#####################################################################################

unique(Lat_Values_Bracket_Sep$Lat)
unique(Lat_Values_Bracket_Sep$Lon_Min)
unique(Lat_Values_Bracket_Sep$Lon_Max)

#####################################################################################

#Eliminate NA from Lat_Values_Bracket
Lat_Values_Bracket_Sep <- Lat_Values_Bracket_Sep %>% filter(!is.na(Lat))
#####################################################################################

#Create dataframes to hold Min Max Bracket values
Temp_Values1_Sep <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)
Lat_Values_Bracket1_Sep <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)

# arrange Lat and Lon values in ascending order to ceate brackets
cnt_Lat_Sep <- sort(unique(Lat_Values_Bracket_Sep$Lat))
cnt_Lon_Sep <- sort(unique(Lat_Values_Bracket_Sep$Lon_Min))

# Create dataframe with various brackets of lon, Lat

for(ii in seq(1,length(cnt_Lat_Sep))){
  
  for(jj in seq(1,length(cnt_Lon_Sep))){
    
    Temp_Values1_Sep <- c(cnt_Lon_Sep[jj],cnt_Lat_Sep[ii],
                      unique(Lat_Values_Bracket_Sep$Lon_Max)[jj],cnt_Lat_Sep[ii+1]
    )
    Lat_Values_Bracket1_Sep <- rbind(Lat_Values_Bracket1_Sep, Temp_Values1_Sep)
    
  }
  
}

#rm("Lat_Values_Bracket1","Temp_Values1")


#####################################################################################

#Eliminate NA from Lat_Values_Bracket1
Lat_Values_Bracket1_Sep <- Lat_Values_Bracket1_Sep %>% filter(!is.na(Lat_Max))
#####################################################################################
write.csv(Lat_Values_Bracket1_Sep, file = "Upload-MyData_geocodio_Sep.csv")
#####################################################################################

Geo_coded_Sep <- read.csv("Upload-MyData_geocodio_Sep.csv", header = TRUE, stringsAsFactors = FALSE)
Geo_coded_Sep <- Geo_coded_Sep %>% select(-Accuracy.Score,-Accuracy.Type,-Number,-Street,-Country,-Source)

##################################################################################################


Lat_Values_Bracket1_Sep$Addr_Min <- NA
Lat_Values_Bracket1_Sep$Addr_Max <- NA

##################################################################################################################


for (ii in seq(1, nrow(Geo_coded_Sep))){
  
  while(jj <= nrow(Lat_Values_Bracket1_Sep)){
    
    if(as.character(Geo_coded_Sep$Latitude[ii]) == as.character(Lat_Values_Bracket1_Sep$Lat_Min[jj]) & as.character(Geo_coded_Sep$Longitude[ii]) == as.character(Lat_Values_Bracket1_Sep$Lon_Min[jj])){
      
      Lat_Values_Bracket1_Sep$Addr_Min[jj] <- Geo_coded_Sep$County[ii]
      
    }
    jj <- jj + 1
  }
  jj <- 1
}






match(Geo_coded$Latitude,Lat_Values_Bracket1$Lat_Min,0)


Geo_coded <- dplyr::distinct(Geo_coded)
Geo_coded %>% filter(Longitude == -71.826 & Latitude == 40.9214)
Geo_coded %>% filter(Latitude == 40.0214) %>% 
  Geo_coded$Latitude[75] %in% Lat_Values_Bracket1$Lat_Max & Geo_coded$Longitude[75] %in% Lat_Values_Bracket1$Lon_Max
Lat_Values_Bracket1 %>% filter(41.5214 %in% Lat_Max && -71.826 %in% as.character(Lon_Max))
Lat_Values_Bracket1 %>% filter(40.9214 == as.character(Lat_Max)) %>% filter(as.character(Lon_Max) == -71.826)





for (ii in seq(1, nrow(Geo_coded_Sep))){
  jj <- 1
  while(jj <= nrow(Lat_Values_Bracket1_Sep)){
    if(as.character(Lat_Values_Bracket1_Sep$Lat_Max[jj]) == as.character(Geo_coded_Sep$Latitude[ii]) & as.character(Lat_Values_Bracket1_Sep$Lon_Max[jj]) == as.character(Geo_coded_Sep$Longitude[ii])){
      
      Lat_Values_Bracket1_Sep$Addr_Max[jj] <- Geo_coded_Sep$County[ii]
    }
    
    jj <- jj + 1
    
  }
  
}


#####################################################################################################################


df_uber_Sep14$Area <- NA
Lat_Values_Bracket1_Sep$Lat_Avg <- (Lat_Values_Bracket1_Sep$Lat_Min + Lat_Values_Bracket1_Sep$Lat_Max)/2
Lat_Values_Bracket1_Sep$Lon_Avg <- (Lat_Values_Bracket1_Sep$Lon_Min + Lat_Values_Bracket1_Sep$Lon_Max)/2

########################################################################################################################

nrow(df_uber_Sep14)

for(ii in seq(800000,900000)){
  
  for(jj in seq(1,nrow(Lat_Values_Bracket1_Sep))){
    
    if(df_uber_Sep14$Lat[ii] >= Lat_Values_Bracket1_Sep$Lat_Min[jj] & df_uber_Sep14$Lat[ii] < Lat_Values_Bracket1_Sep$Lat_Max[jj]){
      
      if(df_uber_Sep14$Lon[ii] >= Lat_Values_Bracket1_Sep$Lon_Min[jj] & df_uber_Sep14$Lon[ii] < Lat_Values_Bracket1_Sep$Lon_Max[jj]){
        df_uber_Sep14$Area[ii] <- case_when(df_uber_Sep14$Lon[ii] <= Lat_Values_Bracket1_Sep$Lon_Avg[jj] ~ Lat_Values_Bracket1_Sep$Addr_Min[jj],
                                            df_uber_Sep14$Lon[ii] > Lat_Values_Bracket1_Sep$Lon_Avg[jj] ~ Lat_Values_Bracket1_Sep$Addr_Max[jj])
      }
      
    }
  }
  
}

#########################################################################################################################


df_uber_Sep14 %>%filter(is.na(Area))



saveRDS(df_uber_Sep14, file = "df_uber_Sep14.rds")
df_uber_Sep14 <- readRDS("df_uber_Sep14.rds")
#####################################################################################################

# Bar plot by Area

ggplot(data = df_uber_Sep14) +
  geom_bar(mapping = aes(x = Area))

ggplot(data = df_uber_Sep14) +
geom_bar(mapping = aes(x = Area, fill= Area)) + 
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45))

###############################################################################################################
# Bar plot by per day in Sep

ggplot(data = df_uber_Sep14) +
  geom_bar(mapping = aes(x = as.Date(df_uber_Sep14$Date.Time,format = "%m/%d/%Y")))

################################################################################################################
# Bar plot by Weekday for all weekdays in Sep

ggplot(data = df_uber_Sep14) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_uber_Sep14$Date.Time,format = "%m/%d/%Y"), abbreviate = "F")))

################################################################################################################


#Counts for each Area
Area_counts_Uber_Sep <- df_uber_Sep14 %>% group_by(Area) %>% tally()

Area_counts_Uber_Sep %>% arrange(desc(n))
Area_counts_Uber_Sep <- Area_counts_Uber_Sep %>% mutate(Area = tolower(Area))


#Average number of rides requested for all days(Total 92 days between July-Sep)
nrow(df_uber_Sep14)

# Count of weekdays from July - September
df_uber_Sep14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))


# Count of individual days from July - September
df_uber_Sep14 %>% mutate(per_day = as.Date(df_uber_Sep14$Date.Time,format = "%m/%d/%Y")) %>% group_by(per_day) %>% tally() %>% arrange(desc(n))

# Count of weekdays from July - September by Area
df_uber_Jul14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day,Area) %>% tally() %>% arrange(desc(n))





