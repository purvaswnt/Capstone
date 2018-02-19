# Read csv file into dataframe
getwd()
setwd("C:/Users/purva/Documents/Capstone")
df_uber_Aug14 <- read.csv("uber-raw-data-aug14.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate Min max values for longitude and latitude
Temp_Values_Aug <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
Lat_Values_Bracket_Aug <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
#####################################################################################


# Find Min and Max values for the Aug dataset
BracketMin_Aug <- data.frame(Lon = NA)
BracketMax_Aug <- data.frame(Lon = NA)
LatValue_Aug <- data.frame(Lat = NA)

MinLat_Aug <- min(df_uber_Aug14$Lat)
MaxLat_Aug <- max(df_uber_Aug14$Lat)

MinLon_Aug <- min(df_uber_Aug14$Lon)
MaxLon_Aug <- max(df_uber_Aug14$Lon)

#Calculate and store Min Max Lon, Lat values based on increment

for(MinLat_Aug in seq(MinLat_Aug,MaxLat_Aug + 0.3000, by = 0.3000)){
  
  while(MinLon_Aug < MaxLon_Aug){
    
    BracketMin_Aug <- MinLon_Aug
    BracketMax_Aug <- MinLon_Aug + 0.3000
    LatValue_Aug <- MinLat_Aug
    
    Temp_Values_Aug <- c(BracketMin_Aug, 
                         BracketMax_Aug,
                         LatValue_Aug
    )
    Lat_Values_Bracket_Aug <- rbind(Lat_Values_Bracket_Aug, Temp_Values_Aug)
    BracketMin_Aug <- BracketMax_Aug
    MinLon_Aug <- BracketMax_Aug
    
    if(MinLon_Aug > MaxLon_Aug){
      Temp_Values_Aug <- c(MinLon_Aug, 
                           MinLon_Aug + 0.3000,
                           LatValue_Aug
      )
      Lat_Values_Bracket_Aug <- rbind(Lat_Values_Bracket_Aug, Temp_Values_Aug)
    }
  }
  
  MinLon_Aug <- min(df_uber_Aug14$Lon)
}

#rm("Lat_Values_Bracket_Aug","LatValue_Aug","Temp_Values_Aug")

#####################################################################################

unique(Lat_Values_Bracket_Aug$Lat)
unique(Lat_Values_Bracket_Aug$Lon_Min)
unique(Lat_Values_Bracket_Aug$Lon_Max)

#####################################################################################

#Eliminate NA from Lat_Values_Bracket
Lat_Values_Bracket_Aug <- Lat_Values_Bracket_Aug %>% filter(!is.na(Lat))
#####################################################################################

#Create dataframes to hold Min Max Bracket values
Temp_Values1_Aug <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)
Lat_Values_Bracket1_Aug <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)

# arrange Lat and Lon values in ascending order to ceate brackets
cnt_Lat_Aug <- sort(unique(Lat_Values_Bracket_Aug$Lat))
cnt_Lon_Aug <- sort(unique(Lat_Values_Bracket_Aug$Lon_Min))

# Create dataframe with various brackets of lon, Lat

for(ii in seq(1,length(cnt_Lat_Aug))){
  
  for(jj in seq(1,length(cnt_Lon_Aug))){
    
    Temp_Values1_Aug <- c(cnt_Lon_Aug[jj],cnt_Lat_Aug[ii],
                          unique(Lat_Values_Bracket_Aug$Lon_Max)[jj],cnt_Lat_Aug[ii+1]
    )
    Lat_Values_Bracket1_Aug <- rbind(Lat_Values_Bracket1_Aug, Temp_Values1_Aug)
    
  }
  
}

#rm("Lat_Values_Bracket1_Aug","Temp_Values1_Aug")


#####################################################################################

#Eliminate NA from Lat_Values_Bracket1
Lat_Values_Bracket1_Aug <- Lat_Values_Bracket1_Aug %>% filter(!is.na(Lat_Max))
#####################################################################################
#Write Lat_Values_Bracket1_Aug to csv
write.csv(Lat_Values_Bracket1_Aug, file = "Aug_Data.csv")


########################################################################################
# Create dataframe which has look-ups of addresses

Temp_Values_Addr_Aug <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)
Final_Values_Bracket_addr_Aug <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)


#####################################################################################


Geo_coded_Aug <- read.csv("Aug_Data_geocodio.csv", header = TRUE, stringsAsFactors = FALSE)
Geo_coded_Aug <- Geo_coded_Aug %>% select(-Accuracy.Score,-Accuracy.Type,-Number,-Street,-Country,-Source)

##################################################################################################


Lat_Values_Bracket1_Aug$Addr_Min <- NA
Lat_Values_Bracket1_Aug$Addr_Max <- NA

##################################################################################################################
#Replace Address Min field in Lat_Values_Bracket1_Aug based on the Geocoded Dataframe

for (ii in seq(1, nrow(Geo_coded_Aug))){
  
  while(jj <= nrow(Lat_Values_Bracket1_Aug)){
    
    if(as.character(Geo_coded_Aug$Latitude[ii]) == as.character(Lat_Values_Bracket1_Aug$Lat_Min[jj]) & as.character(Geo_coded_Aug$Longitude[ii]) == as.character(Lat_Values_Bracket1_Aug$Lon_Min[jj])){
      
      Lat_Values_Bracket1_Aug$Addr_Min[jj] <- Geo_coded_Aug$County[ii]
      
    }
    jj <- jj + 1
  }
  jj <- 1
}

#Replace Address Max field in Lat_Values_Bracket1_Aug based on the Geocoded Dataframe

for (ii in seq(1, nrow(Geo_coded_Aug))){
  jj <- 1
  while(jj <= nrow(Lat_Values_Bracket1_Aug)){
    if(as.character(Lat_Values_Bracket1_Aug$Lat_Max[jj]) == as.character(Geo_coded_Aug$Latitude[ii]) & as.character(Lat_Values_Bracket1_Aug$Lon_Max[jj]) == as.character(Geo_coded_Aug$Longitude[ii])){
      
      Lat_Values_Bracket1_Aug$Addr_Max[jj] <- Geo_coded_Aug$County[ii]
    }
    
    jj <- jj + 1
    
  }
  
}

#####################################################################################################################
Lat_Values_Bracket1_Aug$Addr_Min[1] <- "Atlantic County"

df_uber_Aug14$Area <- NA
Lat_Values_Bracket1_Aug$Lat_Avg <- (Lat_Values_Bracket1_Aug$Lat_Min + Lat_Values_Bracket1_Aug$Lat_Max)/2
Lat_Values_Bracket1_Aug$Lon_Avg <- (Lat_Values_Bracket1_Aug$Lon_Min + Lat_Values_Bracket1_Aug$Lon_Max)/2

########################################################################################################################

nrow(df_uber_Aug14)

for(ii in seq(800000,900000)){
  
  for(jj in seq(1,nrow(Lat_Values_Bracket1_Aug))){
    
    if(df_uber_Aug14$Lat[ii] >= Lat_Values_Bracket1_Aug$Lat_Min[jj] & df_uber_Aug14$Lat[ii] < Lat_Values_Bracket1_Aug$Lat_Max[jj]){
      
      if(df_uber_Aug14$Lon[ii] >= Lat_Values_Bracket1_Aug$Lon_Min[jj] & df_uber_Aug14$Lon[ii] < Lat_Values_Bracket1_Aug$Lon_Max[jj]){
        df_uber_Aug14$Area[ii] <- case_when(df_uber_Aug14$Lon[ii] <= Lat_Values_Bracket1_Aug$Lon_Avg[jj] ~ Lat_Values_Bracket1_Aug$Addr_Min[jj],
                                            df_uber_Aug14$Lon[ii] > Lat_Values_Bracket1_Aug$Lon_Avg[jj] ~ Lat_Values_Bracket1_Aug$Addr_Max[jj])
      }
      
    }
  }
  
}

unique(df_uber_Aug14$Area)
saveRDS(df_uber_Aug14, file = "df_uber_Aug14.rds")
testing_Aug <- readRDS("df_uber_Aug14.rds")

#####################################################################################################
head(df_uber_Aug14)
library(ggplot2)

# Bar plot by Area

ggplot(data = df_uber_Aug14) +
  geom_bar(mapping = aes(x = Area))

ggplot(data = df_uber_Aug14) +
  geom_bar(mapping = aes(x = Area, fill= Area)) + 
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45))

###############################################################################################################
# Bar plot by per day in August

ggplot(data = df_uber_Aug14) +
  geom_bar(mapping = aes(x = as.Date(df_uber_Aug14$Date.Time,format = "%m/%d/%Y")))

################################################################################################################
# Bar plot by Weekday for all weekdays in August

ggplot(data = df_uber_Aug14) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_uber_Aug14$Date.Time,format = "%m/%d/%Y"), abbreviate = "F")))

################################################################################################################

weekdays(as.Date(df_uber_Aug14$Date.Time,format = "%m/%d/%Y"), abbreviate = "F")

################################################################################################################


