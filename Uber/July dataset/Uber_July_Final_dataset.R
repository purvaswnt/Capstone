#Load packages 

library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

#Set working directory
setwd("C:/Users/purva/Documents/Capstone")

# Read csv file into dataframe
df_uber_Jul14 <- read.csv("Uber/July Dataset/uber-raw-data-jul14.csv", header = TRUE, stringsAsFactors = FALSE)

# Create dataframes to calculate Min Max values for longitude and latitude brackets
Temp_Values <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
Lat_Values_Bracket <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)

# create dataframes for holding Bracket Min and Max Lon values. 
BracketMin <- data.frame(Lon = NA)
BracketMax <- data.frame(Lon = NA)
LatValue <- data.frame(Lat = NA)


# Find Min and Max values for the July dataset
MinLat <- min(df_uber_Jul14$Lat)
MaxLat <- max(df_uber_Jul14$Lat)

MinLon <- min(df_uber_Jul14$Lon)
MaxLon <- max(df_uber_Jul14$Lon)

#Increment longitude value starting from min by 0.3000 to create min and max value for longitude for every bracket. 
#Simultaneously increment the latitude values by 0.3000 to create increments for latitude. 


for(MinLat in seq(MinLat,MaxLat + 0.3000, by = 0.3000)){
  
  while(MinLon < MaxLon){
    
    BracketMin <- MinLon
    BracketMax <- MinLon + 0.3000
    LatValue <- MinLat
    
    Temp_Values <- c(BracketMin, 
                     BracketMax,
                     LatValue
    )
    Lat_Values_Bracket <- rbind(Lat_Values_Bracket, Temp_Values)
    BracketMin <- BracketMax
    MinLon <- BracketMax
    
    if(MinLon > MaxLon){
      Temp_Values <- c(MinLon, 
                       MinLon + 0.3000,
                       LatValue
      )
      Lat_Values_Bracket <- rbind(Lat_Values_Bracket, Temp_Values)
    }
  }
  
  MinLon <- min(df_uber_Jul14$Lon)
}


#####################################################################################

unique(Lat_Values_Bracket$Lat)
unique(Lat_Values_Bracket$Lon_Min)
unique(Lat_Values_Bracket$Lon_Max)

#####################################################################################

#Eliminate NA from Lat_Values_Bracket
Lat_Values_Bracket <- Lat_Values_Bracket %>% filter(!is.na(Lat))
#####################################################################################

#Create dataframes to hold Min and Max values for longitudes and latitudes for every bracket
Temp_Values1 <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)
Lat_Values_Bracket1 <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)

# arrange Lat and Lon values in ascending order to ceate brackets
cnt_Lat <- sort(unique(Lat_Values_Bracket$Lat))
cnt_Lon <- sort(unique(Lat_Values_Bracket$Lon_Min))

# Create dataframe with brackets each containing a min and value for longitude and latitude.

for(ii in seq(1,length(cnt_Lat))){
  
  for(jj in seq(1,length(cnt_Lon))){
    
    Temp_Values1 <- c(cnt_Lon[jj],cnt_Lat[ii],
                      unique(Lat_Values_Bracket$Lon_Max)[jj],cnt_Lat[ii+1]
    )
    Lat_Values_Bracket1 <- rbind(Lat_Values_Bracket1, Temp_Values1)
    
  }
  
}

#rm("Lat_Values_Bracket1","Temp_Values1")


#####################################################################################

#Eliminate NA from Lat_Values_Bracket1
Lat_Values_Bracket1 <- Lat_Values_Bracket1 %>% filter(!is.na(Lat_Max))
#####################################################################################
#Write the latitude values to brackets to a csv file for external look-up

write.csv(Lat_Values_Bracket1, file = "Lat_Values_Bracket1_Aug.csv")
#####################################################################################
# Load looked-up areas into dataframe Geo_coded and elminiate unnecessary fields

Geo_coded <- read.csv("Uber\July dataset\Upload-MyData_geocodio_July.csv", header = TRUE, stringsAsFactors = FALSE)
Geo_coded <- Geo_coded %>% select(-Accuracy.Score,-Accuracy.Type,-Number,-Street,-Country,-Source)

##################################################################################################
#Update missing fields in Geo_coded dataframe

Geo_coded[14:20,]$City <- "Toms River"
Geo_coded[14:20,]$State <- "NJ"
Geo_coded[14:20,]$County <- "Ocean County"
Geo_coded[14:20,]$Zip <- "8755"

Geo_coded[3:11,]
Geo_coded[4:10,]$City <- "Manahawkin"
Geo_coded[4:10,]$State <- "NJ"
Geo_coded[4:10,]$County <- "Ocean County"
Geo_coded[4:10,]$Zip <- "8050"

Geo_coded[24:31,]
Geo_coded[25:30,]$City <- "Monmouth Beach"
Geo_coded[25:30,]$State <- "NJ"
Geo_coded[25:30,]$County <- "Monmouth County"
Geo_coded[25:30,]$Zip <- "7750"

Geo_coded[36:41,]
Geo_coded[37:40,]$City <- "Oak Beach"
Geo_coded[37:40,]$State <- "NY"
Geo_coded[37:40,]$County <- "Suffolk County"
Geo_coded[37:40,]$Zip <- "11702"

Geo_coded[62:71,]
Geo_coded[63:70,]$City <- "Toms River"
Geo_coded[63:70,]$State <- "NJ"
Geo_coded[63:70,]$County <- "Ocean County"
Geo_coded[63:70,]$Zip <- "8755"

Geo_coded[73:81,]
Geo_coded[74:80,]$City <- "Monmouth Beach"
Geo_coded[74:80,]$State <- "NJ"
Geo_coded[74:80,]$County <- "Monmouth County"
Geo_coded[74:80,]$Zip <- "7750"

Geo_coded[85:91,]
Geo_coded[86:90,]$City <- "Oak Beach"
Geo_coded[86:90,]$State <- "NY"
Geo_coded[86:90,]$County <- "Suffolk County"
Geo_coded[86:90,]$Zip <- "11702"

Geo_coded[Geo_coded$City == "Yonkers",]$County <- "Westchester"
Geo_coded[Geo_coded$County == "",]$County <- "Black Point"

Geo_coded[99:101,]
Geo_coded[100,]$City <- "Pantigo"
Geo_coded[100,]$State <- "NY"
Geo_coded[100,]$County <- "Suffolk County"
Geo_coded[100,]$Zip <- "11937"

######################################################################################################
#Add Addrress fields to the Lat_Values_Bracket1 dataframe corresponding to Min Lat,Lon values and Max Lat,Lon values
Lat_Values_Bracket1$Addr_Min <- NA
Lat_Values_Bracket1$Addr_Max <- NA

##################################################################################################################
#Replace Address Min value in Lat_values_Bracket1 based on Geo_coded dataframe

for (ii in seq(1, nrow(Geo_coded))){
  
  while(jj <= nrow(Lat_Values_Bracket1)){
    
    if(as.character(Geo_coded$Latitude[ii]) == as.character(Lat_Values_Bracket1$Lat_Min[jj]) & as.character(Geo_coded$Longitude[ii]) == as.character(Lat_Values_Bracket1$Lon_Min[jj])){
      
      Lat_Values_Bracket1$Addr_Min[jj] <- Geo_coded$County[ii]
      
    }
    jj <- jj + 1
  }
  jj <- 1
}


#Replace Address Min value in Lat_values_Bracket1 based on Geo_coded dataframe
for (ii in seq(1, nrow(Geo_coded))){
  jj <- 1
  while(jj <= nrow(Lat_Values_Bracket1)){
    if(as.character(Lat_Values_Bracket1$Lat_Max[jj]) == as.character(Geo_coded$Latitude[ii]) & as.character(Lat_Values_Bracket1$Lon_Max[jj]) == as.character(Geo_coded$Longitude[ii])){
      
      Lat_Values_Bracket1$Addr_Max[jj] <- Geo_coded$County[ii]
    }
    
    jj <- jj + 1
    
  }
  
}


#####################################################################################################################
#Add Area column to the Uber data-set dataframe, and add average latitude and average longitude values for each bracket

df_uber_Jul14$Area <- NA
Lat_Values_Bracket1$Lat_Avg <- (test$Lat_Min + test$Lat_Max)/2
Lat_Values_Bracket1$Lon_Avg <- (test$Lon_Min + test$Lon_Max)/2

########################################################################################################################
#Replace Area field in the dataframe  for Uber, using brackets created in the Lat_Values_Bracket1. The logic here is that if 
# a latitude and longitude value falls between lat, lon Min and lat,lon average values range then the address Min value will be assigned to the Area in the 
#uber dataset, or else the address max value will be assigned



for(ii in seq(1,nrow(df_uber_Jul14))){
  
  for(jj in seq(1,nrow(Lat_Values_Bracket1))){
    
    if(df_uber_Jul14$Lat[ii] >= Lat_Values_Bracket1$Lat_Min[jj] & df_uber_Jul14$Lat[ii] < Lat_Values_Bracket1$Lat_Max[jj]){
      
      if(df_uber_Jul14$Lon[ii] >= Lat_Values_Bracket1$Lon_Min[jj] & df_uber_Jul14$Lon[ii] < Lat_Values_Bracket1$Lon_Max[jj]){
        df_uber_Jul14$Area[ii] <- case_when(df_uber_Jul14$Lon[ii] <= Lat_Values_Bracket1$Lon_Avg[jj] ~ test$Area_Min[jj],
                                            df_uber_Jul14$Lon[ii] > Lat_Values_Bracket1$Lon_Avg[jj] ~ test$Area_Max[jj])
      }
      
    }
  }
  
}

unique(df_uber_Jul14$Area) 

testing %>% filter(is.na(Area))

#Save the updated Uber dataset in a temp file to reload again.

saveRDS(df_uber_Jul14, file = "Uber/July dataset/df_uber_Jul14.rds")
df_uber_Jul14 <- readRDS("Uber/July dataset/df_uber_Jul14.rds")
#####################################################################################################
#Plotting graphs
# Bar plot by Area

ggplot(data = df_uber_Jul14) +
  geom_bar(mapping = aes(x = Area))

###############################################################################################################
# Bar plot by per day in July

ggplot(data = df_uber_Jul14) +
  geom_bar(mapping = aes(x = as.Date(df_uber_Jul14$Date.Time,format = "%m/%d/%Y")))

################################################################################################################
# Bar plot by Weekday for all weekdays in July

ggplot(data = df_uber_Jul14) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_uber_Jul14$Date.Time,format = "%m/%d/%Y"), abbreviate = "F")))

################################################################################################################

#Calcualting counts 

#Counts for each Area
Area_counts_Uber_jul <- df_uber_Jul14 %>% group_by(Area) %>% tally()

Area_counts_Uber_jul %>% arrange(desc(n))

Area_counts_Uber_jul <- Area_counts_Uber_jul %>% mutate(Area = tolower(Area))

#Total number of rides requested for all days
nrow(df_uber_Jul14)

# Count of weekdays from July
df_uber_Jul14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

# Count of individual days from July - September
df_uber_Jul14 %>% mutate(per_day = as.Date(df_uber_Jul14$Date.Time,format = "%m/%d/%Y")) %>% group_by(per_day) %>% tally() %>% arrange(desc(n))






