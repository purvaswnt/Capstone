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



