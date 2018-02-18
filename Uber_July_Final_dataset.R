# Read csv file into dataframe

df_uber_Jul14 <- read.csv("uber-raw-data-jul14.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate Min max values for longitude and latitude
Temp_Values <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
Lat_Values_Bracket <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)

# Find Min and Max values for the July dataset
BracketMin <- data.frame(Lon = NA)
BracketMax <- data.frame(Lon = NA)
LatValue <- data.frame(Lat = NA)

MinLat <- min(df_uber_Jul14$Lat)
MaxLat <- max(df_uber_Jul14$Lat)

MinLon <- min(df_uber_Jul14$Lon)
MaxLon <- max(df_uber_Jul14$Lon)

#Calculate and store Min Max Lon, Lat values based on increment

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

#Create dataframes to hold Min Max Bracket values
Temp_Values1 <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)
Lat_Values_Bracket1 <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)

# arrange Lat and Lon values in ascending order to ceate brackets
cnt_Lat <- sort(unique(Lat_Values_Bracket$Lat))
cnt_Lon <- sort(unique(Lat_Values_Bracket$Lon_Min))

# Create dataframe with various brackets of lon, Lat

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

Geo_coded <- read.csv("Upload-MyData_geocodio_July.csv", header = TRUE, stringsAsFactors = FALSE)
Geo_coded <- Geo_coded %>% select(-Accuracy.Score,-Accuracy.Type,-Number,-Street,-Country,-Source)

##################################################################################################

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

Lat_Values_Bracket1$Addr_Min <- NA
Lat_Values_Bracket1$Addr_Max <- NA

##################################################################################################################


for (ii in seq(1, nrow(Geo_coded))){
  
  while(jj <= nrow(Lat_Values_Bracket1)){
    
    if(as.character(Geo_coded$Latitude[ii]) == as.character(Lat_Values_Bracket1$Lat_Min[jj]) & as.character(Geo_coded$Longitude[ii]) == as.character(Lat_Values_Bracket1$Lon_Min[jj])){
      
      Lat_Values_Bracket1$Addr_Min[jj] <- Geo_coded$County[ii]
      
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


df_uber_Jul14$Area <- NA
Lat_Values_Bracket1$Lat_Avg <- (test$Lat_Min + test$Lat_Max)/2
Lat_Values_Bracket1$Lon_Avg <- (test$Lon_Min + test$Lon_Max)/2

########################################################################################################################

nrow(df_uber_Jul14)

for(ii in seq(700000,800000)){
  
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

saveRDS(df_uber_Jul14, file = "df_uber_Jul14.rds")
testing <- readRDS("df_uber_Jul14.rds")
#####################################################################################################
head(df_uber_Jul14)

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

weekdays(as.Date(df_uber_Jul14$Date.Time,format = "%m/%d/%Y"), abbreviate = "F")

################################################################################################################


Time_frame <- as.POSIXct(df_uber_Jul14$Date.Time, tz="EST",format="%m/%d/%Y %I:%M:%S %p") + as.difftime(30*(0:47),units="mins")
cut(Time_frame, breaks="3 hours", labels=FALSE)

as.POSIXlt(strptime(df_uber_Jul14$Date.Time, format="%Y-%m-%d %H:%M:%S"))
as.Date(df_uber_Jul14$Date.Time[345],format = "%m/%d/%Y")


plot(as.Date(df_uber_Jul14$Date.Time,format = "%m/%d/%Y"), range(df_uber_Jul14$Area), type="l",
     xlab="Date",ylab="Area")


