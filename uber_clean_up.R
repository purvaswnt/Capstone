# Read csv file into dataframe

df_uber_Jul14 <- read.csv("uber-raw-data-jul14.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate Min max values for longitude and latitude
Temp_Values <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
Lat_Values_Bracket <- data.frame(Lon_Min = NA,Lon_Max = NA,Lat = NA)
#####################################################################################


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

#rm("Lat_Values_Bracket","LatValue","Temp_Values")

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

########################################################################################
# Create dataframe which has look-ups of addresses

Temp_Values_Addr <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)
Final_Values_Bracket_addr <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)
A1 <- data.frame(Addr1 = NA)
A2 <- data.frame(Addr2 = NA)


for (ii in seq(1,nrow(Lat_Values_Bracket1))){
  
  A1 <- getAddrDetail(Lat_Values_Bracket1$Lon_Min[ii], Lat_Values_Bracket1$Lat_Min[ii])  
  A2 <- getAddrDetail(Lat_Values_Bracket1$Lon_Max[ii], Lat_Values_Bracket1$Lat_Max[ii])  
  
  t1 <- unlist(A1$results)
  t2 <- unlist(A2$results)
  
  Temp_Values_Addr <- c(Lat_Values_Bracket1$Lon_Min[ii], Lat_Values_Bracket1$Lat_Min[ii], case_when(A1$status == "ZERO_RESULTS" ~ "No Address found",
                                                                                                    A1$status == "OK" ~ t1["formatted_address"]),
                        Lat_Values_Bracket1$Lon_Max[ii], Lat_Values_Bracket1$Lat_Max[ii], case_when(A2$status == "ZERO RESULTS" ~ "No Address found",
                                                                                                    A2$status == "OK" ~ t2["formatted_address"])
                       )
  Final_Values_Bracket_addr <- rbind(Final_Values_Bracket_addr, Temp_Values_Addr)
  

  saveRDS(Final_Values_Bracket_addr, "temp_revgeocoded_July.RDS")
  
}

#rm("Final_Values_Bracket_addr","Temp_Values_Addr")
write.csv(Lat_Values_Bracket1, file = "MyData.csv")


Geo_coded <- read.csv("Upload-MyData_geocodio_July.csv", header = TRUE, stringsAsFactors = FALSE)
Final_Values_Bracket_addr <- Final_Values_Bracket_addr %>% filter(duplicated(Final_Values_Bracket_addr) == FALSE)
Final_Values_Bracket_addr <- Final_Values_Bracket_addr %>% filter(!is.na(Addr_Max))

 test <- Lat_Values_Bracket1
test$Lon_Min[1]
test$Addr_Min <- NA
test$Addr_Max <- NA
test$Area_Min <- NA
test$Area_Max <- NA


for (ii in seq(1, nrow(na.omit(Final_Values_Bracket_addr)))){
  
  for (jj in seq(1, nrow(test))){
    
           if(Final_Values_Bracket_addr$Lat_Min[ii] == test$Lat_Min[jj] && Final_Values_Bracket_addr$Lon_Min[ii] == test$Lon_Min[jj]){
             
             test$Addr_Min[jj] = Final_Values_Bracket_addr$Addr_Min[ii]
           }
  }
  
}

Geo_coded <- Geo_coded %>% select(-Accuracy.Score,-Accuracy.Type,-Number,-Street,-Country,-Source)
##################################################################################################################
for (ii in seq(1, nrow(na.omit(Final_Values_Bracket_addr)))){
  
  for (jj in seq(1, nrow(test))){
    
    if(Final_Values_Bracket_addr$Lat_Max[ii] == test$Lat_Max[jj] && Final_Values_Bracket_addr$Lon_Max[ii] == test$Lon_Max[jj]){
      
      test$Addr_Max[jj] = Final_Values_Bracket_addr$Addr_Max[ii]
    }
  }
  
}
##################################################################################################################


for (ii in seq(1, nrow(test))){
  
  for (jj in seq(1, nrow(Geo_coded))){
    
    if(Geo_coded$Latitude[jj] == test$Lat_Min[ii] && Geo_coded$Longitude[jj] == test$Lon_Min[ii]){
      
      test$Addr_Min[ii] = Geo_coded$County[jj]
    }
  }
  
}

for (ii in seq(1, nrow(test))){
  
  for (jj in seq(1, nrow(Geo_coded))){
    
    if(Geo_coded$Latitude[jj] == test$Lat_Max[ii] && Geo_coded$Longitude[jj] == test$Lon_Max[ii]){
      
      test$Addr_Max[ii] = Geo_coded$County[jj]
    }
  }
  
}


#####################################################################################################################

test <- test %>% select(-Area)

test <- test %>% mutate(Area_Min = replace(Area_Min,grepl("CT",Addr_Min) == TRUE,"Connecticut"))
test <- test %>% mutate(Area_Max = replace(Area_Max,grepl("CT",Addr_Max) == TRUE,"Connecticut"))

test <- test %>% mutate(Area_Min = replace(Area_Min,grepl("NJ",Addr_Min) == TRUE,"New Jersey"))
test <- test %>% mutate(Area_Max = replace(Area_Max,grepl("NJ",Addr_Max) == TRUE,"New Jersey"))

##########################################################################################################

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  test <- test %>% mutate(Area_Min = replace(Area_Min,grepl(df_Area_Mapping_NY$Town[ii], Addr_Min,ignore.case = TRUE) == TRUE, 
                                                 df_Area_Mapping_NY$Area[ii]))
}

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  test <- test %>% mutate(Area_Max = replace(Area_Max,grepl(df_Area_Mapping_NY$Town[ii], Addr_Max,ignore.case = TRUE) == TRUE, 
                                         df_Area_Mapping_NY$Area[ii]))
}
################################################################################################################

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY_City)))){
  test <- test %>% mutate(Area_Min = replace(Area_Min,grepl(df_Area_Mapping_NY_City$Town[ii], Addr_Min,ignore.case = TRUE) == TRUE, 
                                             df_Area_Mapping_NY_City$Area[ii]))
}

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY_City)))){
  test <- test %>% mutate(Area_Max = replace(Area_Max,grepl(df_Area_Mapping_NY_City$Town[ii], Addr_Max,ignore.case = TRUE) == TRUE, 
                                             df_Area_Mapping_NY_City$Area[ii]))
}
######################################################################################################################


test %>% mutate(Addr_Min = replace(Addr_Min,(!is.na(Area_Min) & is.na(Area_Max)) == TRUE,Addr_Max))

for(ii in seq(1,nrow(test))){
  
if(!is.na(test$Area_Min[ii]) & is.na(test$Area_Max[ii])){
  test$Area_Max[ii] <- test$Area_Min[ii]
}
  if(is.na(test$Area_Min) & !is.na(test$Area_Max)){
    test$Area_Min[ii] <- test$Area_Max[ii]
  }
}


#########################################################################################################################
test$Area_Min[4:10] <- "New Jersey"
test$Area_Max[4:10] <- "New Jersey"
test$Area_Min[14:20] <- "New Jersey"
test$Area_Max[14:20] <- "New Jersey"
test$Area_Min[24] <- "New Jersey"
test$Area_Max[24] <- "Brooklyn"
test$Area_Min[25] <- "Suffolk"

test$Area_Min[42] <- "New Jersey"
test$Area_Max[42] <- "New Jersey"

test$Area_Max[25:30] <- "Suffolk"
test$Area_Min[25:30] <- "Suffolk"



test$Area_Max[37:40] <- "Suffolk"
test$Area_Min[37:40] <- "Suffolk"


test$Area_Max[47] <- "Connecticut"
test$Area_Min[47] <- "Suffolk"

test$Area_Max[56:57] <- "Orange"
test$Area_Min[56:57] <- "Orange"


test$Area_Max[59] <- "Connecticut"
test$Area_Min[59] <- "Connecticut"

test[55:58,]
############################################################################################################

df_uber_Jul14$Area <- NA
test$Lat_Avg <- (test$Lat_Min + test$Lat_Max)/2
test$Lon_Avg <- (test$Lon_Min + test$Lon_Max)/2
nrow(df_uber_Jul14)

for(ii in seq(40000,50000)){
 
   for(jj in seq(1,nrow(test))){
      
       if(df_uber_Jul14$Lat[ii] >= test$Lat_Min[jj] & df_uber_Jul14$Lat[ii] < test$Lat_Max[jj]){
         
          if(df_uber_Jul14$Lon[ii] >= test$Lon_Min[jj] & df_uber_Jul14$Lon[ii] < test$Lon_Max[jj]){
            df_uber_Jul14$Area[ii] <- case_when(df_uber_Jul14$Lon[ii] <= test$Lon_Avg[jj] ~ test$Area_Min,
                                                df_uber_Jul14$Lon[ii] > test$Lon_Avg[jj] ~ test$Area_Max)
          }
         
       }
  }
  
}

unique(df_uber_Jul14$Area)
40.7586 -73.9706



test %>% filter(40.7586 >=Lat_Min & 40.7586 < Lat_Max) %>% filter(-73.9706 >=Lon_Min & -73.9706 < Lon_Max)
