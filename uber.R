df_uber_Jul14 <- read.csv("uber-raw-data-jul14.csv", header = TRUE, stringsAsFactors = FALSE)

Lat_Values_Bracket <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)
#Lat_Values_Bracket <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)
Temp_Values <- data.frame(Lon_Min = NA,Lat_Min = NA,Addr_Min = NA,Lon_Max = NA,Lat_Max = NA,Addr_Max = NA)
#Temp_Values <- data.frame(Lon_Min = NA,Lat_Min = NA,Lon_Max = NA,Lat_Max = NA)

BracketMin <- data.frame(Lon = NA, Lat = NA)
BracketMax <- data.frame(Lon = NA, Lat = NA)

A1 <- data.frame(Addr1 = NA)
A2 <- data.frame(Addr2 = NA)

MinLat <- min(df_uber_Jul14$Lat)
MaxLat <- max(df_uber_Jul14$Lat)



#MinLat <- 39.9055

while (MinLat <= MaxLat){
  BracketMin <- df_uber_Jul14 %>% filter(between(Lat,MinLat,MinLat + 0.1000)) %>% arrange(Lat) %>% select(Lon, Lat) %>% 
                                  mutate(r = rank(Lat,ties.method = "first")) %>% filter(r == 1)
  BracketMax <- df_uber_Jul14 %>% filter(between(Lat,MinLat,MinLat + 0.1000)) %>% arrange(desc(Lat)) %>% select(Lon, Lat) %>% top_n(1) %>% 
                                  mutate(r = rank(Lat,ties.method = "first")) %>% filter(r == 1)
  
  A1 <- getAddrDetail(BracketMin$Lon, BracketMin$Lat)  
  A2 <- getAddrDetail(BracketMax$Lon, BracketMax$Lat)  
  
  t1 <- unlist(A1$results)
  t2 <- unlist(A2$results)
  
  Temp_Values <- c(BracketMin$Lon, BracketMin$Lat, t1["formatted_address"],
                   BracketMax$Lon, BracketMax$Lat, t2["formatted_address"]
                   )
  Lat_Values_Bracket <- rbind(Lat_Values_Bracket, Temp_Values)
  
  MinLat = MinLat + 0.1000
  
}

Lat_Values_Bracket




#rm("Lat_Values_Bracket", "Temp_Values", "A1","A2","BracketMin","BracketMax")
df_uber_Jul14 %>% filter(between(Lat,MinLat,MinLat + 0.1000))
df_uber_Jul14 %>% filter(Lat > 40.02)
