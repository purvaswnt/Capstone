############################################################################
#Define the function
getAddrDetail <- function(lon,lat){
  
  geo_reply <- revgeocode(c(lon, lat),output = "all")
  response_details <- data.frame()
    response_details <- geo_reply
  
  while(geo_reply$status == "OVER_QUERY_LIMIT" && is.null(geo_reply$status)== FALSE ){
    print("OVER QUERY LIMIT - Pausing for 1 hour at") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply <- revgeocode(c(lon, lat),output = "all")
    response_details <- geo_reply
  }
  
  return(response_details)
}

############################################################################
response_details1 <- data.frame(Date_Time = NA,Lat = NA, Lon = NA, Base = NA, Formatted_address= NA)
output_df <- data.frame(Date_Time = NA,Lat = NA, Lon = NA, Base = NA, Formatted_address= NA)

startindex <- 1

for (ii in seq(startindex, length(test_df_uber))){
  print(paste("Working on index", ii, "of", length(test_df_uber)))
  #query the google geocoder - this will pause here if we are over the limit.
  result <-  getAddrDetail(test_df_uber$Lon[ii],test_df_uber$Lat[ii]) 
  counter <- ii
  t1 <- unlist(result$results)
  output_df <- c(test_df_uber$Date.Time[ii],test_df_uber$Lat[ii],test_df_uber$Lon[ii],test_df_uber$Base[ii],
                     t1["formatted_address"])
  response_details1 <- rbind(response_details1,output_df)
  
  saveRDS(response_details1, "temp_revgeocoded.RDS")
}
######################################################################################
## Run for actual dataset


response_details1 <- data.frame(Date_Time = NA,Lat = NA, Lon = NA, Base = NA, Formatted_address= NA)
output_df <- data.frame(Date_Time = NA,Lat = NA, Lon = NA, Base = NA, Formatted_address= NA)

startindex <- 1

for (ii in seq(startindex, nrow(na.omit(df_uber_Apr14)))){
  print(paste("Working on index", ii, "of", nrow(na.omit(df_uber_Apr14))))
  #query the google geocoder - this will pause here if we are over the limit.
  result <-  getAddrDetail(df_uber_Apr14$Lon[ii],df_uber_Apr14$Lat[ii]) 
  counter <- ii
  t1 <- unlist(result$results)
  output_df <- c(df_uber_Apr14$Date.Time[ii],df_uber_Apr14$Lat[ii],df_uber_Apr14$Lon[ii],df_uber_Apr14$Base[ii],
                 t1["formatted_address"])
  response_details1 <- rbind(response_details1,output_df)
  
  saveRDS(response_details1, "temp_revgeocoded.RDS")
}


nrow(na.omit(df_uber_Apr14))



write.csv("Results.csv")
res <- readRDS("temp_revgeocoded.rds")
str(res)
#rm("res", "output_df", "response_details1")"df_uber_Apr14","test_df_uber")
t1

revgeocode(c(-73.0344,39.5637))


