# revgeo code addresses for Uber data - Apr '14

getwd()
setwd(dir = "C:/Users/purva/Documents/Capstone")
df_uber_Apr14 <- read.csv("uber-raw-data-apr14.csv", header = TRUE, stringsAsFactors = FALSE)
test_df_uber <- head(df_uber_Apr14)
library(ggplot2)
library(ggmap)
library(dplyr)
library(plyr)
#revgeocode(c(df_uber_Apr14$Lon[1],df_uber_Apr14$Lat[1]))
#rev_code_fun <- function(x){
#  revgeocode(c(x$Lon,x$Lat))
#}

test_df_uber
test_df_uber$Lon[1]

revgeocode(c(test_df_uber$Lon[1],test_df_uber$Lat[1]), output = "all")

#test_df_uber$textAddress <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)), test_df_uber$Lon, test_df_uber$Lat)
#library(dplyr)
getAddrDetail <- function(lon,lat){
  
  geo_reply = mapply(FUN = function(lon, lat) revgeocode(c(lon, lat),output = "all"), test_df_uber$Lon, test_df_uber$Lat)
  
} 
  
  response <- data.frame(addr = NA)
  response$details <- geo_reply$details
  
  while(geo_reply[details] == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = mapply(FUN = function(lon, lat) revgeocode(c(lon, lat),output = "all"), test_df_uber$Lon, test_df_uber$Lat)
    response$details <- geo_reply$details
  }
test_df_uber$details <- response$details
return(test_df_uber)
  }

startindex <- 1
for (ii in seq(startindex, length(test_df_uber))){
  print(paste("Working on index", ii, "of", length(test_df_uber)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getAddrDetail(lon[ii],lat[ii]) 
  print(paste("Status",result$status))     
  result$index <- ii
  #append the answer to the results file.
  geo_reply <- rbind(geo_reply, result)
  #save temporary results as we are going along
  saveRDS(geo_reply, temp.RDS)
}

write.csv("Results.csv")

startindex <- 1
for (ii in seq(startindex, length(test_df_uber))){
  print(paste("Working on index", ii, "of", length(test_df_uber)))
  #query the google geocoder - this will pause here if we are over the limit.
  result <-  getAddrDetail(lon[ii],lat[ii]) 
  
  saveRDS(result, "temp.RDS")
}

res <- readRDS("temp.RDS")
str(res)


str(testing) <- revgeocode(c(-73.9549,40.7690), output = "all")
testing$status 

final_res <- data.frame()
startindex <- 1
for(i in seq(startindex, length(test_df_uber))){
  result <- revgeocode(c(test_df_uber$Lon[i],test_df_uber$Lat[i]), output = "all")
  
  saveRDS(result, "temp.RDS")
}

res <- readRDS("temp.RDS")

res$status