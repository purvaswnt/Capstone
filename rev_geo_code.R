address <- data.frame(Addr)
Addr <-  c("101 Western Ave, Cambridge, MA - 02139")

str(address)

geo_reply = geocode("101 Western Ave, Cambridge, MA - 02139", output='all', messaging=TRUE, override_limit=TRUE)
geo_reply
#now extract the bits that we need from the returned list
answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
answer$status <- geo_reply$status

geocode("Baylor University", output = "more")
geocode("101 Western Ave, Cambridge, MA - 02139", output = "all")

geocode("100 Quannapowitt Parkway, Wakefield, MA - 01880")
   test <- revgeocode(c(-71.08648,42.51842),output = "all")
   test1 <- revgeocode(c(-73.9549,40.7690),output = "all")
   
  bind_rows(test, test1)
   
   
   
   
   
               
               test$results[[1]]$formatted_address    
               test$results[[2]]$address_components[[2]]$long_name 
               test$results$address_components$address_components["long_name"]
                      
  names(test)
  str(test)
  test$locality
  
  test$results[[2]]$formatted_address
  test$status
  test$results
  
  
  testing <- unlist(test$results)
  testing["formatted_address"]
  testing["address_components.long_name"]
  testing[c("address_components.short_name","address_components.short_name")]
  
  unique(testing)
  
  res <- readRDS("temp_revgeocoded.rds")
  res


   
   
 
 