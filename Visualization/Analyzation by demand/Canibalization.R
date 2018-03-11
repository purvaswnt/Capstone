#Combine hour and Area for Non_Uber  and Uber DFs

non_uber_data <- rbind(df_Dial7 %>% select(Area,hour),df_Carmel %>% select(Area,hour), df_American %>% select(Area,hour))
uber_data <- rbind(df_uber_Jul14 %>% select(Area,hour),df_uber_Sep14 %>% select(Area,hour), df_uber_Aug14 %>% select(Area,hour))

#Convert Areas to lower case for both dataframes
non_uber_data <- non_uber_data %>% mutate(Area = tolower(Area))
uber_data <- uber_data %>% mutate(Area = tolower(Area))

#Group data by Area and hour
non_uber_common <- non_uber_data %>% group_by(Area,hour) %>% tally()

#Create groups by hour
gg <- non_uber_common %>% group_by(hour) %>% tally()


#Combine groups by Hour to the non_uber_common DF
non_uber_common <- merge(non_uber_common,gg,by = "hour")

#Calculate percentage of rides per hour
non_uber_common <- non_uber_common %>% mutate(non_uber_pct = round(n*100/nn,2))

#Group data by Area and hour
uber_common <- uber_data %>% group_by(Area,hour) %>% tally()

#Create groups by hour
gg1 <- uber_common %>% group_by(hour) %>% tally()

#Combine groups by Hour to the uber_common DF
uber_common <- merge(uber_common,gg1,by = "hour")

#Calculate percentage of rides per hour
uber_common <- uber_common %>% mutate(uber_pct = round(n*100/nn,2))


#Rename percent columns
colnames(uber_common)[colnames(uber_common)=="uber_pct"] <- "pct"
colnames(non_uber_common)[colnames(non_uber_common)=="non_uber_pct"] <- "pct"

#Add groups to indeitfy uber vs non-uber data
uber_common$group <- "Uber"
non_uber_common$group <- "Non-Uber"

#Eliminate county word from area
uber_common <- uber_common %>% mutate(Area = gsub(" county","",Area))

#Merge Uber and Non Uber data by Area to find common areas between the two datasets
common <- merge(uber_common,non_uber_common,by = "Area")

#Select unique common areas in the common DF
common <- unique(common$Area)

# convert above vector to DF
common <- as.data.frame(common)

#Name column to Area
names(common) <- c("Area")

#select areas from non Uber and Uber data sets which are common to both
uber_common <- merge(uber_common,common,by.x="Area",by.y = "Area")
non_uber_common <- merge(non_uber_common,common,by.x="Area",by.y = "Area")

#combine common areas into DF dd
dd <- rbind(uber_common,non_uber_common)

str(uber_common)
str(non_uber_common)
str(common)
###############################################################################################################
#Eliminate NA's from dd 
dd <- dd %>% filter(!is.na(Area)) 

#Create dataframes based on hours midnight to 8 am, 8am to 6 pm and 7 pm onwards
d_8am <- dd %>% mutate(hour = as.numeric(hour)) %>% filter(hour <= 8)
d_6pm <- dd %>% mutate(hour = as.numeric(hour)) %>% filter(hour >= 9 & hour< 18)
d_7pm <- dd %>% mutate(hour = as.numeric(hour)) %>% filter(hour >= 17)

#Create plot for hours from midnight to 8 am
d_8am <- with(d_8am, d_8am[order(pct, Area),])
ggplot(data=d_8am, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~hour)

d_6pm <- with(d_6pm, d_6pm[order(pct, Area),])
ggplot(data=d_6pm, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~hour)

d_7pm <- with(d_7pm, d_7pm[order(pct, Area),])
ggplot(data=d_7pm, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~hour)


