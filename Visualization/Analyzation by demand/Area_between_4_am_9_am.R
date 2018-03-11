
unique(uber_weekdays_hrs$Area)
unique(non_uber_weekdays_hrs$Area)

# Merge individual counties in New Jersey and Connecticut 
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub(" County","",Area))
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub("Mercer","New Jersey",Area))
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub("Monmouth","New Jersey",Area))
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub("Hunterdon","New Jersey",Area))
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub("Sussex","New Jersey",Area))
uber_weekdays_hrs <- uber_weekdays_hrs %>% mutate(Area = gsub("New Haven","Connecticut",Area))

#########################################################################################################
# Group by hour for Uber and Non-Uber rides to calulate group totals
groups <- non_uber_weekdays_hrs %>% filter(hour %in% c("04","05","06","07","08")) %>% group_by(hour) %>% tally()
groups1 <- uber_weekdays_hrs %>% filter(hour %in% c("04","05","06","07","08")) %>% group_by(hour) %>% tally()

#Group by hour and Area for non-Uber and Uber rides
non_uber_4_9_am <- non_uber_weekdays_hrs %>% filter(hour %in% c("04","05","06","07","08")) %>% group_by(Area,hour) %>% tally() 
uber_4_9_am <- uber_weekdays_hrs %>% filter(hour %in% c("04","05","06","07","08")) %>% group_by(Area,hour) %>% tally() 

#Apply group total values calculated previously to the  Uber and non_uber group by hour and area DFs.
non_uber_4_9_am <- merge(non_uber_4_9_am,groups,by = "hour")
uber_4_9_am <- merge(uber_4_9_am,groups1,by = "hour")

#Rename grouped column from nn to Group_by_hour, this column is basically the group totals for each hour
non_uber_4_9_am <- rename(non_uber_4_9_am,c("nn" = "Group_by_hour"))
uber_4_9_am <- rename(uber_4_9_am,c("nn" = "Group_by_hour"))


#Calculate percent values for each group which has been created by grouping hour and Area
non_uber_4_9_am <- non_uber_4_9_am %>% mutate(pct = round(n*100/Group_by_hour,2))
uber_4_9_am <- uber_4_9_am %>% mutate(pct = round(n*100/Group_by_hour,2))

#Pick top 5 areas for each group(based on hour) based on the heaviest percentages i.e the most popular areas in terms of demand
non_uber_4_9_am <- non_uber_4_9_am %>% group_by(hour) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 
uber_4_9_am <- uber_4_9_am %>% group_by(hour) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 

#Order the DFs by percentage and Area
non_uber_4_9_am <- with(non_uber_4_9_am, non_uber_4_9_am[order(pct,Area),])
uber_4_9_am <- with(uber_4_9_am, uber_4_9_am[order(pct,Area),])

#Add group field to identify the Uber and Non-Uber groups
non_uber_4_9_am$group <- "Non_uber"
uber_4_9_am$group <- "uber"
 

#Combine the uber and non_uber data into dataframe d

d <- rbind(non_uber_4_9_am,uber_4_9_am)
d <- with(d, d[order(pct, Area),])

#Barplot with stacked bars and grouped bar plots 
ggplot(data=d, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~hour)


