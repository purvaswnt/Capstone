
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

non_uber_9_3_pm <- non_uber_weekdays_hrs %>% filter(hour %in% c("09","10","11","12","13","14","15")) %>% group_by(Area,hour) %>% tally() 
uber_9_3_pm <- uber_weekdays_hrs %>% filter(hour %in% c("09","10","11","12","13","14","15")) %>% group_by(Area,hour) %>% tally() 

#Group by hour and Area for non-Uber and Uber rides
groups2 <- non_uber_9_3_pm %>% group_by(hour) %>% tally()
groups3 <- uber_9_3_pm %>% group_by(hour) %>% tally()

#Apply group total values calculated previously to the  Uber and non_uber group by hour and area DFs.
non_uber_9_3_pm <- merge(non_uber_9_3_pm,groups2,by = "hour")
uber_9_3_pm <- merge(uber_9_3_pm,groups3,by = "hour")

#Rename grouped column from nn to Group_by_hour, this column is basically the group totals for each hour
non_uber_9_3_pm <- rename(non_uber_9_3_pm,c("nn" = "Group_by_hour"))
uber_9_3_pm <- rename(uber_9_3_pm,c("nn" = "Group_by_hour"))

#Calculate percent values for each group which has been created by grouping hour and Area
non_uber_9_3_pm <- non_uber_9_3_pm %>% mutate(pct = round(n*100/Group_by_hour,2))
uber_9_3_pm <- uber_9_3_pm %>% mutate(pct = round(n*100/Group_by_hour,2))

#Pick top 5 areas for each group(based on hour) based on the heaviest percentages i.e the most popular areas in terms of demand
non_uber_9_3_pm <- non_uber_9_3_pm %>% group_by(hour) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 
uber_9_3_pm <- uber_9_3_pm %>% group_by(hour) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 

#Order the DFs by percentage and Area
non_uber_9_3_pm <- with(non_uber_9_3_pm, non_uber_9_3_pm[order(pct,Area),])
uber_9_3_pm <- with(uber_9_3_pm, uber_9_3_pm[order(pct,Area),])


#Add group field to identify the Uber and Non-Uber groups
non_uber_9_3_pm$group <- "Non_uber"
uber_9_3_pm$group <- "uber"


#Combine the uber and non_uber data into dataframe d1
d1 <- rbind(non_uber_9_3_pm,uber_9_3_pm)
d1 <- with(d1, d1[order(pct, Area),])
ggplot(data=d1, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~hour)


