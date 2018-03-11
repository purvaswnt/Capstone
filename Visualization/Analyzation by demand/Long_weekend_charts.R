#Add columns which indicate the month and day of the month

df_uber_Sep14 <- df_uber_Sep14 %>% mutate(day = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%d")) %>% 
mutate(month = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%m"))


df_uber_Aug14 <- df_uber_Aug14 %>% mutate(day = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%d")) %>% 
  mutate(month = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%m"))

df_uber_Jul14 <- df_uber_Jul14 %>% mutate(day = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%d")) %>% 
  mutate(month = format(as.Date(Date.Time,format = "%m/%d/%Y"),"%m"))

################################################################################################################
#Add columns which indicate the month and day of the month

df_American <- df_American %>% mutate(day = format(as.Date(DateTime,format = "%m/%d/%Y"),"%d")) %>% 
  mutate(month = format(as.Date(DateTime,format = "%m/%d/%Y"),"%m"))


df_Carmel <- df_Carmel %>% mutate(day = format(as.Date(DateTime,format = "%m/%d/%Y"),"%d")) %>% 
  mutate(month = format(as.Date(DateTime,format = "%m/%d/%Y"),"%m"))

df_Dial7 <- df_Dial7 %>% mutate(day = format(as.Date(DateTime,format = "%Y.%m.%d"),"%d")) %>% 
  mutate(month = format(as.Date(DateTime,format = "%Y.%m.%d"),"%m"))
################################################################################################################
#Filter and combine all long weekend days for Uber

long_weekend <- rbind(df_uber_Jul14 %>% filter(day %in% c("03","04","05","06")),
df_uber_Sep14 %>% filter(day %in% c("01","02")),
df_uber_Aug14 %>% filter(day %in% c("29","30","31")))

#Add column day_name which will indicate the day and the month
long_weekend <- long_weekend %>% mutate(day_name = paste(case_when(month == "07" ~ "July",month == "08" ~ "Aug",month == "09" ~ "Sep"),"-",day)) 

#Group by day_name and Area
Long_weekend_days <- long_weekend %>% group_by(day_name,Area) %>% tally()

#Create DF which has totals of every group(done by day_name)
group5 <- long_weekend %>% group_by(day_name) %>% tally()

#Add the group total to the long weekends DF for Uber
uber_long_weekend <- merge(Long_weekend_days,group5,by = "day_name")

#Rename column with group totals to Group_by_day
uber_long_weekend <- rename(uber_long_weekend,c("n.y" = "Group_by_day","n.x" = "n"))

#Calculate percenatge per group
uber_long_weekend <- uber_long_weekend %>% mutate(pct = round(n*100/Group_by_day,2))


################################################################################################################
#Filter and combine all long weekend days for Non-Uber companies

long_weekend2 <- rbind(df_Carmel %>% filter(day %in% c("03","04","05","06") & month == "07") %>% select(Area,Week_Day,hour,day,month),
                       df_Carmel %>% filter(day %in% c("01","02") & month == "09")  %>% select(Area,Week_Day,hour,day,month),
                       df_Carmel %>% filter(day %in% c("29","30","31") & month == "08") %>% select(Area,Week_Day,hour,day,month))

long_weekend2 <- rbind(long_weekend2,df_American %>% filter(day %in% c("03","04","05","06") & month == "07") %>% select(Area,Week_Day,hour,day,month),
                       df_American %>% filter(day %in% c("01","02") & month == "09")  %>% select(Area,Week_Day,hour,day,month),
                       df_American %>% filter(day %in% c("29","30","31") & month == "08") %>% select(Area,Week_Day,hour,day,month))

long_weekend2 <- rbind(long_weekend2,df_Dial7 %>% filter(day %in% c("03","04","05","06") & month == "07") %>% select(Area,Week_Day,hour,day,month),
                       df_Dial7 %>% filter(day %in% c("01","02") & month == "09")  %>% select(Area,Week_Day,hour,day,month),
                       df_Dial7 %>% filter(day %in% c("29","30","31") & month == "08") %>% select(Area,Week_Day,hour,day,month))


#Combine the long weekend data for non Uber companies
long_weekend2 <- long_weekend2 %>% mutate(day_name = paste(case_when(month == "07" ~ "July",month == "08" ~ "Aug",month == "09" ~ "Sep"),"-",day)) 

#Create DF which has totals of every group(done by day_name)
non_uber_Long_weekend_days <- long_weekend2 %>% group_by(day_name,Area) %>% tally()

#Create DF which has totals of every group(done by day_name)
group6 <- long_weekend2 %>% group_by(day_name) %>% tally()

#Add the group total to the long weekends DF for Non-Uber
non_uber_long_weekend <- merge(non_uber_Long_weekend_days,group6,by = "day_name")

#Rename column with group totals to Group_by_day
non_uber_long_weekend <- rename(non_uber_long_weekend,c("n.y" = "Group_by_day","n.x" = "n"))

#Calculate percenatge per group
non_uber_long_weekend <- non_uber_long_weekend %>% mutate(pct = round(n*100/Group_by_day,2))

#Select top 5 areas ordered by highest percentages for both Uber and Non-Uber
non_uber_long_weekend <- non_uber_long_weekend %>% group_by(day_name) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 
uber_long_weekend <- uber_long_weekend %>% group_by(day_name) %>% arrange(desc(pct)) %>% top_n(5,wt = pct) 

#Add group labels
non_uber_long_weekend$group <- "Non_uber"
uber_long_weekend$group <- "uber"


#Excluded 2ns Sept since it was not a part of the long weekend
d2 <- d2 %>% filter(day_name != "Sep - 02")

#Combine Uber and Non-Uber data
d2 <- rbind(non_uber_long_weekend,uber_long_weekend)

#Remove county appended to areas just to normalize all data
d2 <- d2 %>% mutate(Area = gsub(" County","",Area))

#Convert all areas to lower case
d2 <- d2 %>% mutate(Area = tolower(Area))

#Order by Area and percentage
d2 <- with(d2, d2[order(pct, Area),])
ggplot(data=d2, aes(x=Area, y=pct, fill=group)) +  
  theme(axis.text.x = element_text(face="bold", color="#993333", size=6, angle=90)) +
  geom_bar(stat="identity") + 
  facet_grid(~day_name)






  