# Count of weekdays from July - September for Uber
uber_weekdays_July <- df_uber_Jul14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

uber_weekdays_Aug <- df_uber_Aug14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

uber_weekdays_Sep <- df_uber_Sep14 %>% mutate(day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

#Combine datasets for July, Aug, Sep for Uber and aggregate the combined dataset by the weekday
uber_weekdays <- rbind(uber_weekdays_July,uber_weekdays_Aug,uber_weekdays_Sep)
uber_weekdays <- uber_weekdays %>% group_by(day) %>% tally() 


# Pie Chart with Percentages

slices <- uber_weekdays$nn
lbls <- uber_weekdays$day
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct,"%")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of weekdays for Uber")
############################################################################################################
# Count of weekdays for Non-Uber rides

american_weekdays <- df_American %>% mutate(day = weekdays(as.Date(DateTime,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

Carmel_weekdays <- df_Carmel %>% mutate(day = weekdays(as.Date(DateTime,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

Dial7_weekdays <- df_Dial7 %>% mutate(day = weekdays(as.Date(DateTime,format = "%Y.%m.%d"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

#Combine datasets for Non-Uber data and aggregate the combined dataset by the weekday
non_uber_weekdays <- rbind(american_weekdays,Carmel_weekdays,Dial7_weekdays)
non_uber_weekdays <- non_uber_weekdays %>% group_by(day) %>% tally() 

#Pie chart
non_uber_slices <- non_uber_weekdays$nn
non_uber_lbls <- non_uber_weekdays$day
non_uber_pct <- round(non_uber_slices/sum(non_uber_slices)*100)
non_uber_lbls <- paste(non_uber_lbls,non_uber_pct,"%")
pie(non_uber_slices,labels = non_uber_lbls, col=rainbow(length(non_uber_lbls)),
    main="Pie Chart of weekdays for Non Uber companies")

