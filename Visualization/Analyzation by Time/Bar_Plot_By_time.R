library(knitr)
library(printr)
library(lubridate)

#Extract weekday and hour from the pick-up Date Time for Uber - Aug

df_uber_Aug14 <- df_uber_Aug14 %>% mutate(Week_Day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F"))
df_uber_Aug14 <- df_uber_Aug14 %>% mutate(hour = format(as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%S"),"%H"))

df_uber_Aug14 %>% group_by(hour) %>% tally() %>% arrange(desc(n))

#######################################################################################################################
#Extract weekday and hour from the pick-up Date Time for American

df_American <- df_American %>% mutate(Week_Day = weekdays(as.Date(DateTime,format = "%m/%d/%Y"), abbreviate = "F"))
df_American <- df_American %>% mutate(hour = format(strptime(DateTime, format = "%m/%d/%Y %I:%M:%S %p"),format = "%H"))


#######################################################################################################################
#Extract weekday and hour from the pick-up Date Time for Uber July

df_uber_Jul14 <- df_uber_Jul14 %>% mutate(Week_Day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F"))
df_uber_Jul14 <- df_uber_Jul14 %>% mutate(hour = format(as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%S"),"%H"))

#######################################################################################################################
#Extract weekday and hour from the pick-up Date Time for Uber Sep

df_uber_Sep14 <- df_uber_Sep14 %>% mutate(Week_Day = weekdays(as.Date(Date.Time,format = "%m/%d/%Y"), abbreviate = "F"))
df_uber_Sep14 <- df_uber_Sep14 %>% mutate(hour = format(as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%S"),"%H"))

#######################################################################################################################
#Extract weekday and hour from the pick-up Date Time for Carmel

df_Carmel <- df_Carmel %>% mutate(Week_Day = weekdays(as.Date(DateTime,format = "%m/%d/%Y"), abbreviate = "F"))
df_Carmel <- df_Carmel %>% mutate(hour = format(as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M"),"%H"))

#######################################################################################################################
#Extract weekday and hour from the pick-up Date Time for Dial7

df_Dial7 <- df_Dial7 %>% mutate(Week_Day = weekdays(as.Date(DateTime,format = "%Y.%m.%d"), abbreviate = "F"))
df_Dial7 <- df_Dial7 %>% mutate(hour = format(as.POSIXct(DateTime, format = "%Y.%m.%d %H:%M"),"%H"))


#######################################################################################################################
#Combine weekdays, hours and are for Non-Uber data

non_uber_weekdays_hrs <- df_Dial7 %>% select(DateTime,hour,Week_Day,Area)
non_uber_weekdays_hrs <- rbind(non_uber_weekdays_hrs,df_American %>% select(DateTime,hour,Week_Day,Area),
df_Carmel %>% select(DateTime,hour,Week_Day,Area))


nrow(non_uber_weekdays_hrs)

# group the non Uber data by hours
t1 <- non_uber_weekdays_hrs %>% group_by(hour) %>% tally() %>% select(n)
non_uber_weekdays_hrs %>% group_by(hour) %>% tally() %>% select(hour)

#Calcualte the percentage of rides across the total number of rides in the data-set
t1 <- t1 %>% mutate(pct = round(n*100/sum(n),2))


#######################################################################################################################
#Combine weekdays, hours and are for Uber data

uber_weekdays_hrs <- df_uber_Jul14 %>% select(Date.Time,hour,Week_Day,Area)
uber_weekdays_hrs <- rbind(uber_weekdays_hrs,df_uber_Aug14 %>% select(Date.Time,hour,Week_Day,Area),
                           df_uber_Sep14 %>% select(Date.Time,hour,Week_Day,Area))


nrow(uber_weekdays_hrs)

# group the Uber data by hours
t2 <- uber_weekdays_hrs %>% group_by(hour) %>% tally() 
uber_weekdays_hrs %>% group_by(hour) %>% tally() %>% select(hour)

#Calcualte the percentage of rides across the total number of rides in the data-set
t2 <- t2 %>% mutate(pct = round(n*100/sum(n),2))


#######################################################################################################################
#Bar plot of uber and non uber rides

non_uber_pct <- t1$pct
uber_pct <- t2$pct
height <- rbind(non_uber_pct, uber_pct)

mp <- barplot(height, beside = TRUE,col = c("darkblue", "red"),
              ylim = c(0, 8), names.arg = t1$hour,xlab = "Hour of the day",ylab = "percent of rides",legend.text = row.names(height),
              args.legend = list(x ='topright', bty='n', inset=c(-0.45,-0.2)))
              

text(mp, height, labels = format(height, 4),
     pos = 3, cex = .75) 



#######################################################################################################################

