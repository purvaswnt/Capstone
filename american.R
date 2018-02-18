df_American <- read.csv("other-American_B01362.csv", header = TRUE, stringsAsFactors = FALSE)

str(df_American)

library(dplyr)
library(tidyr)
library(chron)

names(df_American) <- c("Date","Time","Pick_up_addr","x","y","z")
df_American <- df_American %>% select(-x,-y,-z)

df_American <- df_American %>% separate(Pick_up_addr,c("Addr","Area"),remove = FALSE, sep = ",", extra = "merge",fill = "right")

unique(df_American$Area)

head(df_American)

df_American <- df_American %>% mutate(Area = replace(Area,grepl("BX", Area) == TRUE, "Bronx"))

df_American <- df_American %>% mutate(Area = replace(Area,grepl("QU", Area) == TRUE, "Queens"))

df_American <- df_American %>% mutate(Area = replace(Area,grepl("NJ", Area) == TRUE, "New Jersey"))

df_American <- df_American %>% mutate(Area = replace(Area,grepl("CT", Area) == TRUE, "Connecticut"))

df_American <- df_American %>% mutate(Area = replace(Area,grepl("PA", Area) == TRUE, "pennsylvania"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("BK", Area) == TRUE, "Brooklyn"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("LI", Area) == TRUE, "Long Island"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("* B", Area) == TRUE, "Bronx"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl(" THEY GOING TO jfk  ESPERE EL PASAJERO 15 MINUTOS  NYC", Area) == TRUE, 
                                                     "JFK Airport"))

#stringr::str_detect(string, pattern)
 df_American %>% filter(Area == " ")
 df_American %>% filter(Area == "")
 df_American %>% filter(is.na(Area)) %>% mutate(Area = geocode(Addr,output = "more"))



df_American <- df_American %>% mutate(Area = replace(Area,grepl("*YONKERS*", Addr) == TRUE, "Yonkers"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*NEW ROCHELLE*", Addr) == TRUE, "New Rochelle"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*WHITE PLAINS*", Addr) == TRUE, "White Plains"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*RYE*", Addr) == TRUE, "Rye"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*VALHALLA*", Addr) == TRUE, "Valhalla"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*TARRYTOWN*", Addr) == TRUE, "Tarrytown"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*PELHAM MANOR*", Addr) == TRUE, "Pelham Manor"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*SCARSDALE*", Addr) == TRUE, "Scarsdale"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*OSSINING*", Addr) == TRUE, "Ossining"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*VERNON*", Addr) == TRUE, "Vernon"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*Bronx", Addr) == TRUE, "Bronx"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*NYC", Addr) == TRUE, "Manhattan"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*JFK*", Addr) == TRUE, "JFK Airport"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*EWR", Addr) == TRUE, "EWR Airport"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*Queens", Addr) == TRUE, "Queens"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*Brooklyn", Addr) == TRUE, "Brooklyn"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*BRONX", Addr) == TRUE, "Bronx"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*LGA", Addr) == TRUE, "LaGuardia Airport"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*Mott Haven", Addr) == TRUE, "Bronx"))
df_American <- df_American %>% mutate(Area = replace(Area,grepl("*Morrisania", Addr) == TRUE, "Bronx"))

  
df_American <- df_American %>% mutate(DateTime = paste(Date,Time))
head(df_American)

###############################################################################################

## Plotting bar plot to display demand of American cab by Area
ggplot(data = df_American) +
  geom_bar(mapping = aes(x = Area))

###############################################################################################################
# Bar plot by per day from July - Sept

ggplot(data = df_American) +
  geom_bar(mapping = aes(x = as.Date(df_American$DateTime,format = "%m/%d/%Y")))

################################################################################################################

# Bar plot by Weekday for all weekdays in July - Sept

ggplot(data = df_American) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_American$DateTime,format = "%m/%d/%Y"), abbreviate = "F")))

###############################################################################################################


hist(as.POSIXct(df_American$DateTime,format="%m/%d/%Y %I:%M:%S %p"), breaks=24, col="red")

ggplot(data = df_American) +
  geom_histogram(aes(x = strptime(df_American$Time,format="%m/%d/%Y %I:%M:%S %p")), binwidth = 0.5)

as.POSIXct(df_American$DateTime,format="%m/%d/%Y %I:%M:%S %p")



ggplot(data = df_American) +
  geom_bar(mapping = aes(x = Time))



