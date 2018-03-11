#Load packages 

library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

#Set working directory
setwd("C:/Users/purva/Documents/Capstone")

#Load data from spreadsheet to dataframe

df_American <- read.csv("American/other-American_B01362.csv", header = TRUE, stringsAsFactors = FALSE)

#Check structure of the dataframe created
str(df_American)

#Update names of the columns in the dataframe
names(df_American) <- c("Date","Time","Pick_up_addr","x","y","z")

#Drop unwanted columns
df_American <- df_American %>% select(-x,-y,-z)

#Split column Pick_up_Addr into Address and Area column
df_American <- df_American %>% separate(Pick_up_addr,c("Addr","Area"),remove = FALSE, sep = ",", extra = "merge",fill = "right")


#Check unique Areas in the dataframe
unique(df_American$Area)

#Check the first 6 records of the dataframe
head(df_American)

#Replace Area abbreviations with actual names
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
##################################################################################################################
# Load Mapping csv file
df_Area_Mapping <- read.csv("Carmel/Area_Mapping.csv", header = TRUE, stringsAsFactors = FALSE)
df_Area_Mapping_NY <- read.csv("Carmel/County_in_NY.csv", header = TRUE, stringsAsFactors = FALSE)
##################################################################################################################

#Apply mapping file to replace Areas


startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  df_American <- df_American %>% mutate(Area = replace(Area,grepl(df_Area_Mapping_NY$Town[ii], Pick_up_addr,ignore.case = TRUE) == TRUE, 
                                                 df_Area_Mapping_NY$Area[ii]))
}

#######################################################################################################################  
 unique(df_American$Area)
 df_American %>% filter(is.na(Area)) 
 
#Check for empty Areas in the dataframe
df_American %>% filter(Area == " ")
df_American %>% filter(Area == "")
#######################################################################################################################  


# Concatenate the date and time columns from the dataframe
df_American <- df_American %>% mutate(DateTime = paste(Date,Time))

head(df_American)

###############################################################################################

## Plotting bar plot to display demand of American cab by Area
ggplot(data = df_American) +
  geom_bar(mapping = aes(x = Area))

ggplot(data = df_American) +
  geom_bar(mapping = aes(x = Area, fill= Area)) + 
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45))

###############################################################################################################
# Bar plot by per day from July - Sept

ggplot(data = df_American) +
  geom_bar(mapping = aes(x = as.Date(df_American$DateTime,format = "%m/%d/%Y")))

################################################################################################################

# Bar plot by Weekday for all weekdays in July - Sept

ggplot(data = df_American) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_American$DateTime,format = "%m/%d/%Y"), abbreviate = "F")))

###############################################################################################################
#Counts for each Area
Area_counts_American <- df_American %>% group_by(Area) %>% tally()

Area_counts_American %>% arrange(desc(n))

#Total count for all records
nrow(df_American)/92

# Count of weekdays from July - September
df_American %>% mutate(day = weekdays(as.Date(DateTime,format = "%m/%d/%Y"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))
  
# Count of individual days from July - September
df_American %>% mutate(per_day = as.Date(df_American$DateTime,format = "%m/%d/%Y")) %>% group_by(per_day) %>% tally() %>% arrange(desc(n))






