#Load packages 

library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

#Set working directory
setwd("C:/Users/purva/Documents/Capstone")

#Load data from spreadsheet to dataframe

df_Dial7 <- read.csv("Dial 7/other-Dial7_B00887.csv", header = TRUE, stringsAsFactors = FALSE)

#Check structure of the dataframe created
str(df_Dial7)

#Check firstt few rows of the data frame
head(df_Dial7)

# Join Date and Time columns
df_Dial7 <- df_Dial7 %>% mutate(DateTime = paste(Date,Time))


# Add Area column to dataframe
df_Dial7$Area <- NA

#Replace Areas in the dataframe

df_Dial7 %>% filter(is.na(Area) == TRUE)
df_Dial7 %>% filter(PuFrom == "")
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("CT", State) == TRUE, "Connecticut"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("NJ", State) == TRUE, "New Jersey"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("PA", State) == TRUE, "Philadelphia"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("TEB", State) == TRUE, "New Jersey"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("HPN", State) == TRUE, "Westchester"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("DC", State) == TRUE, "Washington D.C"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("VA", State) == TRUE, "Virginia"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("MA", State) == TRUE, "Massachusetts"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("RI", State) == TRUE, "Rhode Island"))
df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl("MACARTHUR AIRPORT", PuFrom,ignore.case = TRUE) == TRUE, "Macarthur Airport"))

###############################################################################################################

# Load mapping files
df_Area_Mapping_NY <- read.csv("Dial 7/County_in_NY.csv", header = TRUE, stringsAsFactors = FALSE)
df_Area_Mapping_NY_City <- read.csv("Dial 7/County_in_NY_City.csv", header = TRUE, stringsAsFactors = FALSE)
###############################################################################################################

#Apply mapping file to replace Areas
startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl(df_Area_Mapping_NY$Town[ii], State,ignore.case = TRUE) == TRUE, 
                                                   df_Area_Mapping_NY$Area[ii]))
}


startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY_City)))){
  df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl(df_Area_Mapping_NY_City$Town[ii], PuFrom,ignore.case = TRUE) == TRUE, 
                                                 df_Area_Mapping_NY_City$Area[ii]))
}


startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl(df_Area_Mapping_NY$Town[ii], PuFrom,ignore.case = TRUE) == TRUE, 
                                                 df_Area_Mapping_NY$Area[ii]))
}

unique(df_Dial7$Area)
df_Dial7 %>% filter(is.na(Area))
###############################################################################################################

## Plotting bar plot to display demand of American cab by Area
ggplot(data = df_Dial7) +
  geom_bar(mapping = aes(x = Area, fill= Area)) + 
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45))
                                                                                                                              

###############################################################################################################
# Bar plot by per day from July - Sept

ggplot(data = df_Dial7) +
  geom_bar(mapping = aes(x = as.Date(df_Dial7$DateTime,format = "%Y.%m.%d")))

################################################################################################################

# Bar plot by Weekday for all weekdays in July - Sept

ggplot(data = df_Dial7) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_Dial7$DateTime,format = "%Y.%m.%d"), abbreviate = "F")))

###############################################################################################################

#Counts for each Area
Area_counts_Dial7 <- df_Dial7 %>% group_by(Area) %>% tally()

Area_counts_Dial7 %>% arrange(desc(n))

#Total count for all records
nrow(df_Dial7)/92

# Count of weekdays from July - September
df_Dial7 %>% mutate(day = weekdays(as.Date(DateTime,format = "%Y.%m.%d"), abbreviate = "F")) %>% 
  group_by(day) %>% tally() %>% arrange(desc(n))

# Count of individual days from July - September
df_Dial7 %>% mutate(per_day = as.Date(df_Dial7$DateTime,format = "%Y.%m.%d")) %>% group_by(per_day) %>% tally() %>% arrange(desc(n))


df_Dial7 %>% mutate(day = weekdays(as.Date(DateTime,format = "%Y.%m.%d"), abbreviate = "F")) %>% 
  group_by(day,Area) %>% tally() %>% arrange(desc(n))






