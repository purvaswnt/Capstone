###############################################################################################################
#Read csv file into dataframe
df_Dial7 <- read.csv("other-Dial7_B00887.csv", header = TRUE, stringsAsFactors = FALSE)
str(df_Dial7)
head(df_Dial7)

# Join Date and Time columns
df_Dial7 <- df_Dial7 %>% mutate(DateTime = paste(Date,Time))

unique(df_Dial7$Area)

# Add Area column to dataframe
df_Dial7$Area <- NA

###############################################################################################################
# Load mapping files
df_Area_Mapping_NY <- read.csv("County_in_NY.csv", header = TRUE, stringsAsFactors = FALSE)
df_Area_Mapping_NY_City <- read.csv("County_in_NY_City.csv", header = TRUE, stringsAsFactors = FALSE)

#Apply mapping file to replace Areas
startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping_NY)))){
  df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl(df_Area_Mapping_NY$Town[ii], State,ignore.case = TRUE) == TRUE, 
                                                   df_Area_Mapping_NY$Area[ii]))
}

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping)))){
  df_Dial7 <- df_Dial7 %>% mutate(Area = replace(Area,grepl(df_Area_Mapping$Town[ii], PuFrom,ignore.case = TRUE) == TRUE, 
                                                 df_Area_Mapping$Area[ii]))
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

###############################################################################################################

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

df_Dial7 <- df_Dial7 %>% filter(PuFrom == "PORT JEFFERSON") %>%
  mutate(Area = replace(Area,grepl("Port Jefferson", PuFrom,ignore.case = TRUE) == TRUE, "Port Jefferson"))
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

as.Date(df_Dial7$DateTime,format = "%Y.%m.%d")

head(df_Dial7)


x <- "27.05.2009 14:03:25:777"  # this is a simplified version of your data
y <- gsub(":", ".", x) 
options(digits.secs = 3)
strptime(y, "%d.%m.%Y %H:%M:%OS")
as.Date(x,format = "%d.%m.%Y")





