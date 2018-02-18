###############################################################################################################
#Read csv file into dataframe
df_Carmel <- read.csv("other-Carmel_B00256.csv", header = TRUE, stringsAsFactors = FALSE)

# Load Mapping csv file
df_Area_Mapping <- read.csv("Area_Mapping.csv", header = TRUE, stringsAsFactors = FALSE)
str(df_Carmel)
unique(df_Carmel$Area)
unique(df_Carmel$PU_Adress)

# Add Area column to dataframe
df_Carmel$Area <- NA

# Join Date and Time columns
df_Carmel <- df_Carmel %>% mutate(DateTime = paste(Date,Time))
###############################################################################################################
# Replace areas

df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*NYC", PU_Adress) == TRUE, "Manhattan"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*NYc", PU_Adress) == TRUE, "Manhattan"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Nyc", PU_Adress) == TRUE, "Manhattan"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*JFK*", PU_Adress) == TRUE, "JFK Airport"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*EWR", PU_Adress) == TRUE, "EWR Airport"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Queens", PU_Adress) == TRUE, "Queens"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*QU*", PU_Adress) == TRUE, "Queens"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*White Plains*", PU_Adress) == TRUE, "White Plains"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*NJ*", PU_Adress) == TRUE, "New Jersey"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*BK*", PU_Adress) == TRUE, "Brooklyn"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*BX*", PU_Adress) == TRUE, "Bronx"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*LI*", PU_Adress) == TRUE, "Long Island"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*CT*", PU_Adress) == TRUE, "Connecticut"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Westchester Airport*", PU_Adress) == TRUE, "Westchester Airport"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Rye*", PU_Adress) == TRUE, "Rye"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*SI", PU_Adress) == TRUE, "Staten Island"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Staten Island", PU_Adress) == TRUE, "Staten Island"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Philadelphia", PU_Adress) == TRUE, "Philadelphia"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*PA*", PU_Adress) == TRUE, "Philadelphia"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Yonkers", PU_Adress) == TRUE, "Yonkers"))
df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl("*Greenwich", PU_Adress) == TRUE, "Connecticut"))
###############################################################################################################
df_Carmel %>% filter(is.na(Area) == TRUE) %>% arrange(Base_No)
###############################################################################################################
#Apply mapping file to replace Areas

startindex <- 1
for (ii in seq(startindex, nrow(na.omit(df_Area_Mapping)))){
  df_Carmel <- df_Carmel %>% mutate(Area = replace(Area,grepl(df_Area_Mapping$Town[ii], PU_Adress,ignore.case = FALSE) == TRUE, 
                                                   df_Area_Mapping$Area[ii]))
}
###############################################################################################
head(df_Carmel)
###############################################################################################

## Plotting bar plot to display demand of American cab by Area
ggplot(data = df_Carmel) +
  geom_bar(mapping = aes(x = Area)) + 
  theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45))

###############################################################################################################
# Bar plot by per day from July - Sept

ggplot(data = df_Carmel) +
  geom_bar(mapping = aes(x = as.Date(df_Carmel$DateTime,format = "%m/%d/%Y")))

################################################################################################################

# Bar plot by Weekday for all weekdays in July - Sept

ggplot(data = df_Carmel) +
  geom_bar(mapping = aes(x = weekdays(as.Date(df_Carmel$DateTime,format = "%m/%d/%Y"), abbreviate = "F")))

###############################################################################################################

