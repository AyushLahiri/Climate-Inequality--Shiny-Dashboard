library(shiny)
library(fullPage)
library(ggplot2)
library(gganimate)
library(transformr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(RCurl)
library(rnaturalearth)
library(tidycensus)
library(viridis)
library(colorBlindness)
library(plotly)
library(ggmap)
library(rgl)
library(maps)
library(countrycode)
library(sf)
library(ggplot2)
library(osmdata)
library(stringi)
library(leaflet)
library(tigris)
library(readxl)

#############These are larger transformations. Their outputs are used as inputs in the shiny app ############################

###########################Create the per capita and total national datasets from separate files#########################
gni_class = read.csv('./Data/GNI_class.csv')

# Loop through each data frame
data_frames <- list(
  list(name = "co21", file = "co2_pc.csv", value_col = "co2_pc"),
  list(name = "co22", file = "co2_total.csv", value_col = "co2_total"),
  list(name = "ch41", file = "methane_pc.csv", value_col = "ch4_pc"),
  list(name = "ch42", file = "methane_total.csv", value_col = "ch4_total"),
  list(name = "no21", file = "no2_pc.csv", value_col = "no2_pc"),
  list(name = "no22", file = "no2_total.csv", value_col = "no2_total")
)
columns_to_pivot <- c('1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999',
                      '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
                      '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019')
# Loop through each data frame
for (df_info in data_frames) {
  # Read the data frame
  df <- read_csv(paste0('./Data/emissions/', df_info$file))
  
  # Convert specified columns to numeric
  df[columns_to_pivot] <- lapply(df[columns_to_pivot], function(x) as.numeric(as.character(x)))
  
  # Pivot the data frame
  df <- df %>%
    pivot_longer(
      cols = columns_to_pivot,
      names_to = "Year",
      values_to = df_info$value_col
    ) %>%
    left_join(gni_class, by = c('iso' = 'Code')) %>%
    mutate(!!sym(df_info$value_col) := ifelse(is.na(!!sym(df_info$value_col)), 0, !!sym(df_info$value_col)),
           group = ifelse(Country == 'Venezuela', 'Low income', group),
           !!sym(df_info$value_col) := ifelse(!!sym(df_info$value_col) < 0, 0.01, !!sym(df_info$value_col))) %>%
    mutate(group = case_when(
      group == "Lower middle income" ~ "Low income",
      group == "Upper middle income" ~ "Middle income",
      TRUE ~ group
    )) %>%
    group_by(group, Year) %>%
    summarise(!!sym(df_info$value_col) := sum(!!sym(df_info$value_col)))
  
  # Assign the transformed data frame back to its variable
  assign(df_info$name, df, envir = .GlobalEnv)
}

# Join the data frames
final_df <- Reduce(function(x, y) left_join(x, y, by = c('group', 'Year')), list(co21, co22, ch41, ch42, no21, no22))
#################################################################################################
disasters = read_csv("C:/Users/ayush/OneDrive/Desktop/Georgetown/Data Viz/Final Project/Data/Climate/clim_dis.csv")
disasters$Indicator <- sub("Climate related disasters frequency, Number of Disasters: ", "", disasters$Indicator)

# Clean up the year columns by removing the 'F' prefix and group
names(disasters) <- sub("^F", "", names(disasters))


disasters_long <- disasters %>%
  pivot_longer(
    cols = c('1980':'2019'),
    names_to = "Year",
    values_to = "Disaster_Count"
  ) 

disasters_grouped <- disasters_long %>%
  group_by(Indicator, Year) %>%
  summarize(Disaster_Count = sum(Disaster_Count, na.rm = TRUE))
disasters_grouped <- disasters_grouped %>%
  arrange(Year)

write.csv(disasters_grouped, "C:/Users/ayush/OneDrive/Desktop/Georgetown/Data Viz/Final Project/Final_Project/Data/Climate/clim_dis.csv", row.names = FALSE)
##############################################################
# Clean up the year columns by removing the 'M' prefix and group
co2 = read_csv('./Data/Climate/co2.csv')
co2$numer_date <- as.numeric(sub("M.*", "", co2$Date))

co2_grouped <- co2 %>% filter(Unit != 'Percent')%>%
  group_by(numer_date) %>%
  summarize(avg_co2 = mean(Value, na.rm = TRUE))

############################################################
#group sea levels. clean data by premoving D prefix in data

global_levels = read_csv('./Data/Climate/sea_lev.csv')
global_levels$Date <- (sub("^D", "", global_levels$Date))
global_levels$Year <- substr(global_levels$Date, nchar(global_levels$Date) - 3, nchar(global_levels$Date))

global_levels = global_levels %>% select(c(Year,Value)) %>% group_by(Year) %>% summarize(avg_sea_level_change = mean(Value))%>%
  mutate(Year = as.numeric(Year))
##########################################################
#group sea levels clean data by removing F prefix in data 
temp_change = read_csv('./Data/Climate/temp_change.csv')

names(temp_change) <- sub("^F", "", names(temp_change))
temp_change_long <- temp_change %>%
  pivot_longer(
    cols = c('1961':'2022'),
    names_to = "Year",
    values_to = "temp_change"
  ) 
temp_change_grouped <- temp_change_long %>%
  group_by(Year) %>%
  summarize(Avg_temp_change = mean(temp_change, na.rm = TRUE))%>%mutate(Year = as.numeric(Year))



############### Create trade value data from individual files ########################################################

path <- "./Data/Exports"

# Get the list of .xlsx files in the directory
files <- dir_ls(path, regexp = "\\.xlsx$")

# List of countries to filter
countries <- c("United States", "Japan", "Germany", 
               "Netherlands, The", "Korea, Rep. of", "United Kingdom", "Australia", "France")

# Loop through files
pwt_data <- read_xlsx("C:/Users/ayush/Downloads/pwt1001 (1).xlsx", sheet = "Data")
sheet2_data <- read_xlsx("C:/Users/ayush/Downloads/pwt1001 (1).xlsx", sheet = "Sheet2")

all_dfs <- list()
for (file_path in files) {
  file_name <- fs::path_ext_remove(fs::path_file(file_path))
  df <- read_xlsx(file_path)
  print(file_name)
  
  #read in export  data from separate files and rehsape 
  year_cols <- as.character(1998:2020)
  df <- df %>%
    mutate(across(all_of(year_cols), ~as.numeric(as.character(.))))
  
  df_long <- df %>%
    pivot_longer(cols = all_of(year_cols), names_to = "Year", values_to = "Export") %>% select(c('Country','Year','Export'))
  
  df_long <- df_long %>%
    filter(Country %in% countries)
  print(countries)
  
  #rename for later consistency
  df_long$Country <- recode(df_long$Country, 
                            "Netherlands, The" = "Netherlands",
                            "Korea, Rep. of" = "Republic of Korea")
  
  # Standardize the country names for case-insensitive matching
  standardized_file_name <- tolower(file_name)
  
  erdi_col_name <- paste0(tolower(file_name), "_erdi")
  
  df_long$Year <- as.integer(df_long$Year)
  
  sheet2_data$Year <- as.integer(sheet2_data$Year)
  
  
  # Join the pl_gdpo from pwt_data
  if (standardized_file_name == 'vietnam'){
    pwt_data$country <- recode(pwt_data$country, 'Viet Nam' = 'vietnam')
  }
  
  df_long <- df_long %>%
    left_join(pwt_data %>%
                filter(tolower(country) == standardized_file_name) %>%
                select(year, pl_gdpo) %>%
                rename(Year = year, !!sym(erdi_col_name) := pl_gdpo), by = "Year")
  print('we are here')
  
  # Join data from Sheet2 which normalizes the data 
  df_long <- df_long %>%
    left_join(sheet2_data %>%
                select(Country,Year, Code, ERDI), by = c("Country" = "Country", "Year" = "Year"))
  
  avg_data <- df_long %>%
    filter(Year %in% 2017:2019) %>%
    group_by(Country) %>%
    summarize(Avg = mean(!!sym(erdi_col_name), na.rm = TRUE))
  
  df_long <- df_long %>%
    left_join(avg_data, by = "Country") %>%
    mutate(!!sym(erdi_col_name) := ifelse(Year == 2020, Avg, !!sym(erdi_col_name))) %>%
    select(-Avg)
  
  pwt_data <- read_xlsx("C:/Users/ayush/Downloads/pwt1001 (1).xlsx", sheet = "Data")
  pwt_data_full <- pwt_data %>% filter(year %in% (1998:2020)) %>% group_by(year) %>% summarize(Avg_ERDI = mean(pl_gdpo, na.rm = TRUE))%>% rename(Year = year)
  pwt_data_2020 <- pwt_data %>% filter(year %in% c(2019,2018)) %>% summarize(Avg_ERDI = mean(pl_gdpo, na.rm = TRUE)) %>% pull(Avg_ERDI)
  
  pwt_data_full <- pwt_data %>% filter(year %in% (1998:2020)) %>% group_by(year) %>% 
    summarize(Avg_ERDI = mean(pl_gdpo, na.rm = TRUE))%>%
    rename(Year = year) 
  
  df_long <- df_long %>%
    left_join(pwt_data_full, by = "Year")%>%mutate(Avg_ERDI = ifelse(Year == 2020,pwt_data_2020,Avg_ERDI ))
  
  #Calculate trade value extraction 
  df_long <- df_long %>% mutate(Transfer = Export*(get(erdi_col_name) / ERDI) - Export,transfer_country = file_name)
  df_long <- df_long %>% mutate(Transfer_fair = Export*(get(erdi_col_name) / Avg_ERDI) - Export,transfer_country = file_name)
  
  
  # Reorder columns
  df_long <- df_long %>%
    select(Code, Country,transfer_country,Year, Transfer,Transfer_fair, !!sym(erdi_col_name),Avg_ERDI, Export, ERDI)
  
  df_transfer <- df_long %>%
    select(Code, Country,transfer_country,Year, Transfer,Transfer_fair)
  
  all_dfs[[file_name]] <- df_transfer
  assign(file_name, df_long)
  
}
transfer_df <- bind_rows(all_dfs)
