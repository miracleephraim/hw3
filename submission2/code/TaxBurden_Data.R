
# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Miracle Ephraim
## Date Created:  02/24/2025
## Date Edited:   02/26/2025
## Description:   Clean and analyze CDC data 


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


cig.data <- read_csv("C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/input/The_Tax_Burden_on_Tobacco__1970-2019.csv", col_names = TRUE)
cpi.data <- read_xlsx("C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/input/historical-cpi-u-202501.xlsx", skip = 3)


# Clean tobacco data --------------------------------------------------------------
cig.data <- cig.data %>%
  mutate(measure = case_when(
    SubMeasureDesc == "Average Cost per pack" ~ "cost_per_pack",
    SubMeasureDesc == "Cigarette Consumption (Pack Sales Per Capita)" ~ "sales_per_capita",
    SubMeasureDesc == "Federal and State tax as a Percentage of Retail Price" ~ "tax_percent",
    SubMeasureDesc == "Federal and State Tax per pack" ~ "tax_dollar",
    SubMeasureDesc == "Gross Cigarette Tax Revenue" ~ "tax_revenue",
    SubMeasureDesc == "State Tax per pack" ~ "tax_state"
  )) %>%
  select(state_abb = LocationAbbr, 
         state = LocationDesc, 
         Year, 
         value=Data_Value, 
         measure)
         
final.data <- pivot_wider(cig.data, 
                        # id_cols = c("state","Year","measure"),
                         names_from = "measure",
                         values_from = "value") %>%
  arrange(state, Year)



# Clean CPI data ----------------------------------------------------------
cpi.data$Jan. <- as.character(cpi.data$Jan.)
cpi.data <- pivot_longer(cpi.data, 
                         cols=c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.","Nov.","Dec."),
                         names_to="month",
                         values_to="index")
cpi.data2 <- cpi.data %>%
#  mutate(index <- as.numeric(index)) %>%
  group_by(Year) %>%
  summarize(index=mean(as.numeric(index), na.rm=TRUE))
cpi.data2$Year <- as.numeric(cpi.data2$Year)


# Form final dataset ------------------------------------------------------
# adjust to 2012 dollars
final.data <- final.data %>%
  left_join(cpi.data2, by="Year") %>%
  mutate(price_cpi=cost_per_pack*(230/index)) 

write_tsv(final.data,"C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/output/TaxBurden_Data.txt",append=FALSE,col_names=TRUE)
write_rds(final.data,"C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/output/TaxBurden_Data.rds")


