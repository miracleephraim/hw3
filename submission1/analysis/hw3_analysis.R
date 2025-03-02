# analysis

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(readr)

df <- read_rds("C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/output/TaxBurden_Data.rds")


# question 1

df2 <-   df %>%
    filter(Year >= 1970 & Year <= 1985) %>%
    group_by(state) %>%
    arrange(Year) %>%
    mutate(tax_diff = tax_state - lag(tax_state),
            tax_change = ifelse((tax_diff > 0),1,0)) %>%
    ungroup()

prop_tax <- df2 %>%
    group_by(Year) %>%
    summarise(prop = sum(tax_change), na.rm=TRUE)

ggplot(data=prop_tax, aes(Year,prop)) +
    geom_bar() 


# question 2

avgs <- df %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  mutate(tax_state2012 = price_cpi * tax_state,
         cost_per_pack2012 = price_cpi * cost_per_pack) %>%
  group_by(Year, state) %>%
  summarise(avg_tax = mean(tax_state2012),
            avg_cost = mean(cost_per_pack2012)

ggplot(data = avgs, aes(avg_tax, avg_cost)) +
    geom_line()

