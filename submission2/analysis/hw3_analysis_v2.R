# analysis

install.packages("modelsummary")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(readr)
library(modelsummary)

df <- read_rds("C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/data/output/TaxBurden_Data.rds")


# question 1

df2 <-  df %>%
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
    geom_col() 


# question 2

avgs2 <- df %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  mutate(tax_state2012 = price_cpi * tax_state,
         cost_per_pack2012 = price_cpi * cost_per_pack) %>%
  group_by(Year) %>%
  summarise(avg_tax = mean(tax_state2012),
            avg_cost = mean(cost_per_pack2012)) %>%
            ungroup() 
            
ggplot() +
geom_line(data = avgs2, aes(Year, avg_tax), color = "blue") +
geom_line(data = avgs2, aes(Year, avg_cost), color = "light blue") 

# question 3
top5_a <- df %>%
    filter(Year == 1970 | Year == 2018) %>%
    group_by(state) %>%
    summarise(diff = price_cpi - lag(price_cpi)) %>%
    filter(!is.na(diff)) %>%
    arrange(desc(diff))

head(top5_a, 5)

# top 5 differences:  DC, NY, RH, HI, MA

change_a <- df %>%
    filter(state_abb == c("NY", "HI", "DC", "RI", "MA")) %>%
    group_by(Year, state_abb) %>%
    summarise(avg_sale = mean(sales_per_capita, na.rm = TRUE))

# graph - change in sales, grouped by state
ggplot(df %>% filter(state_abb == c("NY", "CT", "DC", "RI", "MA")), aes(Year, sales_per_capita, colour = state)) +
geom_line() 


# question 4

# lowest prices
low5 <- df %>%
    filter(Year == 1970 | Year == 2018) %>%
    group_by(state) %>%
    summarise(diff = price_cpi - lag(price_cpi)) %>%
    filter(!is.na(diff)) %>%
    arrange(diff) 

# bottom states: Miss, ND, TN, GA, AL

change2_a <- df %>%
    filter(state_abb == c("MO", "ND", "TN", "GA", "AL")) %>%
    group_by(Year, state_abb) %>%
    summarise(avg_sale = mean(sales_per_capita, na.rm = TRUE))

# graph B -  change in sales, grouped by state
ggplot(df %>% filter(state_abb == c("MO", "ND", "TN", "GA", "NC")), aes(Year, sales_per_capita, colour = state)) +
geom_line() 

# Question 6

model1 <- lm(log(sales_per_capita) ~ log(price_cpi), 
   data = (df %>% filter(Year >= 1970 & Year <= 1990)))
summary(model1)

df <- df %>%
    mutate(log_sales = log(sales_per_capita),
            log_cost = log(cost_per_pack),
            log_cpi =  log(price_cpi),
            tax_cpi=tax_state*(230/index),
            total_tax_cpi=tax_dollar*(230/index),
            log_total_tax=log(total_tax_cpi),                             
            log_state_tax=log(tax_cpi))

# regular OLS
ols1 <- lm(log_sales ~ log_cpi, data = (df %>% filter(Year >= 1970 & Year <= 1990)))
summary(ols1)


# using an instrument
step1 <- lm(log_cpi ~ log_total_tax, data=(
    df %>% filter(Year >= 1970 & Year <= 1990)))
pricehat <- predict(step1)
step2 <- lm(log_sales ~ pricehat, data=(
    df %>% filter(Year >= 1970 & Year <= 1990)))
summary(step2)

# question 9

# regular OLS
ols2 <- lm(log_sales ~ log_cpi, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
summary(ols2)

# question 9
# using an instrument
step3 <- lm(log_cpi ~ log_total_tax, data=(
    df %>% filter(Year >= 1991 & Year <= 2015)))
pricehat <- predict(step3)
step4 <- lm(log_sales ~ pricehat, data=(
    df %>% filter(Year >= 1991 & Year <= 2015)))
summary(step4)

modelsummary(
    list("OLS (1970-1990)" = ols1,
         "IV (1970-1990)" = step2,
         "OLS (1991-2015)" = ols2,
         "IV (1991-2015)" = step4),
    output = "latex",
    title = "Table 1: Elasticity Estimates from OLS and IV",
    coef_omit = "Intercept",
    gof_map = list(
        list("raw" = "nobs", "clean" = "N", "fmt" = 0),
        list("raw" = "r.squared", "clean" = "R²", "fmt" = 2)
    )
)

kable_styling(latex_options = "hold_position")

save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw3/submission2/results/hw_workspace.Rdata")


# As we come into a more recent time period, cigarettes appear to be more responsive to changes in price. Bulk of variation in instrumented model is explained by the instrument.
# is coming from more recent years