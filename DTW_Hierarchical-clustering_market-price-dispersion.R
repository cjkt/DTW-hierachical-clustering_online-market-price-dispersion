########## primary setting ##########
# working directory
# setwd("working directory")

# libraries
library(data.table)
library(writexl)
library(ggplot2)
library(dplyr)
library(ggridges)
library(lubridate)
library(zoo)
library(plotly)
library(tidyr)
library(dtw)


# data import
dataFrame <- fread("D:/DU/home_assignment_data_pricing.csv")

# Extracting "Cellphones" category from main data frame
dfWii <- subset(dataFrame, category=="Nintendo Wii U")

# subset data export
#write_xlsx(dfWii,path="dfWii.xlsx")




########## 1 preliminary data manipulation ##########

#### data type verification
head(dfWii)
str(dfWii)

### data format (str) change
# new data frame with modification (dfWii2)
dfWii2 <- data.frame(dfWii)
dfWii2[,"product_id"] = as.factor(dfWii2$product_id)
dfWii2[,"store_id"] = as.factor(dfWii2$store_id)

str(dfWii2)
head(dfWii2)
summary(dfWii2)

# new data frame with outliers removed (dfWii3)
# removed products: 1292995, 1631225, 1997676, 2073610, 3096810, 2628694
dfWii3 <- dfWii2
dfWii3 <- dfWii3[!(dfWii3$product_id == 1292995 | 
                     dfWii3$product_id == 1631225 |
                     dfWii3$product_id == 1997676 |
                     dfWii3$product_id == 2073610 |
                     dfWii3$product_id == 3096810 |
                     dfWii3$product_id == 2628694) , ]


#### data grouping: time scale
# day level
dfWii_day <- dfWii3

head(df_day)
summary(df_day)

# week level (weekly mean price) # error
#dfWii_week <- dfWii3 %>%
#  select(product_id, date, week, price, store_id) %>%
#  mutate(year = year(date)) %>%
#  mutate(year_week = paste(year, week, sep = "-")) %>%
#  select(year_week, product_id, store_id, price) %>%
#  group_by(year_week, product_id, store_id, price) %>%
#  summarize(mean_price = mean(price))

head(dfWii_week)
summary(dfWii_week)

# month level (monthly mean price)
dfWii_month <- dfWii3 %>%
  select(product_id, date, price, store_id) %>%
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month, product_id, store_id) %>%
  summarize(mean_price = mean(price))

head(dfWii_month)
summary(dfWii_month)

# quarter level (quarter mean price)
dfWii_quarter <- dfWii3 %>%
  select(product_id, date, price, store_id) %>%
  mutate(year_quarter = as.Date(as.yearqtr(date)) %>% ymd()) %>%
  group_by(year_quarter, product_id, store_id) %>%
  summarize(mean_price = mean(price))

head(dfWii_quarter)
summary(dfWii_quarter)

# year level (year mean price)
dfWii_year <- dfWii3 %>%
  select(product_id, date, price, store_id) %>%
  mutate(year = floor_date(date, "years") %>% ymd()) %>%
  group_by(year, product_id, store_id) %>%
  summarize(mean_price = mean(price))

head(dfWii_year)
summary(dfWii_year)

#### data grouping: by variables (product & retailer)
# product level
dfWii_product <- dfWii3 %>%
  select(product_id, date, price, store_id) %>%
  group_by(product_id, date) %>%
  summarize(mean_price = mean(price))

head(dfWii_product)
summary(dfWii_year)

# store level
dfWii_store <- dfWii3 %>%
  select(store_id, date, price, product_id) %>%
  group_by(store_id, date) %>%
  summarize(mean_price = mean(price))

head(dfWii_store)
summary(dfWii_store)





########## 2 preliminary data exploration ##########

### main data frame
dfWii3
head(dfWii3)
summary(dfWii3)
str(dfWii3)


########## 2-1 data characteristics ##########

### product: total 19 (6 removed)
prod_list_df3 <- unique(data.frame(dfWii3$product_id))
str(prod_list)


### retailer: total 42 
retailer_list_df3 <- unique(data.frame(dfWii3$store_id))
str(retailer_list)

### Observation period
# week: 1 ~ 53
min(dfWii3$week)
max(dfWii3$week)
# week day: Mon ~ sun
unique(dfWii3$weekday)
# Year: 2013 ~ 2017
# Date: 2013-06-20 ~ 2017-02-25
min(dfWii3$date)
max(dfWii3$date)

##### some plots of statistical summary

# price boxplot by products
product_overview_box <- ggplot(dfWii2, aes(product_id, price)) +            
  geom_boxplot() +
  ylim(150, 1200) +
  #scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, size = 10)) +
  xlab("Products (ID)") + 
  ylab("Price (SEK)")

product_overview_box # outlying products: 1292995, 1631225, 1997676, 2073610, 3096810

#outliers removed
product_boxplot_without_outliers <- ggplot(dfWii3, aes(product_id, price)) +            
  geom_boxplot() +
  #scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, size = 10)) +
  xlab("Products (ID)") + 
  ylab("Price (SEK)") +
  scale_y_log10()
  

product_boxplot_without_outliers # outlying products: 1292995, 1631225, 1997676, 2073610, 3096810

# group by product: economy & premium
mean_price <- dfWii2 %>%
  group_by(product_id) %>%
  summarize(mp = mean(price))

mean_price

product_overview_mean <- ggplot(mean_price) +                                          ###
  geom_text(aes(x = product_id, y = mp, label = product_id),
            size = 4, angle = 60, 
            color = ifelse(mean_price$mp > 650 | mean_price$mp < 350, "red", "black"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Products (ID)") + 
  ylab("Mean price over the period (SEK)") 

product_overview_mean # outlying products: 3096810, 1292995, 1631225, 1997676, 2073610

# outliers removed (dfWii3)
mean_price_3 <- dfWii3 %>%
  group_by(product_id) %>%
  summarize(mp = mean(price))

mean_price_3

product_mean_without_outliers <- ggplot(mean_price_3) +                                          ###
  geom_text(aes(x = product_id, y = mp, label = product_id),
            size = 4, angle = 45, 
            color = ifelse(mean_price_3$mp > 650 | mean_price_3$mp < 350, "red", "black"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Products (ID)") + 
  ylab("Mean price (SEK)") 

product_mean_without_outliers # outlying products: 3096810, 1292995, 1631225, 1997676, 2073610




# group by sd
sd_price <- dfWii2 %>%
  group_by(product_id) %>%
  summarize(sd_p = sd(price))

product_overview_sd <- ggplot(sd_price, aes(product_id, sd_p)) +
  geom_text(aes(x = product_id, y = sd_p, label = product_id), ###
            size = 4, angle = 60,
            color = ifelse(sd_price$sd_p > 300, "red", "black"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Products (ID)") + 
  ylab("SD (Standard deviation")

product_overview_sd # outlying products: 2628694

# outliers removed (dfWii3)
sd_price_3 <- dfWii3 %>%
  group_by(product_id) %>%
  summarize(sd_p = sd(price))

product_sd_without_outliers <- ggplot(sd_price_3, aes(product_id, sd_p)) +
  geom_text(aes(x = product_id, y = sd_p, label = product_id), ###
            size = 4, angle = 45,
            color = ifelse(sd_price_3$sd_p > 300, "red", "black"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab("Products (ID)") + 
  ylab("SD (Standard deviation")

product_sd_without_outliers



# observation density by period
ggplot(dfWii3, aes(year)) +
  geom_density()

ggplot(dfWii_quarter, aes(year_quarter)) + # error
  geom_density()

ggplot(dfWii_month, aes(year_month)) + # error
  geom_density()

ggplot(dfWii3, aes(week)) +
  geom_density() 

ggplot(dfWii3, aes(weekday)) +
  geom_density()




########## 2-2 by product & retailer ##########
# main variables for analysis: product & retailer
# some plots 
product_total_line <- ggplot(dfWii_product, aes(date, mean_price, colour = product_id)) + ###
  geom_line() +
  labs(color = "Products (ID)") +
  ylab("Average price") +
  xlab("Year")

product_total_line

product_total_line_facet <- ggplot(dfWii_product, aes(date, mean_price, colour = product_id)) + ###
  geom_line() +
  facet_wrap(~product_id) +
  ylab("Price(SEK)") +
  xlab("") +
  theme(legend.position = "none")

product_total_line_facet

ggplot(dfWii_product, aes(date, mean_price, colour = product_id)) +
  geom_point(alpha = 0.1)

product_total_smooth <- ggplot(dfWii_product, aes(date, mean_price, colour = product_id)) + ### 
  geom_smooth(alpha = 0.25) +
  labs(color = "Products (ID)") +
  ylab("Average price") +
  xlab("Year")

product_total_smooth

## retailer (store_id): mean price of products used
dfWii_retailer
head(dfWii_product)
summary(dfWii_product)
str(dfWii_product)

# some plots
retailer_total_line <- ggplot(dfWii_store, aes(date, mean_price, colour = store_id)) + ###
  geom_line() +
  labs(color = "Retailers (ID)") +
  ylab("Average price") +
  xlab("Year")

retailer_total_line

retailer_total_line_facet <- ggplot(dfWii_store, aes(date, mean_price, colour = store_id)) + ###
  geom_line() +
  facet_wrap(~store_id) +
  theme(axis.text.x = element_text(angle = 45, size = 10))+
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("Average price") 

retailer_total_line_facet








########## 2-3 by time scale ##########
### grouped by time scale (year > quarter > month > week > day)
## year
dfWii_year
head(dfWii_year)
summary(dfWii_year)
str(dfWii_year)

## quarter
dfWii_quarter
head(dfWii_quarter)
summary(dfWii_quarter)
str(dfWii_quarter)

## month
dfWii_month
head(dfWii_month)
summary(dfWii_month)
str(dfWii_month)

## week
dfWii_week
head(dfWii_week)
summary(dfWii_week)
str(dfWii_week)

## day
dfWii_day
head(dfWii_week)
summary(dfWii_week)
str(dfWii_week)







########## 3 main task ##########
########## 3-1 requested task 1 ##########

### pricing behaviors of retailers

# plot: total price trend
product_total_line
# df, plot: price trend (max, min, median, mean)

df_date_price_stats <- dfWii3 %>%
  select(date, price) %>%
  group_by(date)%>%
  summarize(mean_price = mean(price), min_price = min(price),
            max_price = max(price), median_price = median(price),
            sd_price = sd(price))

df_date_price_stats

# question: why the overall price is falling?
# find number of retailers introduced each year (competition)
# find number of products introduced each year (low cost products)


plot_date_price_stats_line <- 
  ggplot(df_date_price_stats, aes(x = date)) +
  geom_line(aes(y = mean_price,
                colour = "Mean"), size = 1.5 ) +
  geom_line(aes(y = min_price, 
                colour = "Min"), size = 0.8, alpha = 0.8) +
  geom_line(aes(y = max_price, 
                colour = "Max"), size = 0.8, alpha = 0.8 ) +
  geom_line(aes(y = median_price, 
            colour = "Median"), size = 0.8, alpha = 0.8) +
  ylim(150, 1000) +
  theme_bw() +
  scale_colour_manual("",
                     breaks = c("Max", "Min", "Mean", "Median"),
                     values = c("Max" = "tomato2",
                                "Min" = "gold2",
                                "Mean" = "blue1",
                                "Median" = "deepskyblue2")) +
  ylab("Price (SEK)") +
  xlab("Year")

plot_date_price_stats_line ###

plot_date_price_sd <- ggplot(df_date_price_stats, aes(date, sd_price)) +
  geom_smooth(se = FALSE)

plot_date_price_sd

# plot: number of products by time period
df_date_prod <- dfWii3 %>%
  select(date, product_id) %>%
  group_by(product_id) %>%
  mutate(Products = product_id)

df_date_prod

plot_product_observation_period <- 
  ggplot(df_date_prod, aes(date, fill = Products)) +
  geom_histogram(color = "grey") +
  ylab("Price observation (by product)") +
  xlab("Year")

plot_product_observation_period

ggplot(df_date_prod, aes(date, fill = product_id)) +
  geom_bar(stat = "count") +
  ylab("") +
  xlab("Year")


ggplot(df_date_prod, aes(date, color = product_id)) +
  geom_density()

# retailer analysis
retailer_total_line
retailer_total_line_facet #

df_date_retailer <- dfWii3 %>%
  select(date, store_id, product_id) %>%
  group_by(date, store_id) %>%
  mutate(Store = store_id)

head(df_date_retailer)

plot_retailer_observation_period <- ggplot(df_date_retailer, aes(date, fill = Store)) +
  geom_histogram(color = "grey", bins = 32) +
  ylab("Price observation (by store)")

plot_retailer_observation_period


# remove > 2014-01 & 2016-12
#dfWii4 <- dfWii3 %>%
#  select (date, year, week, product_id, store_id, price) %>%
#  filter(date >= "2014-01-01" & date <= "2016-12-31" )

# monthly (mean) data
#dfWii4_month <- dfWii3 %>%
#  select(product_id, date, price, store_id) %>%
#  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
#  group_by(year_month, product_id, store_id) %>%
#  summarize(mean_price = mean(price))

# testing
#df_date_retailer4 <- dfWii3 %>%
#  select(date, store_id, product_id) %>%
#  group_by(date, store_id)

#head(df_date_retailer4)

#ggplot(df_date_retailer4, aes(date, fill = store_id)) +
#  geom_histogram(color = "grey", bins = 36) +
#  ylab("Price observation (by store)") +
#  theme(axis.text.y = element_blank(),
#        axis.ticks.y = element_blank())



# dividing retailers into 
factor(retailer_list)
# retailer list: "6843", "20443", "11262", "18761", "428", "21333", "2151", 
#"1917", "28603", "12280", "135", "7088", "127", "12551", "53", "765", "13932", 
#"11703", "1260", "236", "164", "140", "12481", "21494", "5533", "32", "6276", 
#"3303", "578", "1", "12886", "9169", "16", "2395", "2354", "2085", "13676", 
#"8411", "525", "112", "3289", "1595"

# retailer entry 2013: 112 127 164 1260 3303
retailer_2013 <- dfWii3 %>%
  select(year, store_id) %>%
  filter(year == 2013)

retailer_2013 <- retailer_2013[!duplicated(retailer_2013), ]

head(retailer_2013)
factor(retailer_2013$store_id)

# retailer entry 2014: 
# retailers in 2014: 112 127 164 1260 2395 3303
retailer_2014 <- dfWii3 %>%
  select(year, store_id) %>%
  filter(year == 2014)

retailer_2014 <- retailer_2014[!duplicated(retailer_2014), ]

head(retailer_2014)
factor(retailer_2014$store_id)

# retailer entry 2015
# retailers in 2015: 16 32 112 127 164 236 428 578 765 1260 2151 2395 3303 
# 6276 6843 11262 12551 21333
retailer_2015 <- dfWii3 %>%
  select(year, store_id) %>%
  filter(year == 2015)

retailer_2015 <- retailer_2015[!duplicated(retailer_2015), ]

head(retailer_2015)
factor(retailer_2015$store_id)

# retailer entry 2016
# retailers in 2016:  6843  20443 11262 428   21333 2151  1917  28603 135   
# 7088  127   12551 53    765   1260  236  
# 164   140   12481 5533  32    6276  3303  578   1     12886 16    2395  
# 2085  13676 525   112 1595
retailer_2016 <- dfWii3 %>%
  select(year, store_id) %>%
  filter(year == 2016)

retailer_2016 <- retailer_2016[!duplicated(retailer_2016), ]

head(retailer_2016)
factor(retailer_2016$store_id)

# retailer entry 2017
# retailers in 2017:  6843  20443 11262 18761 428   21333 2151  1917  28603 
# 12280 135   7088  127   12551 53    765  13932 11703 1260  236   164   140   
# 12481 21494 5533  32    6276  3303  578   1     12886 9169 16    2395  2354  
# 2085  13676 8411  525   112   1595 
retailer_2017 <- dfWii2 %>%
  select(year, store_id) %>%
  filter(year == 2017)

retailer_2017 <- retailer_2017[!duplicated(retailer_2017), ]

head(retailer_2017)
factor(retailer_2017$store_id)


# number of products over time
product_by_year <- dfWii2 %>%
  select(date, product_id) %>%
  distinct()

number_of_products_over_time <- ggplot(product_by_year, aes(x = date)) +
  geom_step(stat = 'count')

number_of_products_over_time  

# number of retailers over time
retailer_by_year <- dfWii3 %>%
  select(date, store_id) %>%
  distinct()

number_of_retailers_over_time <- ggplot(retailer_by_year, aes(x = date)) +
  geom_step(stat = 'count')

number_of_retailers_over_time

# combined figure
num_of_ret_prod_over_time <- ggplot() +
  geom_step(data = product_by_year, aes(x = date, color = "Product"), 
            stat = 'count', size = 1) +
  geom_step(data= retailer_by_year, aes(x = date, color = "Retailer"), 
            stat = 'count', size = 1) +
  scale_color_manual("",
                     breaks = c("Product", "Retailer"),
                     values = c("Product" = "Orange",
                                "Retailer" = "Steelblue2")) +
  ylab("") +
  xlab("Year") + 
  ggtitle("Number of retailers/products over time")
  
  
  
plot_date_price_stats_line <- 
  ggplot(df_date_price_stats, aes(x = date)) +
  geom_line(aes(y = mean_price,
                colour = "Mean"), size = 1.5 ) +
  geom_line(aes(y = min_price, 
                colour = "Min"), size = 0.8, alpha = 0.8) +
  geom_line(aes(y = max_price, 
                colour = "Max"), size = 0.8, alpha = 0.8 ) +
  geom_line(aes(y = median_price, 
                colour = "Median"), size = 0.8, alpha = 0.8) +
  ylim(150, 1000) +
  theme_bw() +
  scale_colour_manual("",
                      breaks = c("Max", "Min", "Mean", "Median"),
                      values = c("Max" = "tomato2",
                                 "Min" = "gold2",
                                 "Mean" = "blue1",
                                 "Median" = "deepskyblue2")) +
  ylab("Price (SEK)") +
  xlab("Year")
  

# price seasonality by month
df2 <- dfWii2
df2$MonthN <- as.numeric(format(as.Date(df2$date),"%m")) 
df2$YearN <- as.numeric(format(as.Date(df2$date),"%Y"))
df2$Month  <- months(as.Date(df2$date), abbreviate = TRUE) 
df3 <- df2 %>%
  group_by(MonthN, YearN, Month) %>%
  summarize(price = mean(price)) %>%
  ungroup() %>%
  mutate(Month = factor(Month, levels = unique(Month)),
         YearN = as.factor(YearN))

price_seasonality <- ggplot(data = df3, 
       aes(x = Month, y = price, group = YearN, color = YearN)) + 
  geom_line(size = 0.8) + 
  labs(color = "Year")

price_seasonality


## 4 selected products for analysis
# product_1: 1293003 (a steady seller with low fluctuation)
prod_1 <- dfWii_month %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(product_id == "1293003")
prod_1

plot_prod_1 <- ggplot(prod_1, aes(year_month, mean_price, color = store_id)) +
  geom_line(size = 1)

plot_prod_1

# store selection: 1260, 127, 112, 3303, 428, 12886
prod_1_store_selected <- prod_1 %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(store_id == "1260" |
           store_id == "127" |
           store_id == "112" |
           store_id == "3303" |
           store_id == "428" |
           store_id == "12886" )

prod_1_store_selected

plot_prod_1_selected_stores <- ggplot(prod_1_store_selected, 
                                      aes(year_month, mean_price, 
                                          color = store_id)) +
  geom_line(size = 1) +
  labs(color = "Retailers") + 
  ylab("Price (SEK)") + 
  xlab("Year") +
  ggtitle("Sample product: 1293003")

plot_prod_1_selected_stores


# product_2: 1997675 (a steady seller with fluctuation in late period)
prod_2 <- dfWii_month %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(product_id == "1997675")

prod_2

plot_prod_2 <- ggplot(prod_2, aes(year_month, mean_price, color = store_id)) +
  geom_line(size = 1)

plot_prod_2

# store selection: 1260,  112, 3303,  12886, 1917
prod_2_store_selected <- prod_2 %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(store_id == "1260" |
           store_id == "112" |
           store_id == "3303" |
           store_id == "12886" |
           store_id == "1917")

prod_2_store_selected

plot_prod_2_selected_stores <- ggplot(prod_2_store_selected, 
                                      aes(year_month, mean_price, 
                                          color = store_id)) +
  geom_line(size = 1)  +
  labs(color = "Retailers") + 
  ylab("Price (SEK)") + 
  xlab("Year") +
  ggtitle("Sample product: 1997675")

plot_prod_2_selected_stores


# product_3: 2716529 (relatively newer product with fluctuations)
prod_3 <- dfWii_month %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(product_id == "2716529")

prod_3

plot_prod_3 <- ggplot(prod_3, aes(year_month, mean_price, color = store_id)) +
  geom_line(size = 1)

plot_prod_3

# store selection: 1260, 112, 3303, 127 
prod_3_store_selected <- prod_3 %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(store_id == "1260" |
           store_id == "112" |
           store_id == "3303" |
           store_id == "127"
         )

prod_3_store_selected

plot_prod_3_selected_stores <- ggplot(prod_3_store_selected, 
                                      aes(year_month, mean_price, 
                                          color = store_id)) +
  geom_line(size = 1) +
  labs(color = "Retailers") + 
  ylab("Price (SEK)") + 
  xlab("Year") +
  ggtitle("Sample product: 2716529") 

plot_prod_3_selected_stores


# product_4: 2716553 (relatively newer product with steady trend)
prod_4 <- dfWii_month %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(product_id == "2716553")

prod_4

plot_prod_4 <- ggplot(prod_4, aes(year_month, mean_price, color = store_id)) +
  geom_line(size = 1)

plot_prod_4

# store selection: 120, 1917, 3303, 12551, 12886, 21494, 236, 428, 32, 112, 117
prod_4_store_selected <- prod_4 %>%
  select(year_month, product_id, store_id, mean_price) %>%
  filter(store_id == "120" |
           store_id == "1917" |
           store_id == "12551" |
           store_id == "12886" |
           store_id == "21494" |
           store_id == "428" |
           store_id == "32" |
           store_id == "112" |
           store_id == "117" )

prod_4_store_selected

plot_prod_4_selected_stores <- ggplot(prod_4_store_selected, 
                                      aes(year_month, mean_price, 
                                          color = store_id)) +
  geom_line(size = 1)+
  labs(color = "Retailers") + 
  ylab("Price (SEK)") + 
  xlab("Year") +
  ggtitle("Sample product: 2716553")

plot_prod_4_selected_stores





########## 3-2 requested task 2 ##########
retailer_total_line_facet
retailer_total_line

# removing retailers entered in 2017
entry_date_check <- dfWii3 %>%
  select(date, store_id) %>%
  group_by(store_id) %>%
  summarize(entry_date = min(date)) %>%
  filter(entry_date >= "2017-01-01")
  
factor(entry_date_check$store_id)

# retailers entered in 2017: 2354 8411 9169 11703 12280 13932 18761 21494
# new dataset with those 8 retailers removed

str(dfWii3)
str(dfWii5)
dfWii5 <- dfWii3[!(dfWii3$store_id == 2354 |
                     dfWii3$store_id == 8411 |
                     dfWii3$store_id == 9169 |
                     dfWii3$store_id == 11703 |
                     dfWii3$store_id == 12280 |
                     dfWii3$store_id == 13932 |
                     dfWii3$store_id == 18761 |
                     dfWii3$store_id == 21494 
                     ), ]
factor(unique(dfWii5$store_id))

dfWii5_store_mean_date <- dfWii5 %>%
  select(date, store_id, product_id, price) %>%
  group_by(date, store_id) %>%
  summarize(mean_price = mean(price))

dfWii5_store_mean_month <- dfWii5_store_mean_date %>%
  select(date, store_id, mean_price) %>%
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month, store_id) %>%
  summarize(monthly_mean = mean(mean_price))

dfWii5_prod_sample <- dfWii5 %>%
  select(date, store_id, product_id, price) %>%
  filter(product_id == 1293003) # sample product name


# data frame using store (product mean) price
dfWii5_store_mean_date
dfWii5_store_mean_month
# data frame using sample product
str(dfWii5_prod_sample)


# data frame reshape
df_clust <- dfWii5_store_mean_date %>% 
  spread(store_id, mean_price)
df_clust2 <- df_clust[, -1]
df_clust3 <- t(df_clust2)

df_clust_m <- dfWii5_store_mean_month %>% 
  spread(store_id, monthly_mean)
df_clust_m2 <- df_clust_m[, -1]
df_clust_m3 <- t(df_clust_m2)

df_clust_p <- dfWii5_prod_sample %>%
  spread(store_id, price) 
df_clust_p <- df_clust_p[, -1]
df_clust_p2 <- df_clust_p[, -1]
df_clust_p3 <- t(df_clust_p2)

# data frame for clustering
df_clust3 # date scale (store mean)
df_clust_m3 # month scale (store mean)
df_clust_p3 # date scale (sample product)

# method - 1
# Add new distance matrix calculation

dtwOmitNA <-function (x,y)
{
  a<-na.omit(x)
  b<-na.omit(y)
  return(dtw(a,b,distance.only=TRUE)$normalizedDistance)
}
pr_DB$set_entry(FUN = dtwOmitNA, names = c("dtwOmitNA"))

# date scale store mean
distances <- dist(df_clust3, method = "dtwOmitNA")
hc <- hclust(distances)
plot(hc,
     cex = 0.8,
     hang = -0.1)
rect.hclust(hc, k = 4)

# month scale store mean
distances_2 <- dist(df_clust_m3, method = "dtwOmitNA")
hc2 <- hclust(distances_2)
plot(hc2,
     cex = 0.8,
     hang = -0.1)
rect.hclust(hc2, k = 5)

# date scale sample product
distances_3 <- dist(df_clust_p3, method = "dtwOmitNA")
hc3 <- hclust(distances_3)
plot(hc3,
     cex = 1,
     hang = -1,
     main = "Retailer Hierachy Cluster",
     sub = "Product 1293003",
     ylab = "Height",
     xlab = "Retailers")
rect.hclust(hc3, k = 3)

# cluster decided
# monthly price & sample product (1293003)

# new data set with cluster
head(dfWii5)

df_clusted <- dfWii5 %>%
  select(date, year, product_id, store_id, price) %>%
  filter(product_id == 1293003)

# group_A = c(135, 1260, 1917, 20443)
# group_B = c(32, 6843, 140, 12886)
# group_C = c(112, 3303, 127, 428, 2085, 1595, 2395)

df_clusted$cluster <- ifelse(
  df_clusted$store_id == 135 |
    df_clusted$store_id == 1260 |
    df_clusted$store_id == 1917 |
    df_clusted$store_id == 20443, "A", ifelse(
      df_clusted$store_id == 32 |
        df_clusted$store_id == 6843 |
        df_clusted$store_id == 140 |
        df_clusted$store_id == 12886, "B", ifelse(
          df_clusted$store_id == 112 |
            df_clusted$store_id == 3303 |
            df_clusted$store_id == 127 |
            df_clusted$store_id == 428 |
            df_clusted$store_id == 2085 |
            df_clusted$store_id == 1595 |
            df_clusted$store_id == 2395, "C", NA
        )))

# data clustering
head(df_clusted)
cluster_price <- df_clusted %>%
  select(date, year, store_id, price, cluster)%>%
  group_by(date, cluster) %>%
  summarise(group_mean = mean(price))

# plotting
ggplot(cluster_price, aes(date, group_mean, group = cluster, color = cluster)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Price (SEK)") +
  labs("Cluster")

ggplot(cluster_price, aes(date, group_mean, group = cluster, color = cluster)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Price (SEK)") +
  labs("Cluster") +
  scale_x_date(limit=c(as.Date("2015-01-01"),as.Date("2016-12-31"))) +
  ylim(450, 700)
  
plot_cluster_price <- ggplot() +
  geom_line(data = cluster_price, 
            aes(x = date, y = group_mean, color = cluster), size = 1) +
  geom_line(data = df_clusted, 
            aes(x = date, y = price, color = cluster), alpha = 0.25) + 
  ylab("Price (SEK)") +
  labs("Cluster") +
  theme_bw() +
  ggtitle("Average prices of retailer clusters (product 1293003)")

plot_cluster_price_short <- ggplot() +
  geom_line(data = cluster_price, 
            aes(x = date, y = group_mean, color = cluster), size = 1) +
  geom_line(data = df_clusted, 
            aes(x = date, y = price, color = cluster), alpha = 0.25) + 
  ylab("Price (SEK)") +
  labs("Cluster") +
  theme_bw() +
  scale_x_date(limit=c(as.Date("2014-10-01"),as.Date("2016-12-31"))) +
  ylim(350, 700) +
  ggtitle("Average prices of retailer clusters (product 1293003)")

plot_cluster_price
plot_cluster_price_short

## another attempt with different sample product (2014120)
dfWii5_prod_sample_x <- dfWii5 %>%
  select(date, store_id, product_id, price) %>%
  filter(product_id == 2014120) # sample product name

df_clust_p_x <- dfWii5_prod_sample_x%>%
  spread(store_id, price) 
df_clust_p_x <- df_clust_p_x[, -1]
df_clust_p2_x <- df_clust_p_x[, -1]
df_clust_p3_x <- t(df_clust_p2_x)

distances_3_x <- dist(df_clust_p3_x, method = "dtwOmitNA")
hc3_x <- hclust(distances_3_x)
plot(hc3_x,
     cex = 1,
     hang = -1,
     main = "Retailer Hierachy Cluster",
     sub = "Product 2014120",
     ylab = "Height",
     xlab = "Retailers")
rect.hclust(hc3_x, k = 4)

df_clusted_x <- dfWii5 %>%
  select(date, year, product_id, store_id, price) %>%
  filter(product_id == 2014120)

df_clusted_x$cluster <- ifelse(
  df_clusted_x$store_id == 135 |
    df_clusted_x$store_id == 1260, "A", ifelse(
      df_clusted_x$store_id == 140 |
        df_clusted_x$store_id == 236 |
        df_clusted_x$store_id == 32 |
        df_clusted_x$store_id == 578, "B", ifelse(
          df_clusted_x$store_id ==  1917|
            df_clusted_x$store_id == 2085 |
            df_clusted_x$store_id == 127 |
            df_clusted_x$store_id == 765, "C", ifelse(
              df_clusted_x$store_id == 112 |
                df_clusted_x$store_id == 3303 |
                df_clusted_x$store_id == 428 |
                df_clusted_x$store_id == 6843 |
                df_clusted_x$store_id == 12886 |
                df_clusted_x$store_id == 12481 |
                df_clusted_x$store_id == 1595 |
                df_clusted_x$store_id == 2395, "D", NA))))

cluster_price_x <- df_clusted_x %>%
  select(date, year, store_id, price, cluster)%>%
  group_by(date, cluster) %>%
  summarise(group_mean = mean(price))
          
plot_cluster_price_x <- ggplot() +
  geom_line(data = cluster_price_x, 
            aes(x = date, y = group_mean, color = cluster), size = 1) +
  geom_line(data = df_clusted_x, 
            aes(x = date, y = price, color = cluster), alpha = 0.25) + 
  ylab("Price (SEK)") +
  labs("Cluster") +
  theme_bw() +
  ggtitle("Average prices of retailer clusters (product 2014120)")

plot_cluster_price_x


########## 4 summary ##########

## preliminary analysis
# product overview (boxplot)
product_overview_box
# product overview (boxplot - outliers removed)
product_boxplot_without_outliers

# products average prices
product_overview_mean 
# products average prices (outliers removed)
product_mean_without_outliers

# products standard deviation
product_overview_sd
# products standard deviatio (outlier removed)
product_sd_without_outliers

# moving average price (all products)
product_total_line
# moving average price (all products - with facet)
product_total_line_facet

# moving average price (all retailers)
retailer_total_line
# moving average price (all retailers - with facet)
retailer_total_line_facet

## main tasks
# moving price (max, min, mean, median)
plot_date_price_stats_line

# product observation
plot_product_observation_period
# number of products and retailers in observation period
num_of_ret_prod_over_time

# product seasonality
price_seasonality

# retailers' moving price (sample products)
plot_prod_1_selected_stores # product 1293003
plot_prod_2_selected_stores # product 1997675
plot_prod_3_selected_stores # product 2716529
plot_prod_4_selected_stores # product 2716553

## hierarchy cluster
# sample product 1: 1293003
plot(hc3,
     cex = 1,
     hang = -1,
     main = "Retailer Hierachy Cluster",
     sub = "Product 1293003",
     ylab = "Height",
     xlab = "Retailers")
rect.hclust(hc3, k = 3)
# average moving price of clusters (sample product 1)
plot_cluster_price

# sample product 2: 2014120
plot(hc3_x,
     cex = 1,
     hang = -1,
     main = "Retailer Hierachy Cluster",
     sub = "Product 2014120",
     ylab = "Height",
     xlab = "Retailers")
rect.hclust(hc3_x, k = 4)
# average moving price of clusters (sample product 2)
plot_cluster_price_x
