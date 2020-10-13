## Data Visualization (GOV 16 - QSS17) Fall 2017
## Lab 3 
##
## Name: James Wen
## Date: Nov 1, 2017 

# Initial Settings --------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggthemes)
library(ggmap)
library(albersusa)
options(stringsAsFactors = FALSE)

# Load Medicare Data ---------------------------------------------------------------

df1 <- read.csv("data/Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv")

# Load Map Data from albersusa --------------------------------------------

us.states <- usa_composite()
us.states.map <- fortify(us.states, region = "name")  # makes data usable for geom_polygon

# Wrangle Medicare Data ---------------------------------------------------

df2 <- df1 %>% 
  select(Provider.State, Average.Medicare.Payments) %>% 
  mutate(average_medicare_payments = gsub("\\$", "", Average.Medicare.Payments),
         average_medicare_payments = gsub("\\,", "", average_medicare_payments),
         average_medicare_payments = as.numeric(average_medicare_payments)) %>% 
  group_by(Provider.State) %>% 
  summarize(average_medicare_payments = mean(average_medicare_payments)) %>% 
  ungroup() %>% 
  mutate(average_medicare_payments = round(average_medicare_payments, 2)) %>% 
  rename(id = Provider.State) 

# Wrangle Map Data --------------------------------------------------------

url <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
state_abbreviations <- read.csv(url) %>% 
  rename(id = Abbreviation) %>% 
  mutate(State = tolower(State)) %>% 
  rename(region = State)

us.states.map <- us.states.map %>% 
  mutate(id = tolower(id)) %>% 
  rename(region = id)

state_data <- left_join(us.states.map, state_abbreviations, by = "region")

# Combine Map Data and Medicare Data --------------------------------------

final_data <- left_join(state_data, df2, by = "id")

# Make a Graph ------------------------------------------------------------

ggplot(data = final_data) +
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = average_medicare_payments),
               size = 0.3,
               color = "white") +
  scale_fill_gradient(high = "blue", low = "light blue") +
  coord_fixed(ratio = 1.4) +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.text = element_text(size = 9)) +
  scale_y_continuous(breaks = FALSE) + 
  scale_x_continuous(breaks = FALSE) +
  labs(
    title = "Average Medicare Payments per State",
    subtitle = "Data Source: Data.gov",
    fill = "Payments, in US Dollar",
    x = "",
    y = "") 
