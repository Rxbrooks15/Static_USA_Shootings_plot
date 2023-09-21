
# Intro + Context ---------------------------------------------------------


#Continuation of US_shootings_recreation
#Potential mapping plots
#stacked barchart

#Deeper case studies on regions of interest and their severity of gun violence 
#Do causes of mass shootings mean something in terms of its prevelence ?

#Proj 2
## Data Visualization (QSS19 Spring 2023)
## Name: Raine Brookshire
#

# Original plot 
#meant to recreate and improve
#https://i.redd.it/flj6volxynqa1.png

#recreation of 
#https://www.statista.com/statistics/811541/mass-shootings-in-the-us-by-state/
#4th plot 
#and/ or 
#https://worldpopulationreview.com/state-rankings/mass-shootings-by-state

# created tot_shooings for number of shootings and tot_victims for number of victims 
#Data on mass shootings 

# Libraries ---------------------------------------------------------------


library(readxl)
library(tidyverse)

library(gganimate)
library(readr)
library(readxl)
library(gridExtra)
# install.packages("rvest")
library(rvest)

library(sf)
library(sp)
library(magick)
library(jpeg)
library(lubridate)
library(urbnmapr)
library(cr)
install.packages(cr)
library(gifski)
library(ggtext)
library(ggplot2)
library(usmap)
library(ggstream)
library(maps)
library(ggmap)
library(googleway)

install.packages("googleway")
install.packages("ggmap")
install.packages("urbnmapr")
install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")

7+0


# Data exploration + loading data ------------------------------------------------------
shootings <- read_csv(file.choose())
file.choose()
#shootings
"C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\Mass_shootings\\Mass Shootings Dataset Ver 5.csv"

#Mass shootings dataset 5
shootings <- read_csv("C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\Mass_shootings\\Mass Shootings Dataset Ver 5.csv")
shootings <- shootings%>% 
  group_by(State) %>% 
  count()

#run
shootings <- shootings %>%
  mutate(year = substr(Date, start = nchar(Date) - 3,
                       stop = nchar(Date))) %>% 
  # filter(year >= 2009) %>% 
  mutate(ordered_date = mdy(Date)) %>% 
  mutate(race_group = case_when(
    Race %in% c("white", "White American or European American") ~ "White",
    Race == "White American or European American/Some other Race" ~ "White",
    Race %in% c("Asian American", "Asian American/Some other race") ~ "Asian",
    Race %in% c("black","Black American or African American", "Black", "Black American or African American/Unknown") ~ "Black",
    Race %in% c("Some other race", "Two or more races", "Unknown", NA) ~ "Other",
    Race == "Native American or Alaska Native" ~ "Native",
    TRUE ~ Race
  ))


  
top_states <- shootings %>%
  arrange(desc(n)) %>%
  top_n(4, n)
View(top_states)

new_shootings <- read_csv(file.choose())  
file.choose()

shootings <- read_csv("C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\Mass_shootings\\Mass Shootings Dataset Ver 5.csv")

#Mother Jones mass shootings database
new_shootings <- read_csv("C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\Mass_shootings\\Mother Jones - Mass Shootings Database, 1982 - 2023 - Sheet1.csv")



shootings <- shootings[complete.cases(shootings[c("Latitude", "Longitude")]),]
shootings_sf <- st_as_sf(shootings, coords = c("Longitude", "Latitude"), crs = 4326)

# 
# new_shootings <- new_shootings[complete.cases(new_shootings[c("latitude", "longitude")]),]
# Removes rows with dashes in latitude or longitude columns
new_shootings <- new_shootings[!(new_shootings$latitude == "-" | new_shootings$longitude == "-"), ]
new_shootings_sf <- st_as_sf(new_shootings, coords = c("longitude", "latitude"), crs = 4326)


###
#important
shootings <- shootings %>%   
  separate(Location, into = c("City", "State"), sep = ",", remove = FALSE) %>%
  mutate(State = str_trim(State))


new_shootings <- new_shootings%>% 
  separate(location...2, into = c("City", "State"), sep = ",", remove = FALSE) %>%
  mutate(State = str_trim(State)) %>% 
  group_by(State)
shootings %>% 
  View
new_shootings %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude))

#number of shootings pers state
shootings %>% 
  group_by(State) %>%
  count() %>%
  top_n(4, n) %>%
  arrange(desc(n))

shootings
#important
#2019 -2023

new_shootings %>% 
  group_by(State) %>%
  count() %>%
  top_n(4, n) %>%
  arrange(desc(n))

# Filtering + data manipulation  ------------------------------------------
shootings_cal <- shootings_sf %>% 
  filter(State == "California")
shootings_cal

new_shootings_cal <- new_shootings_sf %>% 
  filter(State == "California")
new_shootings_cal
4+5

shootings_tex <- shootings_sf %>% 
  filter(State == "Texas")
shootings_tex

shootings_texas <- new_shootings %>% 
  filter(State == "Texas")
shootings_texas


new_shootings_tex <- new_shootings_sf %>% 
  filter(State == "Texas")
new_shootings_tex


shootings_flo <- shootings_sf %>% 
  filter(State == "Florida")
shootings_cal

new_shootings_flo <- new_shootings_sf %>% 
  filter(State == "Florida")
new_shootings_flo




# Shapefiles + Visualization ----------------------------------------------


Cal <- st_read(file.choose())#CA_counties
Tex <- st_read(file.choose())#Tex_counties
Flo<- st_read(file.choose())#Florida


Cal_plot
#working code 
Cal_plot <- shootings %>% 
  filter(State == "California") %>% 
  ggplot() +
  geom_sf(data = Cal, fill = "#012564", color = "white")+
  # geom_point(aes(x = Longitude, y = Latitude, size = `Total victims`),
  #            color = "red")
  # geom_point(data = shootings, aes(x = Longitude, y = Latitude, size = `Total victims`, color = `Total victims`)) +
  geom_sf(data = shootings_cal,aes(size = as.numeric(`Total victims`)),
                                   color = "red", alpha = 0.51)+
  geom_sf(data = new_shootings_cal,aes(size = as.numeric(total_victims)), color = "green", alpha = 0.7)+
 
  # geom_point(data = combined_cali,
  #            aes(x = longitude, y = latitude), color = "blue") +
  theme_void()+
  labs(title = "Shootings Based in California", subtitle = "the red dots are from 1980-2017 and
       the green dots are from 2018-2023",
       size = "Total Victims")



Cal_plot

Cal_plot

# shootings %>% 
#   filter(State == "Texas") %>% 
#   ggplot()+
#   geom_sf(data = Tex, fill = "#012564", color = "white")+
#   geom_point(aes(x = Longitude, y = Latitude, size = `Total victims`),
#              color = "red")



class(shootings$`Total victims`)
new_shootings$total_victims <- as.numeric(new_shootings$total_victims)


#working code 
Tex_plot <- shootings %>% 
  filter(State == "Texas") %>% 
  ggplot() +
  geom_sf(data = Tex, fill = "#012564", color = "grey") +

  geom_point(aes(x = Longitude, y = Latitude, size = `Total victims`),
             color = "red")+
  geom_sf(data = new_shootings_tex, aes(size = as.numeric(total_victims)), color = "green", alpha = 0.5)+
  labs(title = "Why do these 3 states have numerous mass shootings?
  Relating gun sales and internal causes to the gross gun violence in 
  these 3 populous states ",subtitle = "Shootings Based in Texas", size = "Total Victims")+
  theme_void()
  # geom_point(data = shootings_texas, aes(x = longitude, y = latitude),
  #            color = "green")

5+5
Tex_plot
Tex_plot

class(new_shootings_tex$total_victims)
class(new_shootings$total_victims)

#working code 
Flo_plot <- shootings %>% 
  filter(State == "Florida") %>% 
  ggplot() +
  geom_sf(data = Flo, fill = "#012564", color = "grey") +
  
  # geom_point(aes(x = Longitude, y = Latitude, size = `Total victims`),
  #            color = "red")
  
  geom_sf(data = shootings_flo,aes(size = as.numeric(`Total victims`)), color = "red", alpha = 0.56)+
  geom_sf(data = new_shootings_flo, aes(size = as.numeric(total_victims)), color = "green",inherit.aes = FALSE, alpha = 0.7)+
  # geom_point(data = combined_cali,
  #            aes(x = Longitude, y = Latitude), color = "blue") +
  theme_void()+
  labs(title = "Shootings Based in Florida", size = "Total Victims")

Flo_plot

#important checkpoint
combined_plots <- grid.arrange(Cal_plot, Tex_plot, Flo_plot, ncol = 3)
combined_plots
combined_plots


# Context +3 visuals ------------------------------------------------------


#plot of points only till the year 2017
shooting_number <- shootings %>% 
  filter(!is.na(State)) %>% 
  group_by(State) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot(aes(x = reorder(State, n), y = n))+
  geom_point()+
  coord_flip()+
  labs(x = "States", y = "Number of shootings", title = "Total number of shootings based on State")+
  theme_minimal()
#from new dataset to 2022
new_total <- new_shootings %>% 
  group_by(State) %>% 
  count() %>% 
  #Used so the two shooting datasets have the same number of rows 
  arrange(desc(n)) %>%
  head(30) %>% 
  rename(num = n)
new_total

shooting_number
# Number of shootings per state
total_shootings <- shootings %>%
  filter(!is.na(State)) %>%
  group_by(State) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(30)


  mutate(total_count = n + new_total$num)

#important
tot_shootings <- left_join(total_shootings, new_total, by = "State") %>%
  mutate(total_count = n + num)

#Gets the total number of shootings per state
tot_shootings <- tot_shootings %>% 
  filter(!is.na(State)) %>% 
  arrange(desc(total_count)) %>%
  head(20) %>% 
  
  ggplot(aes(x = reorder(State, total_count), y = total_count))+
  geom_point()+
  coord_flip()+
  labs(x = "States", y = "Number of shootings", title = "Total number of shootings based on State")+
  theme_minimal()
  
tot_shootings

#good graph 
tot_victims <- shootings %>%
  filter(!is.na(Cause)) %>%
  filter(!is.na(State)) %>% 
  group_by(State) %>%
  summarize(total_victims = sum(`Total victims`), count = n()) %>%
  #Highlights desired bars 
  mutate(highlight = case_when(
    State %in% c("Florida", "Texas", "California") ~ "1",
    TRUE ~ "0")) %>% 
  
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = reorder(State, -total_victims), y = total_victims, fill = highlight))+
  # guides(legend = "none")+
  scale_fill_manual( values = c( "1"="green", "0"="darkblue" ),
                     guide = "none" )+
  theme(legend.position = "none")+
  coord_flip()+
  labs(x = "States", y = "Total Victims",
       title = "Total victims per state",
       subtitle = "*This graph details the severity of the mass shootings")+
  theme_minimal()
tot_victims
tot_victims


shootings$State[which(shootings$State=='NV')] <- 'Nevada'
shootings$State[which(shootings$State=='CA')] <- 'California'

shootings$Cause[which(shootings$Cause=='frustration')] <- 'anger/ frustration'
shootings$Cause[which(shootings$Cause=='anger')] <- 'anger/ frustration'
shootings$Cause[which(shootings$Cause=='anger/frustration')] <- 'anger/ frustration'
#Frustration is a form of anger

shootings$Cause[which(shootings$Cause=='psycho')] <- 'psychotic episode'
#this is a better suited name

#Trying to find the top 5 most common causes of shootings
shootings %>% 
  distinct(Cause)

#good code
shoot <- shootings %>%
  filter(!is.na(Cause)) %>% 
  #most common causes 
  filter(Cause %in% c("anger/ frustration", 
                      "domestic dispute",
                      "terrorism",
                      "psychotic episode",
                      "revenge"
                      )) %>%
  filter(!is.na(State)) %>% 
  #Meant to highlight 3 selected states on y axis 
  mutate(highlight = case_when(
    State %in% c("Florida", "Texas", "California") ~ "1",
    TRUE ~ "0")) %>% 

  group_by(State, Cause, `Total victims`) %>%
  summarize(count = n()) %>% 
  ggplot() +
  geom_col(aes(x = `Total victims`, y =  State, fill = Cause))+

  theme_minimal()+
  labs(y = "States", title = "Main causes for mass shootings", subtitle = "The dashed line indicates a cutt off 
       where the 3 states are included ")+
  #Creates dashed line 
  geom_vline(xintercept = 100, linetype = "dashed", color = "red")
  #Meant to highlight only Florida, Texas and California in Red 
  # geom_text(data = filter(shoot, highlight == "1"), aes(x = 100, y = State, label = State),
  #           vjust = -0.5, color = "red", size = 4)
  # scale_y_discrete(labels = function(x) ifelse(x %in% c("Florida", "Texas", "California"),
  #                                              paste0("<span style='color:red'>", x, "</span>"), x)) +
  # theme(axis.text.y = element_text(color = "red"))
  #ifelse(shootings$highlight == "2", "green", "black")))                                  
  # scale_y_discrete(labels = function(x) {
  #   ifelse(x %in% c("Florida", "Texas", "California"), expression(bold(red(x))), x)
  # })


shoot

shootings %>% 
  group_by(Cause) %>%
  count()

#We see psycho, terrorism and anger as the main culprits of gun violence 
#In order not to have too many variables for the stacked barchart 
# I can take the top 5 most common causes
  
  distinct(Cause)

 shootings %>%
  filter(!is.na(Cause)) %>% 
  # filter(State %in% c("California", "Texas", "Florida")) %>%
  group_by(State,`Total victims`) %>%
  summarize(count = n())
  ggplot() +
  geom_point(aes(x = `Total victims`, y = State))
  

  
#We already know that Cali, Tex, and Flo are some of the largest states but even  their high population 
#contributes to their gun violence I dont think it is the cheif reason As many populous countries are not crippled with gun violence 
  
#Maybe I can look at gun sales to determine why Flo, Cali, and tex have so many mass shootings 


#nics - latest file
#gunsales data
gun_sales <- read_csv("C:\\Users\\raine\\OneDrive\\Documents\\Adv_data_viz\\nics-latest.csv")

gun_sales
# looking at the values for adjusted and unadjusted it appears adjusted would work better for my research 
#I'd like to create a simple plot to see if high gun sales signify high gun violence especially in my 3 states of interest 

#good code 
gun_sales <- gun_sales %>% 
  filter(state != "United States") %>% 
  group_by(state) %>%
  summarise(total_adjusted = sum(total_adjusted)) %>% 
  mutate(total_adjusted = total_adjusted/1000) %>% 
  mutate(highlight = case_when(
    state %in% c("Florida", "Texas", "California") ~ "1",
    TRUE ~ "0")) %>% 

  #Gets rid of Hawaii and other states with low or no gun sales 
  filter(total_adjusted > 10) %>% 
  
  ggplot()+
  geom_col(aes(x = reorder(state, -total_adjusted), y = total_adjusted, fill = highlight))+
  scale_fill_manual( values = c( "1"="green", "0"="darkblue" ),
                     guide = "none" )+
  # scale_fill_manual(values = c(rep("steelblue", length(unique(gun_sales$state)) - 1), "orange")) +
  coord_flip()+
  theme_minimal()+
  labs(title = "Gun sales from 2000-2023 in the US",
       y = "Total firearms sold in thousands (handgun and long guns)", 
       x = "States")

gun_sales


# Final combination of plots + comments -----------------------------------

####
####
#important
combined_plots <- grid.arrange(Cal_plot, Tex_plot, Flo_plot,tot_victims,shoot,gun_sales, ncol = 3)
##
combined_plots
combined_plots

#Plot tells the story of the prominent rise of mass shootings after 2017. Not only are they getting more lethal and 
# taking more lives but in the large states where I focued my case study the greater purchase of these weapons 
#has some relation to the increase in the killings. Gun sales, prominent mental issues and terrorist attacks also have 
#great impact on the mass shooting numbers... Note the 1st graph in the second row is meant to demonstrate severity of the shootings

#Challenges:figuring out whether to use sf dataframe with geom_sf or regular data frame with geom_point to map the points
#using geom_point in certain cases mapps the coords incorrectly. Had to switch shapefiles several times only to realize it was a problem with the function 
#and not the file.
#Next: Either combining the dataframes or using them separately to plot on maps. First joined data by State but wasnt sure whether to join by 
#Long and Lat, or date column. Ended up working with data separately as NA values were hard to work with 
#Next: Figuring out how to set up points to depend on size. In this case I had to filter values and try not to get the point sizes to overlap due to larger points 
#next: had problems choosing and finding right shapefile for Florida that aligned with the others. Found one but it wasn't parallel and completely vertical
#next Hard to add unique title on top of all the combined layers of plots. Learned to combine more that 3 plots (Took lots of space and time to run )

