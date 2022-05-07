rm(list=ls())    # clear the workspace

############ Temur Gugushvili ###########
############ Russia visitors trips to Georgia for AskGov.ge Prject ###########


#Load library 

library(readxl)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(waffle)
library(magrittr)
library(dplyr)


#Import data

VisitorsTrips <- readr::read_csv("/Users/macbook/Documents/test/shiny/tourism/app/data/tourism_data.csv")
Additional <- readr::read_csv("Data/Additional.csv")


VisitorsTripsYear <- VisitorsTrips %>%
  separate(Year_Month, c('Year', 'Month'))  %>%
transform(Year = as.numeric(Year)) 
#  filter(Month == "01" | Month == "02" | Month == "03")

RussiaTrips <- VisitorsTripsYear %>%
  group_by(Year) %>%
  mutate(TotalTrips = sum(Trips)) %>%
  filter(Country == "Russia") 
  
RussiaTripsSelect <- RussiaTrips  %>% 
  group_by(Year, TotalTrips) %>%
  summarise(RussiaTripsYearly = sum(Trips)) %>%
  mutate(Rest = TotalTrips - RussiaTripsYearly) %>%
  mutate(Percentage = round(RussiaTripsYearly/TotalTrips*100, 1)) %>%
  pivot_longer(cols = starts_with("R"), names_to = "Entries", values_to = "Number", values_drop_na = TRUE)  %>%
  arrange(desc(Year), Entries) %>%
  add_column( x = rep(c(1, 2), 12))


RussiaTripsSelect$Month_label = paste0(RussiaTripsSelect$Percentage, " %") %>% fct_inorder

TripsGeorgia <- ggplot(RussiaTripsSelect, aes(x=Entries, y=Number)) + 
  geom_col(aes(fill= Entries ), width = 1, alpha = 0.8)+
  coord_polar(theta = "x", direction = -1) +
  geom_text(data = RussiaTripsSelect %>% filter(Entries == "RussiaTripsYearly"),
            aes(label = Month_label, y = Number - 10),
            hjust = 0, nudge_x = 0.05) +
  theme_void()+
  theme(plot.background = element_rect(fill = "White", colour = "White"),
    legend.position = "bottom", 
    strip.text = ggtext::element_textbox(size = 11,
    color = "white", fill = "#5D729D", box.color = "#4A618C",
    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))+
  labs(title = "The Russia",
    caption = "Source : Information Centre, Information and Analytical Department, Ministry of Internal Affairs of Georgia | Author:Temur Gugushvili",
       subtitle ="test",
       x = "",
       y = "")+
  scale_fill_manual(values=c("#808080", "#8a0303"))+
  facet_wrap(vars(Year))


ggsave("Visualization/TripsGeorgia.png", 
       TripsGeorgia, 
       width =40, 
       height = 20, 
       units = "cm", 
       dpi = 400)


## რუსეთ-უკრაინის დაწყებიდან 25 დღის შემდეგ  35,028 რუსეთის მოქალაქემ შემოვიდა საქართველოში
## რუსეთის მოქალაქეებიისთვის საქართველოს ტერიტორიაზე ჩამოსვლა პოულარული 2018-2019 წელნში 
## პირველი 04 თვის მონაცემების მიხედვით მიუხედავად იმისა, რომ გაიზარდა საქართველოში რუსეთის მოაქილაქეების პროცენტული წილი ადრე 

#24.02.2022 - 20.03.2022 (25 დღე) რუსეთის ფედერაცია 35 028

#write.csv(RussiaTrips,"Data/RussiaTrips.csv", row.names = FALSE)

