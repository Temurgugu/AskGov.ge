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
library(ggtext)
library(gridExtra)
library(readxl)
library(grid)
library(patchwork)
library(httr)

#Import data

VisitorsTrips <- readr::read_csv("/Users/macbook/Documents/test/shiny/tourism/app/data/tourism_data.csv")

VisitorsTripsYear <- VisitorsTrips %>%
  separate(Year_Month, c('Year', 'Month'))  %>%
transform(Year = as.numeric(Year)) 

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
  theme(plot.title = element_markdown(size=20, colour="#035f8a", hjust=0),
    plot.subtitle = element_markdown (size = 11, lineheight=1.3),
    plot.caption =     element_markdown (size = 8),
    plot.background = element_rect(fill = "White", colour = "White"),
    legend.position = "none", 
    strip.text = ggtext::element_textbox(size = 10,
    color = "black", fill = "#d8dce6", box.color = "#d8dce6",
    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))+
  labs(title = "Is every guest a gift from God?!",
    caption = "Source: Ministry of Internal Affairs of Georgia<br>Author:Temur Gugushvili",
       subtitle = "*The proportion of Russians' trips to Georgia was constantly growing. Prior to the Covid-19 outbreak, 1 in every 5 visits was from  Russia*",
       x = "",
       y = "")+
  scale_fill_manual(values=c("#808080", "#8a0303"))+
  facet_wrap(vars(Year))

TripsGeorgia


tib_summary_text <- tibble(
  x = 0.9, 
  y = c(1.6), 
  label = c("International Visitor Trips<br>*in 2011*<br><span style = 'color: #808080'>Other Countries - **2,491,403**</span> <br>VS <br><span style = 'color: #8a0303'>Russia - **220,294**</span>"))


# Create text plot with geom_richtext() and theme_void()
text_plot <- tib_summary_text %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3.5,
    hjust = 0,
    label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
    label.margin = unit(c(0, 0, 0, 0), "lines"),
    label.r = unit(0.9, "lines"),
    label.colour = NA,
    family = "Sylfaen"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void(base_family="Sylfaen")
  



ggsave("Visualization/TripsGeorgia2.png", 
       grid.arrange(text_plot, TripsGeorgia,
                    ncol=3, widths = c(0.2,2,0.2)),  
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)




## რუსეთ-უკრაინის დაწყებიდან 25 დღის შემდეგ  35,028 რუსეთის მოქალაქემ შემოვიდა საქართველოში
## რუსეთის მოქალაქეებიისთვის საქართველოს ტერიტორიაზე ჩამოსვლა პოულარული 2018-2019 წელნში 
## პირველი 04 თვის მონაცემების მიხედვით მიუხედავად იმისა, რომ გაიზარდა საქართველოში რუსეთის მოაქილაქეების პროცენტული წილი ადრე 

#24.02.2022 - 20.03.2022 (25 დღე) რუსეთის ფედერაცია 35 028

#write.csv(RussiaTrips,"Data/RussiaTrips.csv", row.names = FALSE)

