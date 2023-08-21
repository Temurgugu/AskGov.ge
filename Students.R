rm(list=ls())    # clear the workspace

############ Temur Gugushvili ###########
############ Students in school(Georgia) ###########

#Load library 

library(readxl)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(patchwork)
library(ggtext)
library(httr)
library(rvest)


#Import data

Students <- readr::read_csv("Data/students.csv")

#Modify data 

StudentsFilter <- Students |>
  filter(municipality == "ბათუმი"|
         municipality == "ხულო") 
  

StudentsCity <- Students |>
  filter(city == "მაღალმთიანი"|
         city == "დედაქალაქი")


StudentsCity <- StudentsCity |>

  group_by(city, year)  |>
summarise(students = sum(students))
  

#viz

studentsVIZ <- ggplot2::ggplot(StudentsCity, aes(year, students, fill = city))+
  geom_area(alpha=1, position = position_dodge(width = 0.2))+
  theme_minimal(base_family="Sylfaen") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
    axis.text.x=element_text(angle = 0,  hjust=0.5, colour="black"),
    plot.title = element_markdown(size=20, colour="#035f8a", hjust=0),
    plot.subtitle = element_markdown(size=12, colour="black", hjust=0),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="none") +
  scale_fill_manual(values=c("#f0f2f2", "#7b9e82")) +
  labs(title = "მოსწავლეების რაოდენობა <span style = 'color:#7b9e82'>მაღალმთიანი მუნიციპალიტეტები</span> vs <span style = 'color:#e2e6e1'>თბილისი</span>" ,
       subtitle = "მაღალმთიან მუნიციპალიტეტებში მოსწავლეების რაოდენობა ბოლო 2 ათწლეულია პერმამენტულად მცირდება",
       x = "",
       y = "მოსწავლეების რაოდენობა")+
  scale_x_continuous(breaks=seq(2006, 2022, 4)) +
  scale_y_continuous(breaks=seq(0, 230000, 10000), labels = scales::comma, position = "right") 


studentsVIZ


ggsave("Visualization/studentsVIZ.jpg", 
       studentsVIZ,
       width =30, 
       height = 20, 
       units = "cm", 
       dpi = 400)




  
studentsVIZ

st <- plotly::ggplotly(studentsVIZ)

htmlwidgets::saveWidget(st, "index.html")


