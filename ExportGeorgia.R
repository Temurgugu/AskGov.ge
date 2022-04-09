rm(list=ls())    # clear the workspace

############ Temur Gugushvili ###########
############ Export from Georgia AskGov.ge Prject ###########

#Load library 

library(readxl)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(grid)
library(gridExtra)

#Import data

ExportByCoutry <- readr::read_csv("Data/ExportByCoutry.csv")
ExportByHS <- readr::read_csv("Data/ExportByHS.csv")

#Filter data


ExportByHS <- ExportByHS %>%
              filter(Code == "t1") %>%
              group_by(HS, CodeHS, Year, CodeRussia) %>%
              summarise(ThsdUSD1000Code = sum(ThsdUSD1000)) %>%
              mutate(ThsdUSD1000Total = sum(ThsdUSD1000Code)) %>%
              ungroup() %>%
              group_by(HS, CodeHS, Year, CodeRussia) %>%
              summarise(Percentage = ThsdUSD1000Code/ThsdUSD1000Total)


#Figure

fig1 <- ggplot2::ggplot(ExportByCoutry, aes(Year, ThsdUSD1000, color = Code, group = Country))+
                       geom_line(data = ExportByCoutry %>% filter(Code == "4სხვა ქვეყნები"), size = 0.2) +
                       geom_line(data = ExportByCoutry %>% filter(Code == "1ჩინეთი" | Code == "2რუსეთი" | Code == "3აზერბაიჯანი"), size = 0.8) +
                       geom_point(data = ExportByCoutry %>% filter(Year == "2021", Code != "4სხვა ქვეყნები")) +
                       theme_minimal(base_family="Sylfaen") +
                       theme(plot.caption = element_text(size=10, colour="black", hjust=0),
                             plot.subtitle = element_text(size=15, colour="red", hjust=0),
                             plot.title = element_text(size=20, colour="black", hjust=0),
                             axis.title.x = element_blank(),
                             legend.position = "None") +
                       scale_color_manual(values=c("#FFD400", "#DB0D20", "#00B5E2", "#808080"))+
                       scale_x_continuous(breaks=c(1995, 1999, 2003, 2008, 2012, 2016, 2021))+
                       scale_y_continuous(breaks=seq(0, 800000, 200000), limits = c(0, 800000), labels = scales::comma)+
                       labs(title = "საგარეო ვაჭრობა: ექსპორტი საქართველოდან",
                            subtitle ="სამ სექტორში ექსპორტის 25%-ზე მეტი უკუპირებულია",
                            caption = "",
                            x = "",
                            y = "ათასი აშშ დოლარი")+
                       annotate("text", label = "ჩინეთი", x = 2020.3, y = 570000, size = 3.5, colour = "Black", family="Sylfaen", angle = 46)+
                       annotate("text", label = "რუსეთი", x = 2016.26, y = 320000, size = 3.5, colour = "Black", family="Sylfaen", angle = 55)+
                       annotate("text", label = "აზერბაიჯანი", x = 2011, y = 500000, size = 3.5, colour = "Black", family="Sylfaen", angle = 54)+
                       annotate("text", label = "რუსეთ-საქართველოს ომი შემდეგ საქართველოდ რუსეთში ექსპორტის მზარდ ტენდენციას ინარჩუნებს (ერთეული:$)\nსაქართველოდან ექსპორტის ყველაზე დიდი მიმღები ჩინეთის შემდეგ რუსეთია (2021 წელი)\nექსპორტის ექვსი ყველაზე დიდი მიმართულებიდან სამში რუსეთს 25%-ზე მეტს იკავებს (2021 წელი)" ,
                                 x = 1995, y = 700000, size = 3.5, colour = "Black", hjust = 0, family="Sylfaen")


fig2 <- ggplot(ExportByHS, aes(Year, Percentage, group=CodeRussia, fill=CodeRussia, color = CodeRussia)) +
               geom_area(position="fill", alpha = 0.8) +
               theme_ipsum() +
               theme_minimal(base_family="Sylfaen") +
               theme(legend.position="none",
                     axis.title.x = element_blank(),
                     plot.subtitle = element_text(size=10, colour="black", hjust=0.5))+
               scale_x_continuous(breaks=seq(2009, 2021, 12))+
               scale_y_continuous(labels = scales::percent, trans = "reverse") +
               scale_fill_manual(values=c("#808080", "#8a0303"))+
               scale_color_manual(values=c("#808080", "#8a0303"))+
               facet_wrap(vars(HS),  ncol = 6, labeller = label_wrap_gen())+
               labs(caption = "წყარო: საქსტატის საგარეო ვაჭრობის პორტალი \nავტორი: თემურ გუგუშვილი",
                    x = "",
                    y = "")


ggsave("Visualization/ExportGeorgia.png", 
       grid.arrange(fig1, fig2, nrow = 2),  
      
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)


