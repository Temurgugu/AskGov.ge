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
library(patchwork)
library(ggtext)
library(httr)
library(rvest)

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
  theme(plot.caption = element_markdown(size=10, colour="black", hjust=0),
        plot.title = element_markdown(size=20, colour="#035f8a", hjust=-0.8),
        axis.title.x = element_blank(),
        legend.position = "None",
        ) +
  scale_color_manual(values=c("#FFD400", "#DB0D20", "#00B5E2", "#808080"))+
  scale_x_continuous(breaks=c(1995, 1999, 2003, 2008, 2012, 2016, 2021), limits = c(1995, 2021)) +
  scale_y_continuous(breaks=seq(0, 800000, 200000), limits = c(0, 800000), labels = scales::comma)+
  labs(title = "საგარეო ვაჭრობა: <span style = 'color:#DB0D20'>რუსეთი</span> საქართველოს ექსპორტის ოკუპანტი?!",
       caption = "",
       x = "",
       y = "ათასი აშშ დოლარი")+
  annotate("text", label = "ჩინეთი", x = 2020.3, y = 570000, size = 3.5, colour = "Black", family="Sylfaen", angle = 46)+
  annotate("text", label = "რუსეთი", x = 2016.26, y = 320000, size = 3.5, colour = "Black", family="Sylfaen", angle = 55)+
  annotate("text", label = "აზერბაიჯანი", x = 2011, y = 500000, size = 3.5, colour = "Black", family="Sylfaen", angle = 54)


fig2 <- ggplot(ExportByHS, aes(Year, Percentage, group=CodeRussia, fill=CodeRussia, color = CodeRussia)) +
  geom_area(position="fill", alpha = 0.8) +
  theme_ipsum() +
  theme_minimal(base_family="Sylfaen") +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 90,  hjust=0.5, size=7, colour="black"),
        plot.subtitle = element_text(size=15, colour="#8a0303", hjust=0.5),
        plot.caption = element_markdown(size=9, hjust=1)) +
  scale_x_continuous(breaks=seq(2009, 2021, 12))+
  scale_y_continuous(labels = scales::percent, trans = "reverse") +
  scale_fill_manual(values=c("#808080", "#8a0303"))+
  scale_color_manual(values=c("#808080", "#8a0303"))+
  facet_wrap(vars(HS),  ncol = 6, labeller = label_wrap_gen())+
  labs(caption = "წყარო: საქსტატის საგარეო ვაჭრობის პორტალი|ავტორი: თემურ გუგუშვილი",
       subtitle ="ექსპორტის სამ სექტორში 25%-ზე მეტია ოკუპირებული რუსეთის მიერ",
       x = "",
       y = "")


# Save text data in a tibble
tib_summary_text <- tibble(
  x = 0, 
  y = c(1.7, 1.5, 1.1, 0.9, 0.5, 0.2), 
  label = c("<span style = 'color:grey60'> რუსეთ რუსეთის სამხედრო აგრესიის<br>განხორციელების შემდეგ ქვეყნებს შორის<br>ეკონომიკური ურთიერთდამოკიდებულება<br>პოლიტიკური ინსტრუმენტი კიდევ ერთხელ გახდა</span>",
            "<span style = 'color:grey60'> პოლიტიკური მიზნებისთვის რუსეთი<br>საქართველოდან ექსპორტზე ემბარგოს<br>დაწესების დიდი გამოცდილება აქვს </span>",
            "<span style = 'color:grey60'> რუსეთ-საქართველოს ომის შემდეგ<br>რუსეთში ექსპორტი მზარდ ტენდენციას<br>ინარჩუნებს (ერთეული:$) </span>",
            "<span style = 'color:grey60'> საქართველოდან ექსპორტის ყველაზე<br>დიდი მიმღები ჩინეთის შემდეგ<br>რუსეთია (2021 წელი) </span>",
            "<span style = 'color:grey60'> ექსპორტის ექვსი ყველაზე დიდი<br>მიმართულებიდან სამში რუსეთი<br>25%-ზე მეტს იკავებს (2021 წელი) </span>",
            "<span style = 'color:grey60'> რუსეთში გაზრდილი ექსპორტი საქართველოს<br>დამოკიდებულს ხდის რუსეთის<br>მიმართვს მოწყვლადს </span>"))
            
# Create text plot with geom_richtext() and theme_void()
text_plot <- tib_summary_text %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3.5,
    hjust = 0,
    label.padding = unit(c(0.25, 0.25, 0.25, 0.25), "lines"),
    label.margin = unit(c(0, 0, 0, 0), "lines"),
    label.r = unit(0.3, "lines"),
    label.colour = NA,
    family = "Sylfaen"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void(base_family="Sylfaen")+
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1.98))+
  geom_segment(aes(x = 0, y = 1.7, xend = 0, yend = 1.98, color = "grey60"))+
  geom_segment(aes(x = 0, y = 1.4, xend = 0, yend = 1.6, color = "grey60"))+
  geom_segment(aes(x = 0, y = 1.2, xend = 0, yend = 1.3, color = "grey60"))+
  geom_segment(aes(x = 0, y = 0.3, xend = 0, yend = 1.1, color = "grey60"))+
  theme(legend.position = "None")


ggsave("Visualization/ExportGeorgia2.png", 
       grid.arrange(text_plot, arrangeGrob(fig1, fig2), 
                    ncol=2, widths = 0.5:2),  
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)


