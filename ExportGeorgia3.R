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

fig1 <- ggplot2::ggplot(ExportByCoutry, aes(Year, ThsdUSD1000, group = Country))+
  geom_line(data = ExportByCoutry %>% filter(Code == "4სხვა ქვეყნები"), aes(color = Code), size = 0.2, show.legend = FALSE) +
  geom_line(data = ExportByCoutry %>% filter(Code == "1ჩინეთი" | Code == "2რუსეთი" | Code == "3აზერბაიჯანი" | Code == "4სომხეთი"), aes(color = Code), size = 0.8) +
  geom_point(data = ExportByCoutry %>% filter(Year == "2022", Code != "4სხვა ქვეყნები"), aes(color = Code), show.legend = FALSE) +
  theme_minimal(base_family="Sylfaen") +
  theme(legend.position = c(.4, .9),
        legend.direction = "horizontal",
    plot.caption = element_markdown(size=10, colour="black", hjust=0),
        plot.title = element_markdown(size=20, colour="#035f8a", hjust=-0.8),
        axis.title.x = element_blank()) +
  scale_color_manual("",
                     values=c("#019114", "#DB0D20", "#00B5E2", "#F2A800", "#808080"),
                     label = c("ჩინეთი", "რუსეთი", "აზერბაიჯანი", "სომხეთი", "სხვა ქვეყნები"))+
  scale_x_continuous(breaks=c(1995, 1999, 2003, 2008, 2012, 2016, 2021, 2022), limits = c(1995, 2022)) +
  scale_y_continuous(breaks=seq(0, 800000, 200000), limits = c(0, 800000), labels = scales::comma)+
  labs(title = "საგარეო ვაჭრობა: <span style = 'color:#DB0D20'>რუსეთი</span> საქართველოს ექსპორტის ოკუპანტი?!",
       subtitle = "",
       caption = "",
       x = "",
       y = "ათასი აშშ დოლარი")

plotly::ggplotly(fig1)
 
fig2 <- ggplot(ExportByHS, aes(Year, Percentage, group=CodeRussia, fill=CodeRussia, color = CodeRussia)) +
  geom_area(position="fill", alpha = 0.8) +
  theme_ipsum() +
  theme_minimal(base_family="Sylfaen") +
  theme(
        legend.position="none",
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
       x = "",
       y = "")


# Save text data in a tibble
tib_summary_text <- tibble(
  x = 0, 
  y = c(1.81, 1.47, 1.18, 1.04, 0.8, 0.56, 0.34, 0.2), 
  label = c("<span style = 'color: #DB0D20'> რუსეთ</span>-უკრაინის ომმა კიდევ ერთხელ<br>შეგვახსენა ჩრდილოეთ ბაზარზე ეკონომიკური<br>დამოკიდებულების საფრთხე</span>",
            "საქართველოს დამოუკიდებლობის მოპოვების<br>შემდეგ, პოლიტიკური მიზნით, <span style = 'color: #DB0D20'>რუსეთში </span><br>არაერთხელ შეიზღუდა ქართული პროდუქციის<br>იმპორტი</span>",
            "<span style = 'font-size:18.0pt'>თუმცა</span>",
            "<span style = 'color: #DB0D20'> რუსეთ</span>-საქართველოს ომის შემდეგ<span style = 'color: #DB0D20'> რუსეთში</span><br>ექსპორტი მზარდ ტენდენციას ინარჩუნებს<br>(ერთეული:$) </span>",
            "საქართველოდან ექსპორტის ყველაზე დიდი<br>მიმღები <span style = 'color:#019114'>ჩინეთის</span> შემდეგ<span style = 'color: #DB0D20'> რუსეთია</span> (2021 წელი) </span>",
            "საქართველოს ექსპორტის ექვსი ყველაზე დიდი<br>მიმართულებიდან სამში <span style = 'color: #DB0D20'> რუსეთი</span> 25%-ზე მეტს<br>იკავებს (2021 წელი) </span>",
            "<span style = 'font-size:18.0pt'> შედეგად </span>",
            "<span style = 'color: #DB0D20'> რუსეთში </span> მზარდი ექსპორტი ამცირებს<br>საქართველოს ეკონომიკური და პოლიტიკური<br>დამოუკიდებლობას</span>"))
            
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
  theme_void(base_family="Sylfaen")+
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1.92))+
  geom_segment(aes(x = 0, y = 1.72, xend = 0, yend = 1.92, color = "grey60"))+
  geom_segment(aes(x = 0, y = 1.35, xend = 0, yend = 1.6, color = "grey60"))+
  geom_segment(aes(x = 0, y = 0.95, xend = 0, yend = 1.13, color = "grey60"))+
  geom_segment(aes(x = 0, y = 0.74, xend = 0, yend = 0.87, color = "grey60"))+
  geom_segment(aes(x = 0, y = 0.48, xend = 0, yend = 0.66, color = "grey60"))+
  geom_segment(aes(x = 0, y = 0.1, xend = 0, yend = 0.29, color = "grey60"))+
  theme(legend.position = "None")


ggsave("Visualization/ExportGeorgia3.png", 
       grid.arrange(text_plot, arrangeGrob(fig1, fig2), 
                    ncol=2, widths = 0.5:2),  
       width =35, 
       height = 20, 
       units = "cm", 
       dpi = 400)


