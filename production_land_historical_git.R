#a program to read and compute global sugar cane, sugar beet and other cereals from 1960s
rm(list = ls(all = TRUE))    #delete variables


library("openxlsx")      #load libraries
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("data.table")
library("stringr")
library("maps")
library("countrycode")

#-read dataset
sugar <-read.xlsx #read FAO historical data from SI spreadsheets
#----------------------------------

cc<-c("darkred","darkgray","cornsilk2","floralwhite","cadetblue")
sugar<-sugar %>% select("Item","Element", "Year","Value")
cereal<- sugar %>% filter(Element=="Production")%>%select("Item","Year","Value") %>%filter(Item %in% c("Rice","Maize","Wheat"))%>%mutate(Value=Value/10^6) #Production in Mt
production<- sugar %>% filter(Element=="Production")%>%select("Item","Year","Value") %>%filter(Item %in% c("Sugar cane","Sugar beet"))%>%mutate(Value=Value/10^6) #Production in Mt
#production$Item <- factor(production$Item, levels=c("Rice","Maize","Wheat","Sugar beet","Sugar cane"))
g<-ggplot(production,aes(x=Year,y=Value, fill = Item, color=Item))+geom_area()+#scale_fill_manual(values=c("#999999", "#E69F00"))+
  geom_line(data=cereal,aes(color=Item,x=Year,y=Value))+scale_fill_manual(values=cc)+scale_color_manual(values=cc)+
  scale_x_continuous(limits=c(1961,2020),breaks=seq(1960, 2010, 10),expand = c(0, 0))+
  scale_y_continuous(limits=c(0,2500),breaks=seq(0, 2500, 500),expand = c(0, 0))+
  geom_text(x=2013, y=1650, label="sugarcane",color="lavenderblush4")+
  geom_text(x=2013, y=2250, label="sugar beet", color="cornsilk4")+
  geom_text(x=2013, y=1200, label="maize", color="darkred")+
  geom_text(x=2013, y=550, label="rice", color="dimgray")+
  geom_text(x=2000, y=500, label="wheat", color="cadetblue")+
  theme_classic()+theme(legend.position = "none",legend.title=element_blank(),text = element_text(size = 15), panel.grid = element_blank(),
                        panel.border = element_blank())+ylab(expression("global harvest (Mt y"^-1*")"))
g
ggsave("production_historical.jpg",width = 20, height = 10, units = "cm")

