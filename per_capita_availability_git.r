#-----------------------------------sugar availability per gdp per countries
# Clear workspace
rm(list = ls(all = TRUE))    

# Packages
library(openxlsx)      
library(rlang)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)
library(maps)
library(countrycode)
library(gghighlight)
library(ggrepel)



#-change working directory
setwd("/Users/alonshepon/My Drive/Main/Research topics/sugar and sustainable diets/Data")

#--read dataset
Worldbank_orig <-read.xlsx #world development indicators from SI spreadsheets
sugar_supply_cap <-read.xlsx#FAO sugar supply per capita from SI spreadsheets
income_groups <-read.xlsx #income groups from SI spreadsheets
#----------------------------------
setwd("/Users/alonshepon/My Drive/Main/Research topics/sugar and sustainable diets/code R")

# Format data
income_groups$group[income_groups$group == 'High income'] <- 'A'
income_groups$group[income_groups$group == 'Upper middle income'] <- 'B'
income_groups$group[income_groups$group == 'Lower middle income'] <- 'C'
income_groups$group[income_groups$group == 'Low income'] <- 'D'
income_groups$group[income_groups$iso == 'IND'] <- 'IND'
income_groups$group[income_groups$iso == 'CHN'] <- 'CHN'
income_groups<-income_groups%>%select(iso,group)

data <- Worldbank_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Gather years
  gather(key="year", value="value", 5:ncol(.)) %>% 
  # Format years
  mutate(year=year %>% gsub("y|x", "", .) %>% as.numeric()) %>% 
  # Reduce to variables of interest
  filter(series_code %in% c("NY.GDP.MKTP.CD","SP.POP.TOTL")) %>% 
  # Simplify variables before spreading
  mutate(series_code=recode(series_code,
                            "NY.GDP.MKTP.CD"="gdp",
                            "SP.POP.TOTL"="population")) %>% 
  select(-series_name) %>% 
  # Spread variables
  spread(key="series_code", value="value") %>% 
  # Calculate GDP per capita
  mutate(gdp_per_cap=gdp / population)

#changes to fit data joining
data$country_name[data$country_name=="United States"]<-"United States of America" 
data$country_name[data$country_name=="Egypt, Arab Rep."]<-"Egypt" 
sugar_supply_cap_concise<-sugar_supply_cap %>% 
  janitor::clean_names("snake") %>%
  select( c("area","element","year","value"))%>%
  #take out only kg per year
  filter(element=="Food supply quantity (kg/capita/yr)") %>%
  select(c("area","year","value"))
sugar_supply_cap_concise$area[sugar_supply_cap_concise$area=="United States"]<-"United States of America" 
sugar_supply_cap_concise$year <- as.numeric(sugar_supply_cap_concise$year)
bind_to_one<-left_join(data,sugar_supply_cap_concise,by = c('country_name' = 'area','year'='year'))

#if interested in groups
bind_to_groups<-left_join(bind_to_one,income_groups,by=c('country_code'='iso'))%>%rename(eco_group=group)%>%filter(!is.na(eco_group))
bind_to_groups_1<-bind_to_groups%>%group_by(eco_group,year)%>%filter_at(vars(gdp_per_cap, value,population), all_vars(!is.na(.)))%>%
  mutate(fg=gdp_per_cap*population,sg=value*population)%>%summarise(gdp_group=sum(fg,na.rm=TRUE)/sum(population,na.rm=TRUE),sugar_group=sum(sg,na.rm=TRUE)/sum(population))


#interesting countries
pp<-c("BRA","USA","JPN","GRC","RUS","IDN","BHR","IND","CHN","ICL","CHE","THA","BGD")
bind_to_one_1<-bind_to_one %>% filter(country_code %in% pp)%>%ungroup()
bind_to_one<-bind_to_one
lab1 <- bind_to_one %>% select(country_code,country_name, gdp_per_cap,value)%>%filter(country_code %in% pp)%>% group_by(country_code) %>% summarize(gdp_per_cap = mean(gdp_per_cap,na.rm=TRUE), value = mean(value,na.rm=TRUE),country_name=unique(country_name))
#change names to short names
lab1$country_name[lab1$country_name=="United States of America"]<-"USA"
lab1$country_name[lab1$country_name=="Russian Federation"]<-"Russia"

#draw countries


f<-ggplot(bind_to_one,aes(y=value,x=(gdp_per_cap),color=country_code))+
geom_point(aes(y=value,x=(gdp_per_cap),color=country_code))+
geom_line(aes(y=value,x=(gdp_per_cap),color=country_code))+
xlim(0,120000)+ylim(0,70)+
  gghighlight(country_code %in% pp,use_direct_label =FALSE,unhighlighted_colour = alpha("gray", 0.2))+annotation_logticks(base = 10,sides = "b")+
  theme_minimal()+theme(legend.position="none")+scale_x_log10(labels = function(x) format(x, scientific = FALSE))+
  geom_label_repel(data=lab1,na.rm = TRUE,aes(color=country_code,label=country_name), max.overlaps=5,force_pull= 1,force=3,point.padding = NA)+
  ylab(expression('sugar supply per capita (kg cap'^-1*' yr'^-1*')'))+xlab(expression('GDP per capita ($US cap'^-1*' yr'^-1*')'))
f
ggsave('sugar_supply.jpg',bg="white",width = 7, height = 4, device='jpg', dpi=700)
#geom_label_repel(data=lab1, aes(x=x, y=y, label=country_code), size=3, nudge_x = .3, nudge_y = .5,color="black",check_overlap = TRUE) +

#draw groups
r<-palette()
m<-ggplot(bind_to_groups_1,aes(x=gdp_group,y=(sugar_group),col=eco_group))+
  theme_light()+theme(text = element_text(size = 15))+
  geom_point(data=bind_to_one,aes(y=value,x=gdp_per_cap),color='gray',alpha=0.2)+
  geom_point(data=bind_to_groups_1,aes(x=gdp_group,y=(sugar_group),col=eco_group))+
  geom_line(aes(x=gdp_group,y=(sugar_group),col=eco_group))+
  xlim(0,50000)+ylim(0,42)+ylab(expression('sugar supply (kg cap'^-1*' yr'^-1*')'))+
 xlab(expression('GDP ($US cap'^-1*' yr'^-1*')'))+labs(color=paste('economic','\n','groups'))+
  scale_x_log10(labels = function(x) format(x, scientific = FALSE))+
  annotate("text", x=53000, y=34, label= "2020",size=3,col=r[2])+
  annotate("text", x=2500, y=32, label= "1961",size=3,col=r[2])+
  annotate("text", x=7000, y=32, label= "2020",size=3,col=r[7])+
  annotate("text", x=300, y=24, label= "1961",size=3,col=r[7])+
  annotate("text", x=1300, y=8, label= "2020",size=3,col=r[4])+
  annotate("text", x=55, y=3, label= "1961",size=3,col=r[4])+
  annotate("text", x=3050, y=20.5, label= "2020",size=3,col=r[6])+
  annotate("text", x=55, y=7, label= "1961",size=3,col=r[6])+
  annotate("text", x=4000, y=18.5, label= "2020",size=3,col=r[3])+
  annotate("text", x=47, y=5, label= "1961",size=3,col=r[3])+
  annotate("text", x=9000, y=9.5, label= "2020",size=3,col=r[5])+
  annotate("text", x=100, y=1, label= "1961",size=3,col=r[5])
m
ggsave('sugar_supply_per_groups_log.jpg',bg="white",width = 7, height = 4, device='jpg', dpi=700)
#geom_label_repel(data=lab1, aes(x=x, y=y, label=country_code), size=3, nudge_x = .3, nudge_y = .5,color="black",check_overlap = TRUE) +



  
  