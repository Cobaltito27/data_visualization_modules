# Packages
library(tidyverse)
library(magrittr)
library(ggthemes)
library(RColorBrewer)

# Color set
MyCol<-c("#DDDDDD",brewer.pal(9,"Set1"))

# 1. Read the CSV file
# The date information column must be named "date" in the original file

Year<-"2015"

dt0<-read.csv("./data/filename.csv",
              header=TRUE,stringsAsFactors=FALSE)%>%
     mutate(DateCol=as.Date(date,format("%d/%m/%Y")))%>%
     group_by(DateCol)%>%
     summarise(no=n())%>%
     right_join(data.frame(
       DateCol=seq(as.Date(paste0(Year,"-01-01"),"%Y-%m-%d"),
                   as.Date(paste0(Year,"-12-31"),"%Y-%m-%d"),
                   "day")
     ),by="DateCol")%>%
     mutate(weekday = lubridate::wday(DateCol, label = T, week_start = 1),
            month = lubridate::month(DateCol, label = T),
            date = lubridate::yday(DateCol),
            week = lubridate::isoweek(DateCol),
            navalue=!is.na(no))%>% 
     group_by(month) %>% 
     mutate(monthweek = 1 + week - min(week))

# 2. Generate the calendar map
# label_text = no, fill_color = navalue

dt0%>%
  ggplot(aes(weekday, -monthweek))+
  geom_tile(colour = "white", aes(fill=navalue))+ 
  geom_text(aes(label = no), color = "white")+
  facet_wrap(~month, nrow = 3, ncol = 4)+
  scale_fill_manual(values=c("#BBBBBB","#EE7777"))+
  theme(aspect.ratio = 1/2,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        legend.position = "none",
        text=element_text(color="black",size=13,
                          family="serif", face="bold"))

ggsave(paste0("./",Year," calendar map.png"), width=10, height=6)