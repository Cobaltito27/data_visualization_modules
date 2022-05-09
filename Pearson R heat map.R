# Packages
library(tidyverse)
library(magrittr)
library(ggthemes)
library(RColorBrewer)

# A. Correlation heat map of ONE SET of variables
# ATTENTION: This data frame (Buffer) must have the proper short column names in the right order

varname<-names(Buffer)

Buffer%>%
  cor()%>%
  as.data.frame()%>%
  mutate(x=factor(rownames(.), levels=varname))%>%
  gather(y,R,-x)%>%
  mutate(y=factor(y,levels=varname[length(varname):1]))%>%
  ggplot(aes(x,y))+
  geom_tile(aes(fill=R))+
  scale_x_discrete(position = "top")+
  scale_fill_distiller(palette="Spectral")+
  coord_fixed(ratio=1)+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        text=element_text(color="black",size=13,family="serif", face="bold"))

ggsave("./correlation heat map A.png", width=6, height=5)

# B. Correlation heat map of ONE SET of variables
# ATTENTION: This data frame (Buffer) must have the proper short column names in the right order

varname<-names(Buffer)
# Variable names on x-axis should be as short as possible
varname_x<-names(Buffer)[1:3] 
varname_y<-names(Buffer)[!names(Buffer)%in%varname_x]

Buffer%>%
  cor()%>%
  as.data.frame()%>%
  mutate(x=factor(rownames(.), levels=varname))%>%
  gather(y,R,-x)%>%
  mutate(y=factor(y,levels=varname[length(varname):1]))%>%
  filter(x%in%varname_x,
         y%in%varname_y)%>%
  ggplot(aes(x,y))+
  geom_tile(aes(fill=R))+
  scale_x_discrete(position = "top")+
  scale_fill_distiller(palette="Spectral")+
  coord_fixed(ratio=1)+
  theme_classic()+
  theme(axis.title=element_blank(),
        axis.line=element_line(color="black",size=0.5),
        text=element_text(color="black",size=13,family="serif", face="bold"))

ggsave("./correlation heat map B.png", width=6, height=5)