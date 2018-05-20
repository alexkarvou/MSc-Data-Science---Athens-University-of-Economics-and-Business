data2000<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/hands-on/2000.csv',header=TRUE,sep = ',')
data2001<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/hands-on/2001.csv',header=TRUE,sep = ',')
data2002<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/hands-on/2002.csv',header=TRUE,sep = ',')
my_data<-rbind(data2000,data2001,data2002)
airports<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/hands-on/airports.csv')
carriers<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/hands-on/carriers.csv')



###################
mydata<-filter(my_data,Year==2000,Origin=='LAX')
aux<-group_by(mydata,Month)
aux2<-summarise(aux,count=n())
plt_data<-aux2
mydata<-filter(my_data,Year==2001,Origin=='LAX')
aux<-group_by(mydata,Month)
aux2<-summarise(aux,count=n())
plt_data<-rbind(plt_data,aux2)
mydata<-filter(my_data,Year==2002,Origin=='LAX')
aux<-group_by(mydata,Month)
aux2<-summarise(aux,count=n())
plt_data<-rbind(plt_data,aux2)
plt_data$Year<-c(rep(2000,12),rep(2001,12),rep(2002,12))
plt_data$Month<-recode(plt_data$Month,"1='Jan';2='Feb';3='Mar';
4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
10='Oct';11='Nov';12='Dec'")
plt_data$Month<-factor(plt_data$Month,levels=c('Jan','Feb','Mar','Apr',
                                               'May','Jun','Jul','Aug','Sep',
                                               'Oct','Nov','Dec'))
#plt_data$Month<-factor('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
plt_data$Year<-as.factor(plt_data$Year)
ggplot(plt_data,aes(x=Month,count,color=Year,group=Year))+
  geom_point(size=5)+
  geom_line()+
  scale_color_ptol()+
  theme_minimal()+
  ggtitle('Number of Inbound Flights per Month')
#####map
map<-get_map('Los Angeles International Airport',zoom=13)
ggmap(map)
####
mydata<-filter(my_data,Year==2000,Origin=='LAX')
unique(mydata$UniqueCarrier)
mydata<-filter(my_data,Year==2001,Origin=='LAX')
unique(mydata$UniqueCarrier)
mydata<-filter(my_data,Year==2002,Origin=='LAX')
unique(mydata$UniqueCarrier)
###violins
mydata<-filter(my_data,Year==2001,Month%in%c(6,7,8,9,10,11,12),Origin=='LAX')
mydata$Month<-recode(mydata$Month,"1='Jan';2='Feb';3='Mar';
4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
10='Oct';11='Nov';12='Dec'")
mydata$Month<-factor(mydata$Month,levels=c('Jan','Feb','Mar','Apr',
                                               'May','Jun','Jul','Aug','Sep',
                                               'Oct','Nov','Dec'))
ggplot(mydata,aes(x=factor(Month),y=TaxiOut,fill='brown4'))+
  geom_violin()+
  theme_minimal()+
  scale_fill_ptol()+
  guides(fill=FALSE)+
  ggtitle('Taxi Out as a proxy to management efficiency')+
  labs(x='')

##########radar
library(ggradar)
mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar
ggradar(mtcars_radar)


#####
my_data %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar
mtcars_radar
#
library(plyr)
library(dplyr)
library(fmsb)
aux1<-filter(data2001,Month%in%c(7,8,9,10,11),Origin=='LAX')
auxil<-group_by(aux1,Month)
plt_data<-summarise(auxil,DepDel=median(DepDelay),ArrDel(ArrDelay),TaxIn=max(TaxiIn)
                    ,TaxOut=max(TaxiOut),N_of_flights=n())
radarchart(plt_data)

