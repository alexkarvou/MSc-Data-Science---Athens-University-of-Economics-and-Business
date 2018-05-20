my_data<-read.csv(file='C:/Users/karvo/Desktop/aueb/Visualization/Final Project/2013-Q2-cabi-trip-history-data.csv',
         header=TRUE)
streets<-read.csv(file='C:/Users/karvo/Desktop/aueb/Visualization/Final Project/streets.csv',
                  header=TRUE)
library(plyr)
library(dplyr)

View(streets)
aux1<-group_by(my_data,Start.Station)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Station','counts')
aux2<-group_by(my_data,End.Station)
counts2<-summarise(aux2,counts=n())
names(counts2)<-c('Station','counts')
plt_data<-merge(counts1,counts2,by=c('Station'),all=TRUE)
plt_data<-mutate(plt_data,count=counts.x+counts.y)
plt_data<-select(plt_data,-c(2,3))
#bar plot
library(ggplot2)
library(ggthemes)
ggplot(plt_data,aes(x=Station,y=count))+
  geom_point()+
  coord_flip()


###map
names(streets)[1]<-'Station'  
plt_data<-merge(plt_data,streets,by='Station')
library(ggmap)
map_lat<-mean(plt_data$latitude)
map_long<-mean(plt_data$longitude)
map<-get_map(c(map_long,map_lat),zoom=11,color='bw')
ggmap(map)+
  geom_point(data=plt_data,aes(x=longitude,y=latitude,color=count,alpha=.7),size=5)+
  scale_color_distiller(palette='Spectral')
################# Time 
library(lubridate)
my_data$Start.time<-as.character(my_data$Start.time)
my_data$month_of_rent<-1
for (i in 1:dim(my_data)[1]){
  aux<-mdy(strsplit(my_data$Start.time[i]," ")[[1]][1])
  my_data$month_of_rent[i]<-month(aux)
  print(i)
}
#aux<-mdy(strsplit(my_data$Start.time," ")[[1]][1]);aux
#####map2-subscriptions
library(gridExtra)
my_data21<-filter(my_data,Subscription.Type=='Casual')
aux1<-group_by(my_data21,Start.Station)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Station','counts')
aux2<-group_by(my_data21,End.Station)
counts2<-summarise(aux2,counts=n())
names(counts2)<-c('Station','counts')
names(streets)[1]<-'Station' 
plt_data<-merge(counts1,counts2,by=c('Station'),all=TRUE)
plt_data<-mutate(plt_data,count=counts.x+counts.y)
plt_data<-select(plt_data,-c(2,3))
plt_data<-merge(plt_data,streets,by='Station')
tot_count<-sum(plt_data$count)
plt_data<-mutate(plt_data,Freq=count/tot_count)
library(ggmap)
map_lat<-mean(plt_data$latitude)
map_long<-mean(plt_data$longitude)
map<-get_map(c(map_long,map_lat),zoom=11,color='bw')
map1<-ggmap(map)+
  geom_point(data=plt_data,aes(x=longitude,y=latitude,color=Freq,alpha=.7),size=5)+
  scale_color_distiller(palette='Spectral')+
  ggtitle('Casual')+
  guides(alpha=FALSE)

my_data21<-filter(my_data,Subscription.Type=='Subscriber')
aux1<-group_by(my_data21,Start.Station)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Station','counts')
aux2<-group_by(my_data21,End.Station)
counts2<-summarise(aux2,counts=n())
names(counts2)<-c('Station','counts')
plt_data<-merge(counts1,counts2,by=c('Station'),all=TRUE)
plt_data<-mutate(plt_data,count=counts.x+counts.y)
plt_data<-select(plt_data,-c(2,3))
plt_data<-merge(plt_data,streets,by='Station')
tot_count<-sum(plt_data$count)
plt_data<-mutate(plt_data,Freq=count/tot_count)
library(ggmap)
map_lat<-mean(plt_data$latitude)
map_long<-mean(plt_data$longitude)
map<-get_map(c(map_long,map_lat),zoom=11,color='bw')
map2<-ggmap(map)+
  geom_point(data=plt_data,aes(x=longitude,y=latitude,color=Freq,alpha=.7),size=5)+
  scale_color_distiller(palette='Spectral')+
  ggtitle('Subscribers')+
  guides(alpha=FALSE)
grid.arrange(map1,map2,ncol=2)
####Subscription
ggplot(my_data,aes(Subscription.Type,fill=Subscription.Type))+
  geom_bar()+
  scale_fill_ptol()+
  theme_minimal()
#####neighborhoods
data2<-read.csv(file='C:/Users/karvo/Desktop/aueb/Visualization/Final Project/locs.csv',
                header=TRUE)
#creating column
data2<-data2[,c(3,5,6)]
names(data2)<-c('Start.Station','latitude','longitude')
data2<-filter(data2,Start.Station%in%my_data$Start.Station)
data2$neighborhood<-'asda'
for (i in 184:dim(data2)[1]){
  aux<-revgeocode(c(data2$longitude[i],data2$latitude[i]),output='more')
  data2$neighborhood[i]<-as.character(aux$neighborhood)
  print(i)
}
revgeocode(c(-77.07811,38.89601),output='more')
data2$neighborhood[110]<-'North Rosslyn'
data2$neighborhood[137]<-'Virginia Square'
data2$neighborhood[138]<-'North Rosslyn'
data2$neighborhood[183]<-'Crystal City'
#rest_data<-filter(my_data,!Start.Station%in%data2$Start.Station)
MYDATA<-merge(my_data,data2,by='Start.Station')
#write.csv(streets, file = "streets2.csv",row.names=FALSE)
#joining
library(plyr)
library(dplyr)
aux<-group_by(my_data,Start.Station)
aux<-summarise(aux,count=n())
names(aux)<-c('Station','count')
plt_data<-merge(aux,streets,by=c('Station'))
plt_data$neighborhood<-as.factor(plt_data$neighborhood)
aux<-group_by(plt_data,by=c('neighborhood'))
aux<-summarise(aux,counts=sum(count))
#aux1<-group_by(my_data,)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Station','counts')
aux2<-group_by(my_data,End.Station)
counts2<-summarise(aux2,counts=n())
names(counts2)<-c('Station','counts')
plt_data<-merge(counts1,counts2,by=c('Station'),all=TRUE)
plt_data<-mutate(plt_data,count=counts.x+counts.y)
plt_data<-select(plt_data,-c(2,3))
write.csv(plt_data, file = "neighbours.csv",row.names=FALSE)
#
aux<-select(streets,c(1,4))
names(aux)<-c('Start.Station','neighborhood')
new_data<-merge(my_data,aux,by=c('Start.Station'))
write.csv(new_data, file = "new_data.csv",row.names=FALSE)
####1
ggplot(new_data,aes(x=reorder(neighborhood,neighborhood, function(x) length(x))))+
  geom_bar(aes(y=..count..),fill='orangered3')+
  coord_flip()+
  theme_minimal()+
  xlab(' ')+
  ylab('#')+
  ggtitle('Bicycle rentals per neighborhood')
###2
ggplot(new_data,aes(neighborhood,fill=Subscription.Type))+
  geom_bar(aes(y=..count..),position='fill')+
  scale_y_continuous(labels=scales::percent)+ 
  coord_flip()+
  theme_minimal()+
  scale_fill_ptol()+
  ylab(" ")+
  ggtitle('Bicycle users per neighborhood')+
  theme(legend.title=element_blank())
####neighbs

#
aux<-streets
aux<-select(aux,c(1,2,3))
names(aux)<-c('Start.Station','longitude','latitude')
plt_data<-merge(new_data,aux,by=c('Start.Station'))  
  
#map<-get_map(c(map_long,map_lat),zoom=12,color='bw')
#ggmap(map)+
#  geom_point(data=plt_data,aes(x=longitude,y=latitude,colour=neighborhood),size=5,alpha=.6)+
#  scale_color_discrete()
plt_data<-filter(streets,Station%in%unique(new_data$Start.Station))
map<-get_map(c(map_long,map_lat),zoom=12,color='bw')
ggmap(map)+
  geom_point(data=plt_data,aes(x=longitude,y=latitude,colour=neighborhood),size=5,alpha=.6)+
  scale_color_manual(values=getPalette(30))+
  ggtitle('Stations with coloured neighborhoods')
#####the real neighb map
set2<-c('Northwest Washington','Northeast 
        Washington','Southeast Washington',
        'Crystal City','Southwest Washington',
        'Downtown','Virginia Square',
        'Radnor-Fort Myer Heights',
        'Lyon Village','Old Town',
        'North Rosslyn','Capitol Hill')
cols<-c('mediumblue','cornflowerblue','skyblue1','palegreen','seagreen','khaki4','khaki1',
        'yellow','coral','darkorange2','red','darkred')
map<-get_map(c(map_long,map_lat),zoom=11,color='bw')
plt_data<-NULL
for (i in unique(MYDATA$Start.Station)){
  plt_data<-rbind(plt_data,filter(MYDATA,Start.Station==i)[1,])
}
ggmap(map)+
  geom_point(data=filter(plt_data,neighborhood%in%set2),aes(x=longitude,y=latitude,colour=neighborhood),size=5,alpha=.6)+
  ggtitle('Stations in the most active neighborhoods')+
  scale_color_ptol()+
  theme(legend.position = 'left')

####
ggmap(map)+
  geom_text(data=plt_data,aes(x=longitude,y=latitude,
                              label=as.numeric(as.factor(neighborhood)),
                              colour=neighborhood),size=5)+
  guides(colour=FALSE)
####
library(lubridate)
new_data$Start.time<-as.character(new_data$Start.time)
new_data$wday<-1
for (i in 1:dim(new_data)[1]){
  aux<-wday(mdy(strsplit(new_data$Start.time[i]," ")[[1]][1]))
  new_data$wday[i]<-aux
  print(i)
}
new_data$wday<-as.character(new_data$wday)
library(car)
new_data$wday<-recode(new_data$wday,"c(1,2,3,4,5)='Weekday';c(6,7)='Weekend'")
new_data$wday<-as.factor(new_data$wday)
##diffs between weekdays and weekends
ggplot(new_data,aes(Subscription.Type))+
  geom_bar(aes(y=..count..,fill=wday),position='dodge')+
  theme_minimal()+
  scale_fill_ptol()+
  xlab(' ')+
  ylab('#')+
  ggtitle('Differences between weekdays and weekends')+
  theme(legend.title=element_blank())
####bar3
p1<-ggplot(MYDATA,aes(x=reorder(neighborhood,neighborhood, function(x) length(x))))+
  geom_bar(aes(y=..count..),fill='orangered3')+
  coord_flip()+
  theme_minimal()+
  xlab(' ')+
  ylab('Number of rentals')+
  ggtitle('Bicycle rentals per neighborhood')

colors <- c('dodgerblue4','darkorange2')
grp_cols<-c('neighborhood','Subscription.Type')
dots <- lapply(grp_cols, as.symbol)
aux<-MYDATA%>%
  group_by_(.dots=dots)%>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
aux<-as.data.frame(aux)
aux$neighborhood<-factor(aux$neighborhood,levels = c('Southwest Washington','Nauck','Alcova Heights',
                                                     'Shirlington','Long Branch Creek','Old Town',
                                                     'Columbia Heights','Aurora Highlands',
                                                     'Old Town North','North Rosslyn','Crystal City',
                                                     'Lyon Park','Radnor - Fort Myer Heights',
                                                     'Northwest Washington','Court House','Capitol Hill',
                                                     'Downtown','Old Town West','Colonial Village',
                                                     'Bluemont','Carver / Langston','North Highland',
                                                     'Virginia Square','Northeast Washington',
                                                     'Lyon Village','Southeast Washington',
                                                     'Ashton Heights'))
ggplot(aux,aes(neighborhood,freq,fill=Subscription.Type))+
  geom_bar(stat='identity',position='fill')+
  geom_text(data=filter(aux,Subscription.Type=='Subscriber'),aes(y=freq-0.02,label=round(freq,2)))+
  coord_flip()+
  theme_minimal()+
  scale_fill_manual(values=colors)+
  ylab('%')+
  xlab(' ')+
  ggtitle('??eighborhoods and User Type')
grid.arrange(p1,p2)
#####
write.csv(new_data, file = "new_dataz.csv",row.names=FALSE)
####Freq map by wday
map_lat<-mean(MYDATA$latitude)
map_long<-mean(MYDATA$longitude)
map<-get_map(c(map_long,map_lat),zoom=12,color='bw')
stick<-new_data[,c(1,3,4,5,6,9)]
stick2<-merge(MYDATA,stick,by=c('Start.time','End.date','Start.Station','End.Station','Bike.'));dim(stick2)
aux_data<-filter(stick2,wday=='Weekday')
aux1<-group_by(aux_data,Start.Station)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Start.Station','counts')
#aux2<-group_by(aux_data,End.Station)
#counts2<-summarise(aux2,counts=n())
#names(counts2)<-c('Station','counts')
#plt_data<-merge(counts1,counts2,by=c('Station'),all=FALSE)
#plt_data<-mutate(plt_data,count=counts.x+counts.y)
#plt_data<-select(plt_data,-c(2,3))
counts1$Start.Station<-as.character(counts1$Start.Station)
counts1$long<-5
counts1$lat<-5
for (i in 1:dim(counts1)){
  counts1$lat[i]<-filter(MYDATA,Start.Station==counts1$Start.Station[i])[1,8]
  counts1$long[i]<-filter(MYDATA,Start.Station==counts1$Start.Station[i])[1,9]
}
p1<-ggmap(map)+
  geom_point(data=counts1,aes(x=long,y=lat,color=counts),size=5,alpha=.7)+
  scale_color_distiller(palette='Spectral')+
  ggtitle('Station Usage in Weekdays')
aux_data<-filter(stick2,wday=='Weekend')
aux1<-group_by(aux_data,Start.Station)
counts1<-summarise(aux1,counts=n())
names(counts1)<-c('Start.Station','counts')
#aux2<-group_by(aux_data,End.Station)
#counts2<-summarise(aux2,counts=n())
#names(counts2)<-c('Station','counts')
#plt_data<-merge(counts1,counts2,by=c('Station'),all=FALSE)
#plt_data<-mutate(plt_data,count=counts.x+counts.y)
#plt_data<-select(plt_data,-c(2,3))
counts1$Start.Station<-as.character(counts1$Start.Station)
counts1$long<-5
counts1$lat<-5
for (i in 1:dim(counts1)){
  counts1$lat[i]<-filter(MYDATA,Start.Station==counts1$Start.Station[i])[1,8]
  counts1$long[i]<-filter(MYDATA,Start.Station==counts1$Start.Station[i])[1,9]
}
p2<-ggmap(map)+
  geom_point(data=counts1,aes(x=long,y=lat,color=counts),size=5,alpha=.7)+
  scale_color_distiller(palette='Spectral')+
  ggtitle('Station usage in Weekends')
grid.arrange(p1,p2,ncol=2)

##METROS
metros<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/Final Project/metros.csv',header = TRUE)
names(metros)[1]<-'X'
map<-get_map(c(map_long,map_lat),zoom=13,color='bw')
ggmap(map)+
  geom_point(data=hotels,aes(x=X,y=Y),color='darkred',size=4,alpha=0.6)+
  geom_point(data=plt_data,aes(x=longitude,y=latitude),
             alpha=.6,fill='cornflowerblue',color='cornflowerblue',size=4,shape=22)+
  ggtitle('Bicycle in blue, Metros in red')
#HOTELS  
hotels<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/Final Project/Hotels.csv',header = TRUE)


  
