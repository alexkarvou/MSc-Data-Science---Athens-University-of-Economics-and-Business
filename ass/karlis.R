library(ggplot2)
library(ggmap)
library(ggthemes)
library(plyr)
library(dplyr)
my_data4<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/2nd assignment/2004.csv'
                  ,header = TRUE)
my_data5<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/2nd assignment/2005.csv'
                   ,header = TRUE)
airports<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/2nd assignment/airports.csv')
carriers<-read.csv('C:/Users/karvo/Desktop/aueb/Visualization/2nd assignment/carriers.csv')
###most active companies
ggplot(my_data4,aes(UniqueCarrier))+
  geom_bar(aes(y=(..count..)/sum(..count..)),fill='aquamarine')+
  coord_flip()+
  theme_minimal()+
  scale_fill_ptol()


library(wordcloud2)
library(plyr)
library(dplyr)
aux<-group_by(my_data4,UniqueCarrier)
aux<-summarise(aux,count=n())
aux<-as.data.frame(aux)
names(aux)<-c('word','freq')
aux$word<-as.character(aux$word)
carriers$Code<-as.character(carriers$Code)
carriers$Description<-as.character(carriers$Description)
for (i in 1:dim(aux)[1]){
  if (aux$word[i]%in% unique(carriers$Code)){
    auxil<-carriers$Description[aux$word[i]==carriers$Code]
    aux$word[i]<-select(filter(carriers,Code==aux$word[i]),Description)
      #carriers$Description[aux$word[i]==carriers$Code]
  }

}
aux$word[10]<-'America West Airlines Inc.'
aux$word[17]<-'Us Airways Inc.'
wordcloud2(aux,color='random-dark',size = .4,minSize=.01,shape='star',rotateRatio = 0)
########most busy airports
library(plyr)
library(dplyr)
library(ggmap)
library(ggplot2)
origins<-group_by(my_data4,Origin)
dests<-group_by(my_data4,Dest)
orig_count<-summarise(origins,count=n())
dest_count<-summarise(dests,count=n())
names(dest_count)[1]<-'Origin'
plt_data<-merge(orig_count,dest_count,by=c('Origin'),all=TRUE)
plt_data<-mutate(plt_data,count=count.x+count.y)
plt_data<-select(plt_data,-c(2,3))
names(airports)[1]<-'Origin'
plt_data<-merge(plt_data,airports,by=c('Origin'))
str(plt_data)
map<-get_map('United States',maptype = 'roadmap',zoom=4,color='bw')
ggmap(map)+
  geom_point(data=plt_data,aes(x=long,y=lat,colour=count,alpha=.7),size=8)+
  scale_color_distiller(palette='Spectral')+
  ggtitle('Most busy airports' ,
subtitle = '(33 airports ommited for maximum resolution )')+
  guides(alpha=FALSE)
#########tile
aux<-filter(my_data4,Cancelled==1)
aux<-group_by(aux,Month,CancellationCode)
aux$CancellationCode<-as.character(aux$CancellationCode)
aux<-summarise(aux,count=n())
aux2<-group_by(aux,Month)
aux2<-summarise(aux2,sums=sum(count))
plt_data<-merge(aux,aux2,by='Month',all=TRUE)
library(car)
plt_data$CancellationCode<-as.character(plt_data$CancellationCode)
plt_data$CancellationCode[plt_data$CancellationCode=='A']<-'carrier'
plt_data$CancellationCode[plt_data$CancellationCode=='B']<-'weather'
plt_data$CancellationCode[plt_data$CancellationCode=='C']<-'NAS'
plt_data$CancellationCode[plt_data$CancellationCode=='D']<-'security'
plt_data$Month<-recode(plt_data$Month,"1='January';2='February';3='March';
                       4='April';5='May';6='June';7='July';8='August';9='September';
                       10='October';11='November';12='December'")
plt_data<-mutate(plt_data,rel_freq=count/sums)
plt_data$Month<-factor(plt_data$Month,levels=unique(plt_data$Month))
ggplot(plt_data)+
  geom_tile(aes(x=Month,y=CancellationCode,fill=rel_freq))+
  ggtitle('Reasons for Cancellation percentages-2004')+
  scale_fill_distiller(palette = 'OrRd',direction=1)+
  labs(y='Cancellation Reasons')
  
#########
aux<-filter(my_data4,CancellationCode=='D',Cancelled==1)
aux<-group_by(aux,Month)
aux<-summarise(aux,count=n())
ggplot(plt_data,aes(Month))+
  geom_point(aes(..count..))

#######
#my_data4<-mutate(my_data4)
plt_data<-filter(my_data4,DepDelay>0)
ggplot(plt_data,aes(WeatherDelay,CarrierDelay))+
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
  #geom_raster(aes(fill=..count..),interpolate = TRUE)


######Most meticulous search
auxil<-rbind(my_data4,my_data5)
plt_data<-filter(auxil,DepDelay>0,ArrDelay>0,WeatherDelay==0,NASDelay==0
                 ,LateAircraftDelay==0,CarrierDelay==0,SecurityDelay>0)
names(airports)[1]<-'Origin'
plt_data<-merge(plt_data,airports,by=c('Origin'))
names(plt_data)[34]<-'lat1'
names(plt_data)[35]<-'long1'
names(airports)[1]<-'Dest'
plt_data<-merge(plt_data,airports,by=c('Dest'))
names(plt_data)[40]<-'lat2'
names(plt_data)[41]<-'long2'
plt_data<-plt_data[order(plt_data$DepDelay,decreasing=TRUE),]
map<-get_map('America',zoom=4,color='bw')
ggmap(map)+
  geom_segment(data=plt_data[c(1:4,6:7),],aes(x=long1,y=lat1,xend=long2,yend=lat2),
               colour='#990000',size=1,arrow = arrow(length = unit(10, "points")))+
  geom_segment(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2>35)
               ,aes(x=(long2+long1)/2,y=(lat2+lat1)/2,
                                                xend=(long2+long1)/2,
                                                yend=(lat2+lat1)/2+7))+
  geom_segment(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2<=35)
               ,aes(x=(long2+long1)/2,y=(lat2+lat1)/2,
                    xend=(long2+long1)/2,
                    yend=(lat2+lat1)/2-7))+
  geom_text(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2<=35),
                        aes(x=(long2+long1)/2,
                                            y=(lat2+lat1)/2 -9,
                                            label=paste(Year,'/',Month,
                                                        '/',DayofMonth)))+
  geom_text(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2>35),
                                    aes(x=(long2+long1)/2,
                                        y=(lat2+lat1)/2 +8,
                                        label=paste(Year,'/',Month,
                                                    '/',DayofMonth)))+
  geom_text(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2>35),
            aes(x=(long2+long1)/2,
                y=(lat2+lat1)/2 +9,
                label=paste(round(SecurityDelay/60,1),'h'))
            ,color='olivedrab4',size=8)+
  geom_text(data=filter(plt_data[c(1:4,6:7),],(lat2+lat1)/2<=35),
            aes(x=(long2+long1)/2,
                y=(lat2+lat1)/2 -11,label=paste(round(SecurityDelay/60,1),'h'))
            ,color='olivedrab4',size=8)+
  ggtitle('Most meticulous searches in terms of security delays')
#########################
plt_data<-rbind(my_data4,my_data5)
aux<-group_by(plt_data,Origin)
aux<-summarise(aux,TaxiOuts=sum(TaxiOut))
airp<-as.character(aux$Origin[which.max(aux$TaxiOuts)])
plt_data<-filter(plt_data,Origin==airp)
plt_data$Month<-recode(plt_data$Month,"1='Jan';2='Feb';3='Mar';
                       4='Apr';5='May';6='Jun';7='Jul';8='Aug';9='Sep';
                       10='Oct';11='Nov';12='Dec'")
plt_data$Month<-factor(plt_data$Month,levels=c('Jan','Feb','Mar','Apr',
                                                  'May','Jun','Jul','Aug','Sep',
                                                  'Oct','Nov','Dec'))
ggplot(plt_data,aes(x=Month,y=TaxiOut,fill=factor(Month)))+
  geom_violin()+
  facet_grid(.~Year)+
  theme_minimal()+
  scale_fill_ptol()+
  guides(fill=FALSE)+
  ggtitle('2004 was a badly managed year',subtitle=' for William B Hartsfield-Atlanta Intl, Atlanta, GA')+
  labs(x=' ',y='TaxiOut (in Minutes)')
###############
aux_data<-rbind(my_data4,my_data5)
origins<-group_by(aux_data,Origin)
dests<-group_by(aux_data,Dest)
orig_count<-summarise(origins,count=n())
dest_count<-summarise(dests,count=n())
names(dest_count)[1]<-'Origin'
plt_data<-merge(orig_count,dest_count,by=c('Origin'),all=TRUE)
plt_data<-mutate(plt_data,count=count.x+count.y)
plt_data<-select(plt_data,-c(2,3))
plt_data<-plt_data[order(plt_data$count,decreasing=TRUE),]
aux_data<-plt_data[1:5,]
airps<-as.character(aux_data$Origin)
origs4<-group_by(my_data4,Origin)
dests4<-group_by(my_data4,Dest)
origs4<-data.frame(summarise(origs4,count=n()))
dests4<-summarise(dests4,count=n())
origs5<-group_by(my_data5,Origin)
dests5<-group_by(my_data5,Dest)
origs5<-summarise(origs5,count=n())
dests5<-summarise(dests5,count=n())
names(dests4)[1]<-'Origin'
names(dests5)[1]<-'Origin'
plt_data<-rbind(origs4,origs5,dests4,dests5)
plt_data<-filter(plt_data,Origin%in%airps)
plt_data$Element<-c(rep('Outbound',10),rep('Inbound',10))
plt_data$Year<-c(rep('2004',5),rep('2005',5),rep('2004',5),rep('2005',5))
ggplot(plt_data,aes(x=Origin,y=Element,size=count,color=factor(Year)))+
  geom_jitter(height=0,alpha=.5)+
  scale_color_ptol()+
  theme_minimal()+
  scale_size_continuous(range = c(8,32))+
  guides(size=FALSE)+
  ggtitle('Number of inbound and outbound flights per year',
          subtitle = 'Most busy airports')+
  theme(axis.title.x=element_text(size=13,face='bold'),
        axis.title.y=element_text(size=13,face='bold'))+
  labs(color='Year')
####
#aux_data<-rbind(my_data4,my_data5)
aux1<-group_by(my_data4,UniqueCarrier)
aux1<-summarise(aux1,sumdist=sum(Distance))
aux2<-group_by(my_data5,UniqueCarrier)
aux2<-summarise(aux2,sumdist=sum(Distance))
aux<-rbind(aux1,aux2)
aux$Year<-c(rep('2004',19),rep('2005',20))
ggplot(aux,aes(x=UniqueCarrier,y=sumdist))+
  geom_point(aes(color=Year),size=5,alpha=0.8)+
  theme_minimal()+
  ggtitle('Distance Covered per company per year')+
  scale_color_ptol()+
  labs(y='Distance Covered')

########
aux_data<-rbind(my_data4,my_data5)
aux_data<-select(aux_data,CarrierDelay,WeatherDelay,NASDelay,LateAircraftDelay,Year)
library(reshape)
aux_data<-melt(aux_data,id=c('Year'))
aux_data<-aux_data[aux_data$value>1,]
ggplot(aux_data,aes(value,fill=factor(variable)))+
  geom_density(alpha=.5)+
  scale_fill_ptol(' ')+
  theme_minimal()+
  scale_x_log10()+
  facet_wrap(~Year)+
  ggtitle('Distribution of Delay time per delay type',
          subtitle='When delay is apparent (x-axis logged)')+
  theme(legend.position = 'bottom')


#############kainourgia aeroplana
#aux4<-group_by(my_data4,UniqueCarrier)
#aux4<-summarise(aux4,count=)
aux<-group_by(my_data4,UniqueCarrier)
aux<-summarise(aux,N=n_distinct(TailNum))
aux2<-group_by(my_data5,UniqueCarrier)
aux2<-summarise(aux2,N=n_distinct(TailNum))
aux<-merge(aux,aux2,by='UniqueCarrier',all=TRUE)
aux<-mutate(aux,Diff=N.y-N.x)
aux[20,4]<-49
aux$group<-0
aux$group[aux$Diff<0]<-1
ggplot(aux,aes(UniqueCarrier,Diff,color=factor(group)))+
  geom_point(size=10,alpha=.9)+
  scale_color_ptol()+
  geom_text(aes(UniqueCarrier,Diff-12,label=UniqueCarrier))+
  theme(axis.text.x =element_blank(),axis.ticks.x = element_blank())+
  ggtitle('Difference in fleet size per company between 2004 and 2005')+
  guides(color=FALSE)+
  annotate("segment", x = 17, xend =19, y = 40, yend = 40,
                    arrow=arrow())+
  annotate('text',x=17,y=36,label="Company that began in 2005", 
           family="serif", fontface="italic")

############most active planes- uparxei lathos sta tail numbers
aux<-my_data4 %>%
  group_by(UniqueCarrier, TailNum) %>%
  summarise(count=n())
aux<-as.data.frame(aux)
aux$TailNum<-as.character(aux$TailNum)

names(carriers)[1]<-'UniqueCarrier'
aux<-merge(aux,carriers,by ='UniqueCarrier',all=TRUE)
aux<-arrange(aux,-count)
ggplot(aux[1:200,],aes(x=TailNum,y=count,color=Description))+
  geom_point(size=4,alpha=.6)+
  theme_minimal()+
  scale_color_ptol('Carrier')+
  theme(legend.position = 'bottom',axis.title.x =element_blank(),
        axis.text.x = element_blank())+
  ggtitle('Planes with most flights','and their companies in color')+
  labs(y='Number of flights',x='Tail number')
  #geom_text(aes(x=TailNum,y=count,label=UniqueCarrier))

