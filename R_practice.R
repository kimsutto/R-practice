
################
library(KernSmooth)
head(state.x77)
t77<-data.frame(state.x77)
st77<-t77[,c(2,5)]
st77
plot(st77,
     pch=20,
     main="statex77")
dd<-bkde2D(st77,bandwidth = c(800,2))
par(new=T)
contour(
  dd$x1,dd$x2,dd$fhat,
  col=heat.colors(7)[7:1],
  nlevels=7, lwd=2)
#### 해석 : 수입이 4-5000인 경우 사망률은 6-8이다 

#################
ii<-iris[,c(3,4)]
ii
plot(ii,
     xlim=c(1,10),
     ylim=c(1,5),
     pch=20,
     main="iris")
ddd<-bkde2D(ii,bandwidth = c(2,1))
par(new=T)
contour(
  ddd$x1,ddd$x2,ddd$fhat,
  xlim=c(1,10),
  ylim=c(1,5),
  col=heat.colors(7)[7:1],
  nlevels = 7,lwd=2)
#### 해석 : Petal.Length가 2-6일때, Petal.Width가 1-2이다 

##################

library(lattice)
library(latticeExtra)
library(MASS)
head(state.x77)
t77<-data.frame(state.x77)
st77<-t77[,c(2,5)]
st77

st.Income <-cut(st77$Income,10)
levels(st.Income)<-paste("I",1:10)
st.Murder <- cut(st77$Murder,8)
levels(st.Murder)<-paste("M",1:8)
st77.freq <- table(st.Income,st.Murder)
st77.freq

cloud(st77.freq,
      panel.3d.cloud=panel.3dbars,
      main="state.x77", xlab="Income", ylab="Murder",
      zlab="freq",zlim=c(0,max(st77.freq)*1.3),
      scales=list(arrows=FALSE, just = "right"),
      col.facet=level.colors(st77.freq,
                             at= do.breaks(range(st77.freq),24),
                             col.regions = terrain.colors, colors = TRUE),
      screen=list(z=20,x=-50))

##################

Petal.Length <- cut(iris$Petal.Length,10)
levels(Petal.Length)<-paste("l",1:10)
Petal.Width <- cut(iris$Petal.Width,8)
levels(Petal.Width)<-paste("w",1:8)
petal.freq <- table(Petal.Length,Petal.Width)
petal.freq

cloud(petal.freq,
      panel.3d.cloud=panel.3dbars,
      main="petal.freq", xlab="length", ylab="width",
      zlab="freq",zlim=c(0,max(petal.freq)*1.3),
      scales=list(arrows=FALSE, just = "right"),
      col.facet=level.colors(petal.freq,
                             at= do.breaks(range(petal.freq),24),
                             col.regions = terrain.colors, colors = TRUE),
                             screen=list(z=20,x=-50))


###################

pp<-infert$parity
tbl<-table(pp)
prop<-round((tbl/sum(tbl))*100, digits=0)
sum(prop)
m<-length(prop)
pie(prop,col=2:(m+1),main="infert.parity",
    labels=paste(names(tbl),"EA",prop))
barplot(prop,col=2:(m+1),main="infert.parity",
        ylim=c(0,50))
p.vec <-rep(1:m, prop)
p <-matrix(p.vec,10,10)
color<-2:(m+1)
image(p,col=color,axes=F,main="infert.parity")
abline(h=seq(-0.05,1.05,1.1/10),col="white",lwd=4)
abline(v=seq(-0.05,1.05,1.1/10),col="white",lwd=4)


#####################

pp<-infert$education
tbl<-table(pp)
prop<-round((tbl/sum(tbl))*100, digits=0)
sum(prop)
m<-length(prop)
pie(prop,col=2:(m+1),main="infert.parity",
    labels=paste(names(tbl),"EA",prop))
barplot(prop,col=2:(m+1),main="infert.parity",
        ylim=c(0,100))
p.vec <-rep(1:m, prop)
p <-matrix(p.vec,10,10)
color<-2:(m+1)
image(p,col=color,axes=F,main="infert.parity")
abline(h=seq(-0.05,1.05,1.1/10),col="white",lwd=4)
abline(v=seq(-0.05,1.05,1.1/10),col="white",lwd=4)

####################

rm(list=ls())
HairEyeColor
mosaicplot(HairEyeColor, main = "HairEyeColor", color=c("red","green") , off=20)

##############관찰정보: 브라운헤어는 남녀 모두 eye가 브라운이많다 


##################

rm(list=ls())
santa<-data.frame(belief=c('no belief','no belief','no belief','no belief',
                           'belief','belief','belief','belief',
                           'belief','belief','no belief','no belief',
                           'belief','belief','no belief','no belief'),
                  sibling=c('older brother','older brother','older brother','older sister',
                            'no older sibling','no older sibling','no older sibling','older sister',
                            'older brother', 'older sister', 'older brother', 'older sister',
                            'no older sibling','older sister','older brother','no older sibling'))
santa
mosaicplot(~belief+sibling, data=santa, color=TRUE)

##############관찰정보: 형제가 없는 쪽이 산타를 믿는 경향이 있다. 


######################

#####################

setwd("C:/Users/susu/Desktop/r")
c<-read.csv("table.csv", header=TRUE)

library(treemap)
data(c)
str(c)
treemap(c,
        index=c("cities", "election_name"),
        vSize="election_number",
        vColor="election_number",
        type="value",
        bg.labels="yellow")

#####################
c$election_number.total<-  c$election_no*c$election_number
c.x<-aggregate(c[, 4:6],by=list(c$cities), sum)
c.x
c.x$c.y<-c.x$election_number.total/c.x$election_no
treemap(c.x,
        index=c("Group.1"),
        vSize="election_number",
        vColor="c.y",
        type="value",
        bg.labels="yellow")


#######################
rm(list=ls())
library(MASS)
st<-data.frame(state.x77)
radius<-sqrt(st$Population)
symbols(st$Income, st$Illiteracy,
        circles=radius,
        inches=0.2,
        fg="white",
        bg="green",
        lwd=1.5,
        xlab="Income",
        ylab="Illiteracy",
        main="US State data")
text(st$Income, st$Illiteracy,
     1:nrow(st),
     cex=0.8,
     col="black")
st

#####################관찰 : Income이 4-5000이고 Illiteracy가 0.5-1.5인 도시들이 많다


##########################
air<-airquality
boxplot(Temp~Month, data=air,
        col="orange",
        ylim=c(50,100),
        xlab="Month Number",
        ylab="Temp",
        main="Different boxplots for each month")
#####################관찰: 7월이 일교차가 적고 덥다 

###########################
boxplot(Wind~Month, data=air,
        col="yellow",
        ylim=c(0,20),
        xlab="Month Number",
        ylab="Wind",
        main="Different boxplots for each month")
#####################관찰: 5월이 풍속이 가장 쎄다 

###########################
boxplot(Ozone~Month, data=air,
        col="gray",
        ylim=c(0,170),
        xlab="Month Number",
        ylab="Ozone",
        main="Different boxplots for each month")
######################관찰:  7-8월이 오조농도가 큰편이다 


##########################


install.packages('devtools')
library('devtools')
install_github('dkahle/ggmap',ref="tidyup")
library('ggmap')
library('ggplot2')
register_google(key='') 

names<-c("1.왕십리점","2.천호점","3.자양점","4.용산점","5.성수점","6.수서점","7.양재점",
         "8.신도림점","9.청계천점","10.명일점")
addr<- c("성동구 왕십리광장로17","강동구 천호대로1017", "광진구 아차산로272","용산구 한강대로 23길55",
         "성동구 뚝섬로379","강남구 광평로280","서초구 매헌로16","구로구 새말로97","중구 청계천로400",
         "강동구 고덕로276")
gc<-geocode(enc2utf8(addr))
df<-data.frame(name=names,lon=gc$lon,lat=gc$lat)
cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,maptype = "roadmap",zoom=11,marker=gc)
ggmap(map)
gmap<-ggmap(map)
gmap+geom_text(data=df,
               aes(x=lon,y=lat),
               size=5,
               label=df$name)

#############################
library(treemap)
data(GNI2014)
str(GNI2014)
head(GNI2014)
df<-head(GNI2014,100)
add<-c(df$country)
gc<-geocode(add)
ddf<-data.frame(lon=gc$lon,lat=gc$lat)
gc$lon<-ifelse(gc$lon>180,-(360-gc$lon),gc$lon)
cen<-c(mean(ddf$lon),mean(ddf$lat))
map<-get_googlemap(center=cen,maptype="roadmap",marker=gc)
gmap<-ggmap(map)
gmap+geom_point(data=df,aes(x=lon,y=lat,size=population),
                alpha=0.5)
