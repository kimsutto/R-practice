

###

library(nnet)


# 학습기간 : 2017.1.1~2017.10.13
# 예측기간 : 2017.10.16~2017.10.20

setwd("C:/Users/susu/Desktop/bigData")
dd<- read.csv("dollar_rate.csv")

names(dd)[1:2] = c("ymd","current")
dd$current<-gsub(",","",dd$current)
df<-data.frame(ymd=dd$ymd,current=as.numeric(dd$current))
plot(df$ymd,df$current,xlab="일자",ylab="현재지수",type="o")

df <- df[order(df$ymd),]
df
train_learning <-NULL
train_result <- NULL
for(i in 1:189) {
  train_learning <- rbind(train_learning,
                          df$current[i:(i+9)])
  train_result <- rbind(train_result,
                        df$current[(i+10):(i+14)])
}
test_learning <-NULL
test_result<-NULL
for ( i in 51:100) {
  test_learning <- rbind(test_learning,
                         df$current[i:(i+9)])
  test_result <- rbind(test_result,
                       df$current[(i+10):(i+14)])
}
HIDDEN.NODES = 20
ITERATION = 500
train_result
model <- nnet(train_learning, train_result,
              size=HIDDEN.NODES,
              linout=TRUE, rang = 0.1,decay = 5e-4,
              skip =TRUE, maxit =ITERATION)
predicted2<- predict(model, test_learning,type="raw")
predicted2

err<-abs(test_result-predicted2)
mean(err)
MAPE <- mean(err/test_result)*100 
MAPE




###탑승객 하위 10개 역의 2013년도 월별 승객 추이도

setwd("C:/Users/susu/Desktop/bigData")
subway <- read.csv("subway.csv",header=TRUE,
                   stringsAsFactors = FALSE)
head(subway)
str(subway)


class(subway[,"income_date"])<-"character"
subway[,"income_date"]<-as.Date(subway[,"income_date"],format="%Y%m%d")
subway[,"income_date"]
unique(format(subway[,"income_date"],"%Y"))

subway2 <- subset(subway,subset =format(income_date,"%Y")!="2014")

idx <- grep("\\(",subway2$stat_name)
unique(subway2$stat_name[idx],
       1, nchar(subway2$stat_name[idx])-3)

year<-format(subway2$income_date,"%Y")
month <- format(subway2$income_date,"%m")
subway2<-cbind(subway2,year,month)
head(subway2)

subname<-read.csv('subway_latlong.csv',header = TRUE,
                  stringsAsFactors = FALSE)
tot <- aggregate(subway2[,"on_tot"],
                 by=list(stat_name=subway2$stat_name),
                 FUN=sum)

cc = merge(x=tot,y=subname,by.x="stat_name",by.y="STATION_NM")

df2<- data.frame(stat_name=cc$stat_name,
                 line_num=cc$LINE_NUM,
                 on_tot=cc$x)
df2<- df2[with(df2,order(line_num)),]

df2$stat_name<-factor(df2$stat_name,
                      levels=df2$stat_name)


df3<-df2[order(df2$on_tot),]
df3<-df3[1:10,]
df3$stat_name<-factor(df3$stat_name,levels=df3$stat_name)
ten.station <- df3$stat_name
tmp <- subset(subway2,subset=stat_name %in% ten.station & year == "2013", select=c("stat_name","on_tot","month"))
stat_top10_2013<-aggregate(tmp$on_tot,by=list(month=tmp$month,stat_name=tmp$stat_name),FUN=sum)
stat_top10_2013
names(stat_top10_2013)[3] = "on_tot"
plt <- ggplot(stat_top10_2013,aes(x=month,y=on_tot,colour=stat_name,group=stat_name))
plt<- plt+ theme_classic() + geom_line() +geom_point(size=6,shape=19,alpha=0.5)
plt+scale_x_discrete("2013년", labels=paste0(unique(as.numeric(month)),"월")) +ylab("탑승객수") + scale_colour_discrete(name=c("지하철"))
