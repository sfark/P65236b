#opdelling til hverdage, weekender og helligdage.

### Vi f?r en vektor paa med tal fra 1-7 hvor mandag=1 soendag=7
dagligpris <- as.data.frame(mutate(select(pris,dato,Oslo),weekday = wday(dato)))

##Vi laver 2 matricer en for hverdags priser og en for weekend priser
hverdagspris <- filter(dagligpris,weekday==2|weekday==3|weekday==4|weekday==5|weekday==6)
weekendpris <- filter(dagligpris,weekday==1|weekday==7)

#gennemsnits prisen for weekend og hverdag
mean(weekendpris[,2])
mean(hverdagspris[,2])

#forskellen p? hverdags pris ?rligt
meanhverdag <- c(
  mean(hverdagspris[1:261,2]),
  mean(hverdagspris[262:522,2]),
  mean(hverdagspris[523:783,2]),
  mean(hverdagspris[784:1044,2]),
  mean(hverdagspris[1045:1304,2]),
  mean(hverdagspris[1305:1565,2])
)
MEANWEEKEND <- c(
  mean(weekendpris[1:104,2]),
  mean(weekendpris[105:208,2]),
  mean(weekendpris[209:312,2]),
  mean(weekendpris[313:417,2]),
  mean(weekendpris[418:522,2]),
  mean(weekendpris[523:626,2])
)
Meandata <- as.data.frame(cbind( c(2013:2018),meanhverdag,MEANWEEKEND))

##?rlig plot gennemsnits pris weekend mod hverdag
ggplot(Meandata,aes(x=Meandata[,1],y=meanhverdag))+
  geom_line(size=1)+
  geom_line(aes(x=Meandata[,1],y=MEANWEEKEND),col="red",size=1)+
  ylab("EL-Spot Price")+xlab("Year")+
  geom_point()+geom_point(aes(x=Meandata[,1],y=MEANWEEKEND),col="red")

##m?nedlig gennemsnits pris weekend mod hverdag
meanmonthhverdag <- c(summarize(group_by(hverdagspris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))
meanmonthweekend <-c(summarize(group_by(weekendpris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))

meanmonthdata <- as.data.frame(cbind(c(1:length(meanmonthhverdag$Oslo)),meanmonthhverdag$Oslo,meanmonthweekend$Oslo))

###m?nedligt plot af gennemsnits pris, hverdag mod weekend
ggplot(meanmonthdata,aes(x=meanmonthdata[,1],y=V2))+
  geom_line(size=0.5)+
  geom_line(aes(x=meanmonthdata[,1],y=V3),col="red",size=0.5)+
  ylab("EL-Spot Price")+xlab("Months")+
  geom_point(aes(x=meanmonthdata[,1],y=V2,color="black"),size=0.9)+
  geom_point(aes(x=meanmonthdata[,1],y=V3,color="red"),size=0.9)+
  theme(legend.position = "right") +
  scale_color_manual(name="Day",labels=c("Weekday","Holyday"),values = c("black", "red"))

### Hellllllllllllllllllllllllllllllllllllllllligdag
helligdage <- c("2013-01-01 12:00:00 GMT","2013-03-28 12:00:00 GMT","2013-03-29 12:00:00 GMT","2013-04-01 12:00:00 GMT","2013-05-01 12:00:00 GMT","2013-05-09 12:00:00 GMT","2013-05-17 12:00:00 GMT","2013-05-20 12:00:00 GMT","2013-12-25 12:00:00 GMT","2013-12-26 12:00:00 GMT","2014-01-01 12:00:00 GMT","2014-04-17 12:00:00 GMT","2014-04-18 12:00:00 GMT","2014-04-21 12:00:00 GMT","2014-05-01 12:00:00 GMT","2014-05-29 12:00:00 GMT","2014-06-09 12:00:00 GMT","2014-12-25 12:00:00 GMT","2014-12-26 12:00:00 GMT","2015-01-01 12:00:00 GMT","2015-04-02 12:00:00 GMT","2015-04-03 12:00:00 GMT","2015-04-06 12:00:00 GMT","2015-05-01 12:00:00 GMT","2015-05-14 12:00:00 GMT","2015-05-25 12:00:00 GMT","2015-12-25 12:00:00 GMT","2015-12-26 12:00:00 GMT","2016-01-01 12:00:00 GMT","2016-03-24 12:00:00 GMT","2016-03-25 12:00:00 GMT","2016-03-28 12:00:00 GMT","2016-05-05 12:00:00 GMT","2016-05-16 12:00:00 GMT","2016-05-17 12:00:00 GMT","2016-12-25 12:00:00 GMT","2016-12-26 12:00:00 GMT","2017-04-13 12:00:00 GMT","2017-04-14 12:00:00 GMT","2017-04-17 12:00:00 GMT","2017-05-01 12:00:00 GMT","2017-05-17 12:00:00 GMT","2017-05-25 12:00:00 GMT","2017-12-25 12:00:00 GMT","2017-12-26 12:00:00 GMT","2018-01-01 12:00:00 GMT","2018-03-29 12:00:00 GMT","2018-03-30 12:00:00 GMT","2018-04-02 12:00:00 GMT","2018-05-01 12:00:00 GMT","2018-05-10 12:00:00 GMT","2018-05-17 12:00:00 GMT","2018-05-21 12:00:00 GMT","2018-12-25 12:00:00 GMT","2018-12-26 12:00:00 GMT")

helligdage = strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")


helligedage2019 <- c("2019-01-01 12:00:00 GMT",
                     "2019-04-18 12:00:00 GMT",
                     "2019-04-19 12:00:00 GMT",
                     "2019-02-22 12:00:00 GMT",
                     "2019-05-01 12:00:00 GMT",
                     "2019-05-17 12:00:00 GMT",
                     "2019-05-30 12:00:00 GMT",
                     "2019-06-10 12:00:00 GMT",
                     "2019-12-25 12:00:00 GMT",
                     "2019-12-26 12:00:00 GMT")

helligedage2019 <-  strptime(helligedage2019, format = "%Y-%m-%d %H:%M:%S", "GMT")
helligdage <-  strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")
dato3 <- strptime(dato, format = "%Y-%m-%d %H:%M:%S", "GMT")
match(helligdage,dato3)
dummyhelligdage <- numeric(length = length(dato3))
dummyhelligdage[match(helligdage,dato3)] <- 1
weekend <- strptime(weekendpris[,1],format = "%Y-%m-%d %H:%M:%S", "GMT")
dummyweekend <- numeric(length = length(dato3))
dummyweekend[match(weekend,dato3)] <- 1
pris <- cbind(dato,PRICES,dummyhelligdage)

