#Necessary libraries, please install them beforehand if they are not already installed.

library(readxl)
library(ggplot2)
# necessary data manipulations

xdata<-read_xlsx("C:\\Users\\Lenovo\\Desktop\\IE360\\data360.xlsx",sheet="EVDS") #data of usd/try, general cpi index, interest rate and the google trend data of "dolar","enflasyon","faiz" for years 2010-2021 in monthly manner 

# days in the data is a dummy variable, it is added to be able to R can transform this Year,Month columns into date format.
chardates<-paste(xdata$Year,xdata$Month,xdata$Day,sep="-") #turning numeric year,month,day data into char to convert them date format
dates<-as.Date(chardates) #converting the char data to date format

qdata = data.frame(date=dates,year=xdata$Year,usd=xdata$USDTRY,cpi=xdata$CPI,interestRate=xdata$InterestRate,usdTrend=xdata$trendUSD,cpiTrend=xdata$trendCPI,intrateTrend=xdata$trendInterest)
# qdata data frame is used for plotting


#usd/try time graph
ggplot(data=qdata,aes(x=date,y=usd))+ geom_line(color="blue") + geom_smooth(fill = NA, color="red",linetype = "twodash", size = 0.5) + 
  labs(title = "USD/TRY vs Time", x = "Time", y = "Exchange Rate" ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) + 
  scale_y_continuous(breaks = seq(from = 0,to = 15,by = 1), minor_breaks = seq(from = 0,to = 15,by = .2))

#usd/try boxplots
ggplot(data = qdata,aes(x = date, y = usd )) + geom_boxplot(aes(y = usd,group=year, col=year))+labs(title = "Boxplots of USD/TRY vs Time grouped by Years", x = "Time",y = "Exchange Rate") + scale_y_continuous(breaks = seq(from = 0,to = 15,by = 1),minor_breaks = seq(from = 0,to = 15,by = .2)) 
+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month")+ theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))


#dolar trend graph
ggplot(data=qdata,aes(x=date,y=usdTrend)) + geom_line(col="blue") + scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "Google Trend of dolar over time", x = "Time", y = "Trend" )


#cpi time graph
ggplot(data=qdata,aes(x=date,y=cpi)) +geom_line(color="blue")+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "CPI vs Time", x = "Time",y = "CPI Index") + scale_y_continuous(breaks = seq(from = 0,to = 800,by = 100))

#cpi time boxplot
ggplot(data=qdata,aes(x=date,y=cpi)) +geom_boxplot(aes(y = cpi,group=year, col=year))+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "CPI vs Time", x = "Time",y = "CPI Index") + scale_y_continuous(breaks = seq(from = 0,to = 800,by = 100))


#cpi trend time graph
ggplot(data=qdata,aes(x=date,y=cpiTrend)) +geom_line(color="blue")+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "Google Trends of enflasyon", x = "Time",y = "Trend") + scale_y_continuous(breaks = seq(from = 0,to = 100,by = 10))
ggplot(data=qdata,aes(x=date,y=interestRate)) +geom_line(color="blue")+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "Interest Rate vs Time", x = "Time",y = "Interest Rate") + scale_y_continuous(breaks = seq(from = 0,to = 25,by = 5))



#int rate time boxplot
ggplot(data = qdata,aes(x = date, y = interestRate )) + geom_boxplot(aes(y = interestRate,group=year, col=year))+labs(title = "Boxplots of Interest Rate vs Time grouped by Years", x = "Time",y = "Exchange Rate") + scale_y_continuous(breaks = seq(from = 0,to = 25,by = 1)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y %b")+ theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4))


#intrate time graph
ggplot(data=qdata,aes(x=date,y=intrateTrend)) +geom_line(color="blue")+ scale_x_date(date_breaks = "1 year", date_labels = "%Y %b", date_minor_breaks = "1 month") + theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +labs(title = "Google trends of faiz", x = "Time",y = "Trend") + scale_y_continuous(breaks = seq(from = 0,to = 100,by = 10))

cor(qdata$usd,qdata$cpi) #correlation coeff

plot(qdata$usd,qdata$cpi, main = "CPI Index vs. USD Exchange Rate",xlab = "USD/TRY Exchange Rate", ylab = "CPI Index",col="blue",sub="Correlation coefficient=0.9897631") # scatter plot

plot(qdata$usd,qdata$interestRate, main = "Interest Rate vs. USD Exchange Rate",xlab = "USD/TRY Exchange Rate", ylab = "Interest Rate",col="blue") # scatter plot

cor(qdata$usd,qdata$interestRate) #correlation coeff

interest1<-data.frame(interestrate=qdata$interestRate[-c(97:132)],date=qdata$date[-c(97:132)]) # data manipulation

usd1<-data.frame(USDTRY=qdata$usd[-c(97:132)],date=qdata$date[-c(97:132)]) # data manipulation


plot(usd1$USDTRY,interest1$interestrate, main = "Interest Rate vs. USD Exchange Rate (2018-2021) Exclueded",xlab = "USD/TRY Exchange Rate", ylab = "Interest Rate",col="blue")
cor(usd1$USDTRY,interest1$interestrate) # correlation










