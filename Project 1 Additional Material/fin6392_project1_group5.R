# Course: FIN6392.0W1 Financial Technology and Blockchain
# Professor: Zhiqiang Zheng
# Project 1: Big Data Alpha Model
# Group 5:Jessica Chen, Tamo Natarajan, Jane Song, Kort Suter, and Kristine Wilson


# install.packages("Quandl")
library(Quandl)
library(quantmod)

###-------------------------------stock Price------------------------
start.date<-"2010-01-01"; end.date<-"2022-09-01"
### Download Stock Data

# Home Depot(HD)
data.HD<-getSymbols("HD",from=start.date,to=end.date,auto.assign=FALSE)
data.HD <- data.HD$HD.Adjusted
data.HD <-apply.monthly(data.HD,mean)

# Tesla(TSLA)
data.TSLA<-getSymbols("TSLA",from=start.date,to=end.date,auto.assign=FALSE)
data.TSLA <- data.TSLA$TSLA.Adjusted
data.TSLA <-apply.monthly(data.TSLA,mean)

# Costco(COST)
data.COST<-getSymbols("COST",from=start.date,to=end.date,auto.assign=FALSE)
data.COST <- data.COST$COST.Adjusted
data.COST <-apply.monthly(data.COST,mean)

# Royal Caribbean(RCL)
data.RCL<-getSymbols("RCL",from=start.date,to=end.date,auto.assign=FALSE)
data.RCL <- data.RCL$RCL.Adjusted
data.RCL <-apply.monthly(data.RCL,mean)

# Southwest Airlines(LUV)
data.LUV<-getSymbols("LUV",from=start.date,to=end.date,auto.assign=FALSE)
data.LUV <- data.LUV$LUV.Adjusted
data.LUV <-apply.monthly(data.LUV,mean)

# combine stock and calculate return
combined <-cbind(data.HD,data.TSLA,data.COST,data.RCL,data.LUV)
combined$HD.ret<-Delt(combined$HD.Adjusted)
combined$TSLA.ret<-Delt(combined$TSLA.Adjusted)
combined$RCL.ret<-Delt(combined$RCL.Adjusted)
combined$LUV.ret<-Delt(combined$LUV.Adjusted)
combined$COST.ret<-Delt(combined$COST.Adjusted)

write.zoo(combined,file = 'stock_return.csv',index.name="Date",sep=",")


###-----------------macro index-------------------------------------
#Retrieving macroeconomic data from Federal Reserve Database

#get unemployment rate
getSymbols("UNRATE",src="FRED")
unrate <- UNRATE["2010-01-01/2022-01-01"];
unrate <- apply.monthly(unrate,mean)

#get real GDP
getSymbols("GDPC1",src="FRED")
rgdp <-GDPC1["2010-01-01/2022-09-22"]
rgdp <- apply.monthly(rgdp,mean)

#get consumer price index
getSymbols("CPALTT01USM657N",src="FRED")
cpi <-CPALTT01USM657N["2010-01-01/2022-09-22"]
cpi <-apply.monthly(cpi,mean)

#get customer sentiment
getSymbols("UMCSENT",src="FRED")
umcsent <-UMCSENT["2010-01-01/2022-09-22"]
umcsent <- apply.monthly(umcsent,mean)

#get Chicago Fed National Activity Index
getSymbols("CANDH",src="FRED")
candh <- CANDH["2010-01-01/2022-09-22"]
candh <-apply.monthly(candh,mean)

#get crude oil prices
getSymbols("DCOILBRENTEU",src="FRED")
crudeoil <- DCOILBRENTEU["2010-01-01/2022-09-22"]
crudeoil <- apply.monthly(crudeoil,mean)

#get retail sales
getSymbols("RSXFS",src="FRED")
retailsal <- RSXFS["2010-01-01/2022-09-22"]
retailsal <- apply.monthly(retailsal,mean)

#get industrial production
getSymbols("INDPRO",src="FRED")
indpro <- INDPRO["2010-01-01/2022-09-22"]
indpro <- apply.monthly(indpro,mean)

#get bond yields
getSymbols("IRLTLT01USM156N",src="FRED")
bondyd <- IRLTLT01USM156N["2010-01-01/2022-09-22"]
bondyd <- apply.monthly(bondyd,mean)

#get producer price index
getSymbols("PPIACO",src="FRED")
ppiaco <- PPIACO["2010-01-01/2022-09-22"]
ppiaco <- apply.monthly(ppiaco,mean)

df1 <- unrate
df1$gdp <- rgdp
df1$cpi <- cpi
df1$csent <- umcsent
df1$candh <- candh
#df1$crudeoil <- crudeoil_month
df1$retailsal <- retailsal
df1$indpro <- indpro
df1$bondyd <- bondyd
df1$ppiaco <-ppiaco

print(tail(df1,90))


#--------FamaFrench import ----------------------------------

## Fama French 3 factor model
FF.raw <-read.fwf(file="C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/F-F_Research_Data_Factors.txt",width=c(8,8,8,8,8),skip=4)
FF.raw<-FF.raw[-1154:-1253,] #delete the bottom part of yearly summaries need to adjust
names(FF.raw) <-paste(c("text.date","RmxRf","SMB","HML","Rf"))
str(FF.raw)

FF.raw$RmxRf <-as.numeric(as.character(FF.raw$RmxRf))/100
FF.raw$Rf <-as.numeric(as.character(FF.raw$Rf))/100
FF.raw$SMB <-as.numeric(as.character(FF.raw$SMB))/100
FF.raw$HML <-as.numeric(as.character(FF.raw$HML))/100
FF.raw$FF.date <-seq(as.Date("1926-07-01"),as.Date("2022-07-31"),by="months")
FF.raw$FF.date <-as.yearmon(FF.raw$FF.date,"%y-%m-%d")

FF.data <-subset(FF.raw,FF.raw$FF.date>="2010-01-01"& FF.raw$FF.date<="2022-07-31") 

setwd("C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1")
write.csv(FF.data,file = 'FF.csv')

#-------------------------sentiment analysis: reddit---------------------------
library(sentimentr)
library(timetk)

#Analyzing Reddit Post Data Sentiment - TSLA
TSLA_comments <- read.csv("C:/Users/jessi/OneDrive/Documents/FinanceProject1/TSLAnews_comments_all50.csv")
TSLAcomments_sentiment <- sentiment(TSLA_comments$title)
TSLA_comments$created_utc <- as.Date(as.POSIXct(TSLA_comments$created_utc, origin="1970-01-01"))
names(TSLA_comments)[1] <- 'Timestamp'
head(TSLA_comments)
TSLAcomments_sentiment = subset(TSLAcomments_sentiment, select = c(element_id,sentiment))
TSLAcomments_sentiment  <- aggregate(sentiment ~ element_id, data=TSLAcomments_sentiment, FUN=mean)
head(TSLAcomments_sentiment,15)
TSLAcommentsandsent <- cbind(Timestamp=TSLA_comments$Timestamp, TSLA=TSLAcomments_sentiment)
head(TSLAcommentsandsent,15)
TSLAcommentsandsent<-TSLAcommentsandsent[ order(TSLAcommentsandsent$Timestamp , decreasing = FALSE ),]
TSLAcommentsandsent<-tk_xts(TSLAcommentsandsent, select = c("TSLA.sentiment"), date_var = Timestamp)
TSLAcommentsandsent <- apply.monthly(TSLAcommentsandsent,mean)
TSLAcommentsandsent <- data.frame(date=index(TSLAcommentsandsent), coredata(TSLAcommentsandsent))
head(TSLAcommentsandsent)
write.table(TSLAcommentsandsent,file="TSLAredditmonthlynews_sentimentr.csv", na="", sep=",", row.names = FALSE)


#Analyzing Reddit Post Data Sentiment - COST
COST_comments <- read.csv("C:/Users/jessi/OneDrive/Documents/FinanceProject1/COSTnews_comments_all50.csv")
COSTcomments_sentiment <- sentiment(COST_comments$title)
COST_comments$created_utc <- as.Date(as.POSIXct(COST_comments$created_utc, origin="1970-01-01"))
names(COST_comments)[1] <- 'Timestamp'
head(COST_comments)
COSTcomments_sentiment = subset(COSTcomments_sentiment, select = c(element_id,sentiment))
COSTcomments_sentiment  <- aggregate(sentiment ~ element_id, data=COSTcomments_sentiment, FUN=mean)
head(COSTcomments_sentiment,15)
COSTcommentsandsent <- cbind(Timestamp=COST_comments$Timestamp, COST=COSTcomments_sentiment)
head(COSTcommentsandsent,15)
COSTcommentsandsent<-COSTcommentsandsent[order(COSTcommentsandsent$Timestamp , decreasing = FALSE ),]
COSTcommentsandsent<-tk_xts(COSTcommentsandsent, select = c("COST.sentiment"), date_var = Timestamp)
COSTcommentsandsent <- apply.monthly(COSTcommentsandsent,mean)
COSTcommentsandsent <- data.frame(date=index(COSTcommentsandsent), coredata(COSTcommentsandsent))
head(COSTcommentsandsent)
write.table(COSTcommentsandsent,file="COSTredditmonthlynews_sentimentr.csv", na="", sep=",", row.names = FALSE)


#Analyzing Reddit Post Data Sentiment - LUV
LUV_comments <- read.csv("C:/Users/jessi/OneDrive/Documents/FinanceProject1/LUVnews_comments_all50.csv")
LUVcomments_sentiment <- sentiment(LUV_comments$title)
LUV_comments$created_utc <- as.Date(as.POSIXct(LUV_comments$created_utc, origin="1970-01-01"))
names(LUV_comments)[1] <- 'Timestamp'
head(LUV_comments)
LUVcomments_sentiment = subset(LUVcomments_sentiment, select = c(element_id,sentiment))
LUVcomments_sentiment  <- aggregate(sentiment ~ element_id, data=LUVcomments_sentiment, FUN=mean)
head(LUVcomments_sentiment,15)
LUVcommentsandsent <- cbind(Timestamp=LUV_comments$Timestamp, LUV=LUVcomments_sentiment)
head(LUVcommentsandsent,15)
LUVcommentsandsent<-LUVcommentsandsent[ order(LUVcommentsandsent$Timestamp , decreasing = FALSE ),]
LUVcommentsandsent<-tk_xts(LUVcommentsandsent, select = c("LUV.sentiment"), date_var = Timestamp)
LUVcommentsandsent <- apply.monthly(LUVcommentsandsent,mean)
LUVcommentsandsent <- data.frame(date=index(LUVcommentsandsent), coredata(LUVcommentsandsent))
head(LUVcommentsandsent)
write.table(LUVcommentsandsent,file="LUVredditmonthlynews_sentimentr.csv", na="", sep=",", row.names = FALSE)


#Analyzing Reddit Post Data Sentiment - HD
HD_comments <- read.csv("C:/Users/jessi/OneDrive/Documents/FinanceProject1/HDnews_comments_all50.csv")
HDcomments_sentiment <- sentiment(HD_comments$title)
HD_comments$created_utc <- as.Date(as.POSIXct(HD_comments$created_utc, origin="1970-01-01"))
names(HD_comments)[1] <- 'Timestamp'
head(HD_comments)
HDcomments_sentiment = subset(HDcomments_sentiment, select = c(element_id,sentiment))
HDcomments_sentiment  <- aggregate(sentiment ~ element_id, data=HDcomments_sentiment, FUN=mean)
head(HDcomments_sentiment,15)
HDcommentsandsent <- cbind(Timestamp=HD_comments$Timestamp, HD=HDcomments_sentiment)
head(HDcommentsandsent,15)
HDcommentsandsent<-HDcommentsandsent[ order(HDcommentsandsent$Timestamp , decreasing = FALSE ),]
HDcommentsandsent<-tk_xts(HDcommentsandsent, select = c("HD.sentiment"), date_var = Timestamp)
HDcommentsandsent <- apply.monthly(HDcommentsandsent,mean)
HDcommentsandsent <- data.frame(date=index(HDcommentsandsent), coredata(HDcommentsandsent))
head(HDcommentsandsent)
write.table(HDcommentsandsent,file="HDredditmonthlynews_sentimentr.csv", na="", sep=",", row.names = FALSE)


#Analyzing Reddit Post Data Sentiment - RCL
RCL_comments <- read.csv("C:/Users/jessi/OneDrive/Documents/FinanceProject1/RCLnews_comments_all50.csv")
RCLcomments_sentiment <- sentiment(RCL_comments$title)
RCL_comments$created_utc <- as.Date(as.POSIXct(RCL_comments$created_utc, origin="1970-01-01"))
names(RCL_comments)[1] <- 'Timestamp'
head(RCL_comments)
RCLcomments_sentiment = subset(RCLcomments_sentiment, select = c(element_id,sentiment))
RCLcomments_sentiment  <- aggregate(sentiment ~ element_id, data=RCLcomments_sentiment, FUN=mean)
head(RCLcomments_sentiment,15)
RCLcommentsandsent <- cbind(Timestamp=RCL_comments$Timestamp, RCL=RCLcomments_sentiment)
head(RCLcommentsandsent,15)
RCLcommentsandsent<-RCLcommentsandsent[ order(RCLcommentsandsent$Timestamp , decreasing = FALSE ),]
RCLcommentsandsent<-tk_xts(RCLcommentsandsent, select = c("RCL.sentiment"), date_var = Timestamp)
RCLcommentsandsent <- apply.monthly(RCLcommentsandsent,mean)
RCLcommentsandsent <- data.frame(date=index(RCLcommentsandsent), coredata(RCLcommentsandsent))
head(RCLcommentsandsent)
write.table(RCLcommentsandsent,file="RCLredditmonthlynews_sentimentr.csv", na="", sep=",", row.names = FALSE)

#--------------technical_analysis--------------------------------------------------
library(quantmod)
library(timetk)
#Fetching stock data from Yahoo Finance - TESLA
sym.vec <-c("^GSPC","TSLA")
getSymbols(sym.vec, from = "2010-01-01", to = "2022-09-01")
TSLA <- apply.monthly(TSLA,mean)
TSLA <- data.frame(date=index(TSLA), coredata(TSLA))
head(TSLA)

#Fetching stock data from Yahoo Finance - COSTCO
sym.vec <-c("^GSPC","COST")
getSymbols(sym.vec, from = "2010-01-01", to = "2022-09-01")
COST <- apply.monthly(COST,mean)
COST <- data.frame(date=index(COST), coredata(COST))
head(COST)

#Fetching stock data from Yahoo Finance - SOUTHWEST AIRLINES
sym.vec <-c("^GSPC","LUV")
getSymbols(sym.vec, from = "2010-01-01", to = "2022-09-01")
LUV <- apply.monthly(LUV,mean)
LUV <- data.frame(date=index(LUV), coredata(LUV))
head(LUV)

#Fetching stock data from Yahoo Finance - HOME DEPOT
sym.vec <-c("^GSPC","HD")
getSymbols(sym.vec, from = "2010-01-01", to = "2022-09-01")
HD <- apply.monthly(HD,mean)
HD <- data.frame(date=index(HD), coredata(HD))
head(HD)

#Fetching stock data from Yahoo Finance - ROYAL CARIBBEAN
sym.vec <-c("^GSPC","RCL")
getSymbols(sym.vec, from = "2010-01-01", to = "2022-09-01")
RCL <- apply.monthly(RCL,mean)
RCL <- data.frame(date=index(RCL), coredata(RCL))
head(RCL)

#Getting Technical Indicators for Data - TESLA
library("TTR")
head(TSLA); nrow(TSLA); colnames(TSLA)
TSLAsma3=SMA(TSLA[c("TSLA.Close")], n=3) # 3-day moving average
TSLAsma13=SMA(TSLA[c("TSLA.Close")], n=13) # 13-day moving average
tail(TSLAsma3,50)
tail(TSLAsma13,50)
TSLAsma20 <- SMA(TSLA["TSLA.Close"], 20) # 20-day moving average
tail(TSLAsma3)
TSLAema14=EMA(TSLA[c("TSLA.Close")], n=14) #14-day EMA
tail(TSLAema14,n=50)
TSLAbb20 = BBands(TSLA[c("TSLA.Close")], sd=2.0) #bollinger band, default 20 days
tail(TSLAbb20,n=30)

#Getting Technical Indicators for Data - COSTCO
head(COST); nrow(COST); colnames(COST)
COSTsma3=SMA(COST[c("COST.Close")], n=3) # 3-day moving average
COSTsma13=SMA(COST[c("COST.Close")], n=13) # 13-day moving average
tail(COSTsma3,50)
tail(COSTsma13,50)
COSTsma20 <- SMA(COST["COST.Close"], 20) # 20-day moving average
tail(COSTsma20)
COSTema14=EMA(COST[c("COST.Close")], n=14) #14-day EMA
tail(COSTema14,n=50)
COSTbb20 = BBands(COST[c("COST.Close")], sd=2.0) #bollinger band, default 20 days
tail(COSTbb20,n=30)

#Getting Technical Indicators for Data - SOUTHWEST AIRLINES
head(LUV); nrow(LUV); colnames(LUV)
LUVsma3=SMA(LUV[c("LUV.Close")], n=3) # 3-day moving average
LUVsma13=SMA(LUV[c("LUV.Close")], n=13) # 13-day moving average
tail(LUVsma3,50)
tail(LUVsma13,50)
LUVsma20<- SMA(LUV["LUV.Close"], 20) # 20-day moving average
tail(LUVsma20)
LUVema14=EMA(LUV[c("LUV.Close")], n=14) #14-day EMA
tail(LUVema14,n=50)
LUVbb20 = BBands(LUV[c("LUV.Close")], sd=2.0) #bollinger band, default 20 days
tail(LUVbb20,n=30)

#Getting Technical Indicators for Data - HOME DEPOT
head(HD); nrow(HD); colnames(HD)
HDsma3=SMA(HD[c("HD.Close")], n=3) # 3-day moving average
HDsma13=SMA(HD[c("HD.Close")], n=13) # 13-day moving average
tail(HDsma3,50)
tail(HDsma13,50)
HDsma20 <- SMA(HD["HD.Close"], 20) # 20-day moving average
tail(HDsma20)
HDema14=EMA(HD[c("HD.Close")], n=14) #14-day EMA
tail(HDema14,n=50)
HDbb20 = BBands(HD[c("HD.Close")], sd=2.0) #bollinger band, default 20 days
tail(HDbb20,n=30)

#Getting Technical Indicators for Data - ROYAL CARIBBEAN
head(RCL); nrow(RCL); colnames(RCL)
RCLsma3=SMA(RCL[c("RCL.Close")], n=3) # 3-day moving average
RCLsma13=SMA(RCL[c("RCL.Close")], n=13) # 13-day moving average
tail(RCLsma3,50)
tail(RCLsma13,50)
RCLsma20 <- SMA(RCL["RCL.Close"], 20) # 20-day moving average
tail(RCLsma20)
RCLema14=EMA(RCL[c("RCL.Close")], n=14) #14-day EMA
tail(RCLema14,n=50)
RCLbb20 = BBands(RCL[c("RCL.Close")], sd=2.0) #bollinger band, default 20 days
tail(RCLbb20,n=30)

#Data Frames with all stock data plus indicator data
TSLArsi14 = RSI(TSLA[c("TSLA.Close")], n=14)
TSLAmacd = MACD(TSLA[c("TSLA.Close")], nFast=12, nSlow=26, nSig=9,
                maType=SMA)
allDataTSLA = data.frame(TSLA,TSLAsma3,TSLAsma13,TSLAsma20,TSLAema14,TSLAbb20,TSLArsi14,TSLAmacd) # merge all data

COSTrsi14 = RSI(COST[c("COST.Close")], n=14)
COSTmacd = MACD(COST[c("COST.Close")], nFast=12, nSlow=26, nSig=9,
                maType=SMA)
allDataCOST = data.frame(COST,COSTsma3,COSTsma13,COSTsma20,COSTema14,COSTbb20,COSTrsi14,COSTmacd) # merge all data

LUVrsi14 = RSI(LUV[c("LUV.Close")], n=14)
LUVmacd = MACD(LUV[c("LUV.Close")], nFast=12, nSlow=26, nSig=9,
               maType=SMA)
allDataLUV = data.frame(LUV,LUVsma3,LUVsma13,LUVsma20,LUVema14,LUVbb20,LUVrsi14,LUVmacd) # merge all data

HDrsi14 = RSI(HD[c("HD.Close")], n=14)
HDmacd = MACD(HD[c("HD.Close")], nFast=12, nSlow=26, nSig=9,
              maType=SMA)
allDataHD = data.frame(HD,HDsma3,HDsma13,HDsma20,HDema14,HDbb20,HDrsi14,HDmacd) # merge all data

RCLrsi14 = RSI(RCL[c("RCL.Close")], n=14)
RCLmacd = MACD(RCL[c("RCL.Close")], nFast=12, nSlow=26, nSig=9,
               maType=SMA)
allDataRCL = data.frame(RCL,RCLsma3,RCLsma13,RCLsma20,RCLema14,RCLbb20,RCLrsi14,RCLmacd) # merge all data

write.table(allDataTSLA,file="TSLA_with_indicators_monthly.csv", na="", sep=",", row.names = FALSE)
write.table(allDataCOST,file="COST_with_indicators_monthly.csv", na="", sep=",", row.names = FALSE)
write.table(allDataLUV,file="LUV_with_indicators_monthly.csv", na="", sep=",", row.names = FALSE)
write.table(allDataHD,file="HD_with_indicators_monthly.csv", na="", sep=",", row.names = FALSE)
write.table(allDataRCL,file="RCL_with_indicators_monthly.csv", na="", sep=",", row.names = FALSE)

## Graphs for each stock - TESLA
getSymbols("TSLA")
chartSeries(TSLA,subset='2010-01::2022-09',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(TSLA,multi.col=TRUE,theme="white",subset='2010-01::2022-09')
lineChart(TSLA,line.type='h',TA=NULL,subset='2010-01::2022-09')

## Graphs for each stock - COSTCO
getSymbols("COST")
chartSeries(COST,subset='2010-01::2022-09',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(COST,multi.col=TRUE,theme="white",subset='2010-01::2022-09')
lineChart(COST,line.type='h',TA=NULL,subset='2010-01::2022-09')

## Graphs for each stock - SOUTHWEST AIRLINES
getSymbols("LUV")
chartSeries(LUV,subset='2010-01::2022-09',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(LUV,multi.col=TRUE,theme="white",subset='2010-01::2022-09')
lineChart(LUV,line.type='h',TA=NULL,subset='2010-01::2022-09')

## Graphs for each stock - HOME DEPOT
getSymbols("HD")
chartSeries(HD,subset='2010-01::2022-09',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(HD,multi.col=TRUE,theme="white",subset='2010-01::2022-09')
lineChart(HD,line.type='h',TA=NULL,subset='2010-01::2022-09')

## Graphs for each stock - ROYAL CARIBBEAN
getSymbols("RCL")
chartSeries(RCL,subset='2010-01::2022-09',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(RCL,multi.col=TRUE,theme="white",subset='2010-01::2022-09')
lineChart(RCL,line.type='h',TA=NULL,subset='2010-01::2022-09')

#--------------data combine: Python part----------------------------------
# import pandas as pd
# 
# macro = pd.read_csv("C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/macro.csv")
# macro = macro.fillna(method = 'ffill')
# 
# 
# macro.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/macro_2.csv')
# 
# 
# 
# macro_2 = pd.read_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/macro_2.csv')
# tsla = pd.read_excel('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/Fundamental Financial Data.xlsx',
#                      sheet_name='Tesla_Import')
# cost = pd.read_excel('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/Fundamental Financial Data.xlsx',
#                      sheet_name='Costco_Import')
# hd = pd.read_excel('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/Fundamental Financial Data.xlsx',
#                    sheet_name='Homedepot_Import')
# luv = pd.read_excel('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/Fundamental Financial Data.xlsx',
#                     sheet_name='Southwest_Import')
# rcl = pd.read_excel('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/Fundamental Financial Data.xlsx',
#                     sheet_name='RoyalCaribbean_Import')
# 
# 
# macro_2['Date'] = macro_2['Date'].astype('datetime64')
# macro_2['Date'] = macro_2['Date'].dt.strftime('%m/%Y')
# 
# # tsla
# tsla['Date'] = tsla['Date'].dt.strftime('%m/%Y')
# tsla_factor = macro_2.merge(tsla, on='Date', how='left')
# tsla_factor = tsla_factor.fillna(method = 'bfill')
# 
# # cost
# cost['Date'] = cost['Date'].dt.strftime('%m/%Y')
# cost_factor = macro_2.merge(cost, on='Date', how='left')
# cost_factor = cost_factor.fillna(method = 'bfill')
# 
# # hd
# hd['Date'] = hd['Date'].dt.strftime('%m/%Y')
# hd_factor = macro_2.merge(hd, on='Date', how='left')
# hd_factor = hd_factor.fillna(method = 'bfill')
# 
# 
# #luv
# luv['Date'] = luv['Date'].dt.strftime('%m/%Y')
# luv_factor = macro_2.merge(luv, on='Date', how='left')
# luv_factor = luv_factor.fillna(method = 'bfill')
# 
# 
# #rcl
# rcl['Date'] = rcl['Date'].dt.strftime('%m/%Y')
# rcl_factor = macro_2.merge(rcl, on='Date', how='left')
# rcl_factor = rcl_factor.fillna(method = 'bfill')
# 
# tsla_factor.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/tsla.csv')
# cost_factor.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/cost.csv')
# hd_factor.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/hd.csv')
# luv_factor.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/luv.csv')
# rcl_factor.to_csv('C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/rcl.csv')


#---------------plm_regression----------------------------------
alldata <- read.csv(file = 'C:/Users/Jane/Dropbox (Personal)/with Pandas/22Fall/Fin6392/project1/cleaneddata/ALL DATA REGRESSION.csv')
head(alldata)

library(plm)
colnames(alldata)
model <- plm(ret1~macro_gdp+macro_cpi+macro_csent+macro_candh+
               macro_retailsal+macro_indpro+macro_bondyd+macro_ppiaco+
               macro_DCOILBRENTEU+Debt.to.Equity.Ratio+Return.on.Assets+
               TTM.Net.EPS+PE.Ratio+FF.RmxRf+FF.SMB+FF.HML+
               adjusted+sma3+sma13+sma20+ema14+rsi14+macd+signal+
               Social.Media.Sentiment,
             data=alldata,index=c("Stock","Date"),model="within")
summary(model)



#---------------rolling window for alpha and beta values----------------------------------

### rolling window approach to check alpha and beta over time in 2016
library (quantmod)
data.LUV <- getSymbols("LUV", from="2012-09-01",to="2022-09-01",auto.assign = FALSE)
LUV.monthly <-to.monthly(data.LUV)
LUV.monthly <- LUV.monthly[-1,]
LUV.ret <- Delt(LUV.monthly$data.LUV.Adjusted)

data.mkt <- getSymbols("^GSPC", from="2012-09-01",to="2022-09-01",auto.assign = FALSE)
mkt.monthly <-to.monthly(data.mkt); mkt.monthly <-mkt.monthly[,6]
mkt.ret <- Delt(mkt.monthly$data.mkt.Adjusted)
mkt.ret <-mkt.ret[-1,]
market.df <-data.frame(mkt.ret) #convert it into data frame

rets <-  diff(log(data.LUV$LUV.Adjusted))
rets$GSPC <- diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1] <- "LUV"
rets <- rets[-1,]
require(zoo)
coeffs <- rollapply(rets,width=252,FUN=function(x)
{roll.reg=lm(LUV~GSPC, data=as.data.frame(x))
return(roll.reg$coef)
},
by.column=FALSE
)
coeffs <- na.omit(coeffs);coeffs <- coeffs[-1,];names(coeffs) <- c("Alpha","Beta")
plot(coeffs$Alpha)
title(main= "LUV Alpha History")

plot(coeffs$Beta)
title(main= "LUV Beta History")

