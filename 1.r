library("forecast")
library("graphics")
library("TTR")
library("TSA")
library("rio")


data1 <- read.csv("Files/Data_1.csv") 
data2 <- read.csv("Files/Data_2.csv")


#exploration
  #inspect data1
View(data1)
str(data1)
dim(data1)


  #change data into time series
data1.ts <- ts(data1$Yt)
data2.ts <- ts(data2$Sales)


  #get the summary of time series data
summary(data1.ts)
summary(data2.ts)

  #Visualize time series
    #1st data
ts.plot(data1.ts, xlab="Time Period ", ylab="Reading", 
        main = "Time Series Plot")
points(data1.ts)

    #2nd data
ts.plot(data2.ts, xlab="Time Period ", ylab="Sales", 
        main = "Time Series Plot of Sales")
points(data2.ts)


  #train test split
train_data <- data2[1:96,]
test_data <- data2[97:120,]
train_ts <- ts(training_ma$Sales)
test_ts <- ts(testing_ma$Sales)

    #Visualize entire dataset
plot(data2.ts, col="red",main="Plot semua data")
points(data2.ts)

    #Visualize train test
plot(train_ts, col="blue",main="Plot data latih")
points(train_ts)

    #visualize test data
plot(test_ts, col="blue",main="Plot data uji")
points(test_ts)
```
    #with ggplot
library(ggplot2)
ggplot() + 
  geom_line(data = train_data, aes(x = Period, y = Sales, col = "Data Latih")) +
  geom_line(data = test_data, aes(x = Period, y = Sales, col = "Data Uji")) +
  labs(x = "Periode Waktu", y = "Sales", color = "Legend") +
  scale_colour_manual(name="Keterangan:", breaks = c("Data Latih", "Data Uji"),
                      values = c("blue", "red")) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.caption = element_text(hjust=0.5, size=12))

  #Single Moving Average (SMA)
data.sma<-SMA(train_ts, n=4)
data.sma

    #forecasting 1 period to the future
data.ramal<-c(NA, data.sma)
data.ramal
    #forecasting 24 periods to the future with train data
data.gab<-cbind(actual=c(train_ts,rep(NA,24)),
                smoothen=c(data.sma,rep(NA,24)),
                forecast=c(data.ramal,rep(data.ramal[length(data.ramal)],23)),
                actual=c(rep(NA, length(train_ts)), test_ts))

    #Visualize
ts.plot(data2.ts, xlab="Time Period ", ylab="Sales", main= "SMA N=4 Data Sales")
points(data2.ts)
lines(data.gab[,2],col="green",lwd=2)
lines(data.gab[,3],col="red",lwd=2)
legend("topleft",c("actual","smoothen","forecast"), lty=8, col=c("black","green","red", "yellow"), cex=0.5)

    #find accuracy score of the train dataset
error_train.sma = train_ts-data.ramal[1:length(train_ts)]
SSE_train.sma = sum(error_train.sma[5:length(train_ts)]^2)
MSE_train.sma = mean(error_train.sma[5:length(train_ts)]^2)
MAPE_train.sma = mean(abs((error_train.sma[5:length(train_ts)]/train_ts[5:length(train_ts)])*100))

akurasi_train.sma <- matrix(c(SSE_train.sma, MSE_train.sma, MAPE_train.sma))
row.names(akurasi_train.sma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_train.sma) <- c("Akurasi m = 4")
akurasi_train.sma    #MAPE <2%, means it has an incredible accuracy

    #find accuracy score of the train dataset
error_test.sma = test_ts-data.gab[97:120,3]
SSE_test.sma = sum(error_test.sma^2)
MSE_test.sma = mean(error_test.sma^2)
MAPE_test.sma = mean(abs((error_test.sma/test_ts*100)))

akurasi_test.sma <- matrix(c(SSE_test.sma, MSE_test.sma, MAPE_test.sma))
row.names(akurasi_test.sma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_test.sma) <- c("Akurasi m = 4")
akurasi_test.sma     #MAPE <10%, somewhat great accuracy

  #Double Moving Average (DMA)
dma <- SMA(data.sma, n = 4)
At <- 2*data.sma - dma
Bt <- 2/(4-1)*(data.sma - dma)
data.dma<- At+Bt
data.ramal2<- c(NA, data.dma)

t = 1:24
f = c()

for (i in t) {
  f[i] = At[length(At)] + Bt[length(Bt)]*(i)
}

data.gab2 <- cbind(aktual = c(train_ts,rep(NA,24)), 
                   pemulusan1 = c(data.sma,rep(NA,24)), 
                   pemulusan2 = c(data.dma, rep(NA,24)), 
                   At = c(At, rep(NA,24)), Bt = c(Bt,rep(NA,24)), 
                   ramalan = c(data.ramal2, f[-1]))
data.gab2

    #Visualize
ts.plot(data2.ts, xlab="Time Period ", ylab="Sales", main= "DMA N=4 Data Sales")
points(data2.ts)
lines(data.gab2[,3],col="green",lwd=2)
lines(data.gab2[,6],col="red",lwd=2)
legend("topleft",c("data aktual","data pemulusan","data peramalan"), lty=8, col=c("black","green","red"), cex=0.8)

    #accuracy test of train data
error_train.dma = train_ts-data.ramal2[1:length(train_ts)]
SSE_train.dma = sum(error_train.dma[8:length(train_ts)]^2)
MSE_train.dma = mean(error_train.dma[8:length(train_ts)]^2)
MAPE_train.dma = mean(abs((error_train.dma[8:length(train_ts)]/train_ts[8:length(train_ts)])*100))

akurasi_train.dma <- matrix(c(SSE_train.dma, MSE_train.dma, MAPE_train.dma))
row.names(akurasi_train.dma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_train.dma) <- c("Akurasi m = 4")
akurasi_train.dma    #%2 accuracy

Perhitungan akurasi pada data latih menggunakan nilai MAPE menghasilkan nilai MAPE yang kurang dari 2% sehingga dikategorikan sangat baik. Selanjutnya, perhitungan nilai akurasi dilakukan pada data uji.

#Menghitung nilai keakuratan data uji
error_test.dma = test_ts-data.gab2[97:120,6]
SSE_test.dma = sum(error_test.dma^2)
MSE_test.dma = mean(error_test.dma^2)
MAPE_test.dma = mean(abs((error_test.dma/test_ts*100)))

akurasi_test.dma <- matrix(c(SSE_test.dma, MSE_test.dma, MAPE_test.dma))
row.names(akurasi_test.dma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_test.dma) <- c("Akurasi m = 4")
akurasi_test.dma     #10% accuracy

# Single Exponential Smoothing & Double Exponential Smoothing
  # train test split
training<-data1[1:40,]
testing<-data1[41:50,]
train.ts <- ts(training$Yt)
test.ts <- ts(testing$Yt)

  
  #Explore
plot(data1.ts, col="black",main="Plot semua data")
points(data1.ts)

plot(train.ts, col="red",main="Plot data latih")
points(train.ts)

plot(test.ts, col="blue",main="Plot data uji")
points(test.ts)
  #Eksplorasi dengan GGPLOT
ggplot() + 
  geom_line(data = training, aes(x = Periode, y = Yt, col = "Data Latih")) +
  geom_line(data = testing, aes(x = Periode, y = Yt, col = "Data Uji")) +
  labs(x = "Periode Waktu", y = "Membaca", color = "Legend") +
  scale_colour_manual(name="Keterangan:", breaks = c("Data Latih", "Data Uji"),
                      values = c("blue", "red")) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.caption = element_text(hjust=0.5, size=12))

  # SES
    #Cara 1 (fungsi ses)
ses.1 <- ses(train.ts, h = 10, alpha = 0.2)
plot(ses.1)
ses.1

ses.2<- ses(train.ts, h = 10, alpha = 0.7)
plot(ses.2)
ses.2

autoplot(ses.1) +
  autolayer(fitted(ses.1), series="Fitted") +
  ylab("Membaca") + xlab("Periode")

    #Cara 2 (fungsi Holtwinter)
ses1<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE, alpha = 0.2)
plot(ses1)

      #ramalan
ramalan1<- forecast(ses1, h=10)
ramalan1

ses2<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE, alpha = 0.7)
plot(ses2)

      #ramalan
ramalan2<- forecast(ses2, h=10)
ramalan2


#SES
ses.opt <- ses(train.ts, h = 10, alpha = NULL)
plot(ses.opt)
ses.opt

    #Lamda Optimum Holt Winter
sesopt<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE,alpha = NULL)
sesopt
plot(sesopt)

    #ramalan
ramalanopt<- forecast(sesopt, h=10)
ramalanopt


  #accuracy score
SSE1<-ses1$SSE
MSE1<-ses1$SSE/length(train.ts)
RMSE1<-sqrt(MSE1)

akurasi1 <- matrix(c(SSE1,MSE1,RMSE1))
row.names(akurasi1)<- c("SSE", "MSE", "RMSE")
colnames(akurasi1) <- c("Akurasi lamda=0.2")
akurasi1

SSE2<-ses2$SSE
MSE2<-ses2$SSE/length(train.ts)
RMSE2<-sqrt(MSE2)

akurasi2 <- matrix(c(SSE2,MSE2,RMSE2))
row.names(akurasi2)<- c("SSE", "MSE", "RMSE")
colnames(akurasi2) <- c("Akurasi lamda=0.7")
akurasi2


    #accuracy score test data
selisih1<-ramalan1$mean-testing$Yt
SSEtesting1<-sum(selisih1^2)
MSEtesting1<-SSEtesting1/length(testing)

selisih2<-ramalan2$mean-testing$Yt
SSEtesting2<-sum(selisih2^2)
MSEtesting2<-SSEtesting2/length(testing)

selisihopt<-ramalanopt$mean-testing$Yt
SSEtestingopt<-sum(selisihopt^2)
MSEtestingopt<-SSEtestingopt/length(testing)

akurasitesting1 <- matrix(c(SSEtesting1,SSEtesting2,SSEtestingopt))
row.names(akurasitesting1)<- c("SSE1", "SSE2", "SSEopt")
akurasitesting1

akurasitesting2 <- matrix(c(MSEtesting1,MSEtesting2,MSEtestingopt))
row.names(akurasitesting2)<- c("MSE1", "MSE2", "MSEopt")
akurasitesting2

### DES
#Lamda=0.2 dan gamma=0.2
des.1<- HoltWinters(train.ts, gamma = FALSE, beta = 0.2, alpha = 0.2)
plot(des.1)

#ramalan
ramalandes1<- forecast(des.1, h=10)
ramalandes1

#Lamda=0.6 dan gamma=0.3
des.2<- HoltWinters(train.ts, gamma = FALSE, beta = 0.3, alpha = 0.6)
plot(des.2)

#ramalan
ramalandes2<- forecast(des.2, h=10)
ramalandes2
```
#Visually evaluate the prediction
plot(data1.ts)
lines(des.1$fitted[,1], lty=2, col="blue")
lines(ramalandes1$mean, col="red")


#Lamda dan gamma optimum
des.opt<- HoltWinters(train.ts, gamma = FALSE)
des.opt
plot(des.opt)

#ramalan
ramalandesopt<- forecast(des.opt, h=10)
ramalandesopt

#accuracy score
  #train_data
ssedes.train1<-des.1$SSE
msedes.train1<-ssedes.train1/length(train.ts)
sisaandes1<-ramalandes1$residuals
head(sisaandes1)

mapedes.train1 <- sum(abs(sisaandes1[3:length(train.ts)]/train.ts[3:length(train.ts)])
                      *100)/length(train.ts)

akurasides.1 <- matrix(c(ssedes.train1,msedes.train1,mapedes.train1))
row.names(akurasides.1)<- c("SSE", "MSE", "MAPE")
colnames(akurasides.1) <- c("Akurasi lamda=0.2 dan gamma=0.2")
akurasides.1

ssedes.train2<-des.2$SSE
msedes.train2<-ssedes.train2/length(train.ts)
sisaandes2<-ramalandes2$residuals
head(sisaandes2)

mapedes.train2 <- sum(abs(sisaandes2[3:length(train.ts)]/train.ts[3:length(train.ts)])
                      *100)/length(train.ts)

akurasides.2 <- matrix(c(ssedes.train2,msedes.train2,mapedes.train2))
row.names(akurasides.2)<- c("SSE", "MSE", "MAPE")
colnames(akurasides.2) <- c("Akurasi lamda=0.6 dan gamma=0.3")
akurasides.2
  
  #accuracy test data
selisihdes1<-ramalandes1$mean-testing$Yt
selisihdes1

SSEtestingdes1<-sum(selisihdes1^2)
MSEtestingdes1<-SSEtestingdes1/length(testing$Yt)
MAPEtestingdes1<-sum(abs(selisihdes1/testing$Yt)*100)/length(testing$Yt)

selisihdes2<-ramalandes2$mean-testing$Yt
selisihdes2

SSEtestingdes2<-sum(selisihdes2^2)
MSEtestingdes2<-SSEtestingdes2/length(testing$Yt)
MAPEtestingdes2<-sum(abs(selisihdes2/testing$Yt)*100)/length(testing$Yt)

selisihdesopt<-ramalandesopt$mean-testing$Yt
selisihdesopt

SSEtestingdesopt<-sum(selisihdesopt^2)
MSEtestingdesopt<-SSEtestingdesopt/length(testing$Yt)
MAPEtestingdesopt<-sum(abs(selisihdesopt/testing$Yt)*100)/length(testing$Yt)

akurasitestingdes <-
  matrix(c(SSEtestingdes1,MSEtestingdes1,MAPEtestingdes1,SSEtestingdes2,MSEtestingdes2,
           MAPEtestingdes2,SSEtestingdesopt,MSEtestingdesopt,MAPEtestingdesopt),
         nrow=3,ncol=3)
row.names(akurasitestingdes)<- c("SSE", "MSE", "MAPE")
colnames(akurasitestingdes) <- c("des ske1","des ske2","des opt")
akurasitestingdes

#ses vs des comparison
MSEfull <-
  matrix(c(MSEtesting1,MSEtesting2,MSEtestingopt,MSEtestingdes1,MSEtestingdes2,
           MSEtestingdesopt),nrow=3,ncol=2)
row.names(MSEfull)<- c("ske 1", "ske 2", "ske opt")
colnames(MSEfull) <- c("ses","des")
MSEfull


## Smoothing seasonly data
#Import data
library(rio)
data3 <- import("https://raw.githubusercontent.com/rizkynurhambali/Praktikum-MPDW-2324/main/Pertemuan%201/Electric_Production.csv")
data3.ts <- ts(data3$Yt)

#train test split
training<-data3[1:192,2]
testing<-data3[193:241,2]
training.ts<-ts(training, frequency = 13)
testing.ts<-ts(testing, frequency = 13)

#Membuat plot time series
plot(data3.ts, col="red",main="Plot semua data")
points(data3.ts)

plot(training.ts, col="blue",main="Plot data latih")
points(training.ts)

plot(testing.ts, col="green",main="Plot data uji")
points(testing.ts)

#### Pemulusan

#Pemulusan dengan winter aditif 
winter1 <- HoltWinters(training.ts,alpha=0.2,beta=0.1,gamma=0.1,seasonal = "additive")
winter1$fitted
xhat1 <- winter1$fitted[,2]

winter1.opt<- HoltWinters(training.ts, alpha= NULL,  beta = NULL, gamma = NULL, seasonal = "additive")
winter1.opt
winter1.opt$fitted
xhat1.opt <- winter1.opt$fitted[,2]

# Peramalan
forecast1 <- predict(winter1, n.ahead = 49)
forecast1.opt <- predict(winter1.opt, n.ahead = 49)

#Plot time series
plot(training.ts,main="Winter 0.2;0.1;0.1",type="l",col="black",
     xlim=c(1,25),pch=12)
lines(xhat1,type="l",col="red")
lines(xhat1.opt,type="l",col="blue")
lines(forecast1,type="l",col="red")
lines(forecast1.opt,type="l",col="blue")
legend("topleft",c("Actual Data",expression(paste(winter1)),
                   expression(paste(winter1.opt))),cex=0.5,
       col=c("black","red","blue"),lty=1)

#accuracy score

#Akurasi data training
SSE1<-winter1$SSE
MSE1<-winter1$SSE/length(training.ts)
RMSE1<-sqrt(MSE1)
akurasi1 <- matrix(c(SSE1,MSE1,RMSE1))
row.names(akurasi1)<- c("SSE", "MSE", "RMSE")
colnames(akurasi1) <- c("Akurasi")
akurasi1

SSE1.opt<-winter1.opt$SSE
MSE1.opt<-winter1.opt$SSE/length(training.ts)
RMSE1.opt<-sqrt(MSE1.opt)
akurasi1.opt <- matrix(c(SSE1.opt,MSE1.opt,RMSE1.opt))
row.names(akurasi1.opt)<- c("SSE1.opt", "MSE1.opt", "RMSE1.opt")
colnames(akurasi1.opt) <- c("Akurasi")
akurasi1.opt

akurasi1.train = data.frame(Model_Winter = c("Winter 1","Winter1 optimal"),
                            Nilai_SSE=c(SSE1,SSE1.opt),
                            Nilai_MSE=c(MSE1,MSE1.opt),Nilai_RMSE=c(RMSE1,RMSE1.opt))
akurasi1.train

#Akurasi Data Testing
forecast1<-data.frame(forecast1)
testing.ts<-data.frame(testing.ts)
selisih1<-forecast1-testing.ts
SSEtesting1<-sum(selisih1^2)
MSEtesting1<-SSEtesting1/length(testing.ts)

forecast1.opt<-data.frame(forecast1.opt)
selisih1.opt<-forecast1.opt-testing.ts
SSEtesting1.opt<-sum(selisih1.opt^2)
MSEtesting1.opt<-SSEtesting1.opt/length(testing.ts)
```

#Pemulusan dengan winter multiplikatif 
winter2 <- HoltWinters(training.ts,alpha=0.2,beta=0.1,gamma=0.3,seasonal = "multiplicative")
winter2$fitted
xhat2 <- winter2$fitted[,2]

winter2.opt<- HoltWinters(training.ts, alpha= NULL,  beta = NULL, gamma = NULL, seasonal = "multiplicative")
winter2.opt$fitted
xhat2.opt <- winter2.opt$fitted[,2]

#Forecast
forecast2 <- predict(winter2, n.ahead = 49)
forecast2.opt <- predict(winter2.opt, n.ahead = 49)
#### Plot Deret Waktu

#Plot time series
plot(training.ts,main="Winter 0.2;0.1;0.1",type="l",col="black",
     xlim=c(1,25),pch=12)
lines(xhat2,type="l",col="red")
lines(xhat2.opt,type="l",col="blue")
lines(forecast2,type="l",col="red")
lines(forecast2.opt,type="l",col="blue")
legend("topleft",c("Actual Data",expression(paste(winter2)),
                   expression(paste(winter2.opt))),cex=0.5,
       col=c("black","red","blue"),lty=1)
#accuracy
#Akurasi data training
SSE2<-winter2$SSE
MSE2<-winter2$SSE/length(training.ts)
RMSE2<-sqrt(MSE2)
akurasi1 <- matrix(c(SSE2,MSE2,RMSE2))
row.names(akurasi1)<- c("SSE2", "MSE2", "RMSE2")
colnames(akurasi1) <- c("Akurasi lamda=0.2")
akurasi1

SSE2.opt<-winter2.opt$SSE
MSE2.opt<-winter2.opt$SSE/length(training.ts)
RMSE2.opt<-sqrt(MSE2.opt)
akurasi1.opt <- matrix(c(SSE2.opt,MSE2.opt,RMSE2.opt))
row.names(akurasi1.opt)<- c("SSE2.opt", "MSE2.opt", "RMSE2.opt")
colnames(akurasi1.opt) <- c("Akurasi")
akurasi1.opt

akurasi2.train = data.frame(Model_Winter = c("Winter 1","winter2 optimal"),
                            Nilai_SSE=c(SSE2,SSE2.opt),
                            Nilai_MSE=c(MSE2,MSE2.opt),Nilai_RMSE=c(RMSE2,RMSE2.opt))
akurasi2.train
```
#Akurasi Data Testing
forecast2<-data.frame(forecast2)
testing.ts<-data.frame(testing.ts)
selisih2<-forecast2-testing.ts
SSEtesting2<-sum(selisih2^2)
MSEtesting2<-SSEtesting2/length(testing.ts)

forecast2.opt<-data.frame(forecast2.opt)
selisih2.opt<-forecast2.opt-testing.ts
SSEtesting2.opt<-sum(selisih2.opt^2)
MSEtesting2.opt<-SSEtesting2.opt/length(testing.ts)