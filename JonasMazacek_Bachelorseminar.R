#in den kommentaren ist jeweils entweder eine erklärung oder gewisse komponenten 
#des codes neutralisiert welche nicht immer oder nur zu überprüfungszwecken verwendet wurden
#die verwendeten packages müssen zuerst installiert werden
#ACHTUNG DER ERSTE TEIL IST DER SCRAPER - EIN LAUFENLASSEN ÜBERSCHREIBT DIE DATEN DIE DATEN IM FILE CLEAN ODER RAWDATA 

#SCRAPER
rm(list = ls())
library("crypto2")
coin_info <- crypto_info(limit=500) #Scrape die essenziellen Informationen zu den ersten 500 Kryptowährungen

list1 <- crypto_list() 
list2 <- 0
list2 <- list1$id==(1 )|list1$id==(2 ) #Generiere eine Tabelle die den Ansprüchen der Funktion crypto_history entspricht
list2 <- list1[which((list1$symbol == "BTC") | (list1$symbol == "XRP") | 
                      (list1$symbol == "LTC") | (list1$symbol == "DOGE" ) | 
                       (list1$symbol == "XLM" ) | (list1$symbol == "XMR" ) |
                       (list1$symbol == "ETH")),] #Wähle diese Kryptowährungen aus

                  
rawdata <- crypto_history(
  coin_list = list2,
  convert = "USD",
  limit = NULL,
  start_date = NULL,
  end_date = NULL,
  interval = NULL,
  sleep = NULL,
  finalWait = TRUE
) #Scrape die von mir gewählten und den Ansprüchen von crypto_history entsprechenden Kryptowhärungen


write.csv(rawdata, "C:\\Users\\Jonas\\Documents\\UNI\\6. Semester\\rawdata.csv") #Generiere zur Sicherheit eine Excel-Datei mit allen Datenpunkten
check <- unique(data$slug) #Check zu den Daten

rawdata <- read.table("rawdata.csv", 
                   sep=",", header=TRUE, stringsAsFactors = TRUE)


data <- subset(rawdata[,c(7,12,16)]) #Wähle nur die Parameter aus die mich Interssieren, Anfangspreis, Name und Zeit

write.csv(data, "C:\\Users\\Jonas\\Documents\\UNI\\6. Semester\\cleandata.csv") #Generiere zur Sicherheit eine Excel-Datei mit allen Datenpunkten

#DATEIVERARBEITUNG
#AB HIER LAUFEN LASSEN!
#ACHTUNG LAUFENLASSEN DES GESAMTEN CODES DAUERT CCA 2 STUNDEN (WEGEN BOOTSTRAPPING BEI AVR-TEST)
#FÜR VERKÜRZUNG IST REDUKTION VON NBOOTS BEI AVR-TEST MÖGLICH


rm(list = ls())
setwd("~/UNI/6. Semester")
library("moments")
library("stats")
library("vrtest")
library("pracma")
library("fNonlinear")
library("forecast")
library("TTR")
library("tseries")
library("SharpeR")
library("randtests")



data <- read.table("cleandata.csv", 
                      sep=",", header=TRUE, stringsAsFactors = TRUE)


#wenn wir in kleinere Zeitreihen unterteilen dann hier
data$time_close <- as.Date(data$time_close)

#data <- subset(data, data$time_close > "2018-12-31")

data <- subset(data[,c(2,3,4)])


cryptlist <- c("BTC","XRP","LTC","DOGE","XLM","XMR","ETH")




k <- 1
runner <- 0
while(k<8){
runner <- cryptlist[c(k)]  
tempdata <- data[data$symbol== runner,]
plot.default(tempdata$time_close, tempdata$open,  main = runner,type = "l"  ,xlab = "Time", ylab = "USD Price")
runner <- 0
k <- k+1}


#calculating returns
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$return <- 0
  tempdata$return <- c(diff(log(tempdata$close)) *  100, NA)
#  tempdata$return <- c(-diff(tempdata$close)/tempdata$close[-1] *  100, NA)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset



#calculating returns (Alternative)
#tempset <- subset(data[c(0),])
#k <- 1
#runner <- 0
#while(k<8){
#  runner <- cryptlist[c(k)]  
#  tempdata <- data[data$symbol== runner,]
#  tempdata$return <- 0
#  tempdata$return <- c(-diff(tempdata$open)/tempdata$open[-1] *  100, NA)
#  tempset <- rbind(tempset, tempdata)
#  runner <- 0
#  k <- k+1}
#data <- tempset



#calculating mean
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$mean <- 0
  tempdata$mean <- mean(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset


#calculating median and other stuff
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$median <- 0
  tempdata$median <- median(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset


#max
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$max <- 0
  tempdata$max <- max(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset

#min
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$min <- 0
  tempdata$min <- min(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset

#skew
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$skewness <- 0
  tempdata$skewness <- skewness(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset

#kurt
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata$kurtosis <- 0
  tempdata$kurtosis <- kurtosis(tempdata$return, na.rm = TRUE)
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset


#jarquebera
tempset <- subset(data[c(0),])
k <- 1
runner <- 0
while(k<8){
  runner <- cryptlist[c(k)]  
  tempdata <- data[data$symbol== runner,]
  tempdata <- na.omit(tempdata)
  tempdata$jarque <- 0
  test <- jarque.bera.test(tempdata$return)
  tempdata$jarque <- test$p.value
  tempset <- rbind(tempset, tempdata)
  runner <- 0
  k <- k+1}
data <- tempset





#creating dataframe with results for ljung-test
ljung <- data.frame(matrix(ncol = 7, nrow = 365))
colnames(ljung) <- cryptlist


#ljung-box
y <- 1
x <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  while (x<365) {
  resultlist <- Box.test(tempdata$return, lag = x, type = c("Ljung-Box"), fitdf = 0)
  ljung[x,y] <- resultlist$p.value
  x <- x+1
  }
  
  x <- 0
  y <- y+1
  }


#ljung <- as.data.frame(ljung < 0.05)

help("plot")


#plotte mir die resultate von ljung-box

windowsFonts(A = windowsFont("Times New Roman"))

plot(head(ljung$BTC,20), ylab="Ljung-Box p-value", xlab="Number of lags", main="Ljung-Box-Test 2018-12-31 - 2022-04-24", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),family = "A")
lines(head(ljung$ETH,20), col="green")
lines(head(ljung$XRP,20), col="red")
lines(head(ljung$LTC,20), col="blue")
lines(head(ljung$DOGE,20), col="yellow")
lines(head(ljung$XLM,20), col="pink")
lines(head(ljung$XMR,20), col="brown")
abline(a=0.05,b=0)






help("lines")


plot(head(ljung$ETH,20), ylab="Ljung-Box Value", xlab="Number of lags", main="ETH")
plot(head(ljung$XRP,20), ylab="Ljung-Box Value", xlab="Number of lags", main="XRP")
plot(head(ljung$LTC,20), ylab="Ljung-Box Value", xlab="Number of lags", main="LTC")
plot(head(ljung$DOGE,20), ylab="Ljung-Box Value", xlab="Number of lags", main="DOGE")
plot(head(ljung$XLM,20), ylab="Ljung-Box Value", xlab="Number of lags", main="XLM")
plot(head(ljung$XMR,20), ylab="Ljung-Box Value", xlab="Number of lags", main="XMR")
#ljung <- subset(ljung, ljung < 0.05, select = "BTC")
help("plot")
#help("subset")

#creating dataframe with results for avr-test
avr <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(avr) <- cryptlist


#avr
y <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  r <- tempdata$return
  r <- na.omit(r)
    resultlist <- AutoBoot.test(r, nboot = 500, wild = "Normal",prob=c(0.025,0.975))
    avr[1,y] <- resultlist$pval

  
  y <- y+1
}






#creating dataframe with results for hurst-exponent
hurst <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(hurst) <- cryptlist


#hurst
y <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  r <- tempdata$return
  r <- na.omit(r)
  resultlist <- hurstexp(r, d = 50)
  hurst[1,y] <- resultlist$Hs
  
  
  y <- y+1
}





#creating dataframe with results for runs
runs <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(runs) <- cryptlist


#runs
y <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  r <- tempdata$return
  r <- na.omit(r)
  resultlist <- runs.test(r)
  runs[1,y]  <- resultlist$p.value
  
  y <- y+1
}




#creating dataframe with results for bdstest
#bds <- data.frame(matrix(ncol = 7, nrow = 8))
#colnames(bds) <- cryptlist


#bds
#y <- 1
#while(y<8){
#  runner <- cryptlist[c(y)]  
#  tempdata <- data[data$symbol== runner,]
  
#  r <- tempdata$return
#  r <- na.omit(r)
#  resultlist <- bdsTest(r, m = 3)
#  bds[,y] <- resultlist@test
  
  
#  y <- y+1
#}


#resultlist <- bdsTest(r, m = 3)
#g <- resultlist@test



#Running results 

#creating dataframe with results for ljung-test
rljung <- data.frame(matrix(ncol = 7, nrow = length(seq(from=as.Date("2013-04-29"), to=as.Date("2022-04-24"), by="day"))))
colnames(rljung) <- cryptlist
rljung$date <- seq(as.Date("2013-04-29"),as.Date("2022-04-24"),by = 1)



#ljung-box
nacounter <- 0
y <- 1
x <- 0
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  numrow <- nrow(tempdata) - 350
  while (x<numrow) {
    subtempdata <- subset(tempdata[c(x:350+x),])
    resultlist <- Box.test(subtempdata$return, lag = 2, type = c("Ljung-Box"), fitdf = 0)
    
    if(is.na(resultlist$p.value)){
      nacounter <- nacounter+1
      }
    
    rljung[rljung$date== (min(tempdata$time_close))+x+350,y] <- resultlist$p.value
    x <- x+1
  }
  
  x <- 0
  y <- y+1
}


plot(rljung$date, rljung$BTC, xlim = c(16250,19000), ylab="Ljung-Box Value", xlab="Date", main="Ljung-Box Test", type="l", 
sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1))
lines(rljung$date,rljung$ETH, col="green")
abline(a=0.05,b=0)

plot(rljung$date, rljung$XRP, xlim = c(16250,19000), ylab="Ljung-Box Value", xlab="Date", main="Ljung-Box Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="red")
lines(rljung$date,rljung$LTC, col="blue")
lines(rljung$date,rljung$XLM, col="pink")
abline(a=0.05,b=0)

plot(rljung$date, rljung$DOGE, xlim = c(16250,19000), ylab="Ljung-Box Value", xlab="Date", main="Ljung-Box Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="yellow")

lines(rljung$date,rljung$XMR, col="brown")
abline(a=0.05,b=0)



#creating a percentage efficiency indicator
ljungp <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(ljungp) <- cryptlist


y <- 1
while(y<8){
  tempdata <- as.data.frame(rljung[,y])
  
  tempdata <- na.omit(tempdata)
  
  
  colnames(tempdata) <- c("r")
  percentage <- length(which(tempdata$r< 0.05))/nrow(tempdata)
  
  
  ljungp[1,y] <- percentage
  
  
  y <- y+1
}










#creating dataframe with results for avr-test
ravr <- data.frame(matrix(ncol = 7, nrow = length(seq(from=as.Date("2013-04-29"), to=as.Date("2022-04-24"), by="day"))))
colnames(ravr) <- cryptlist
ravr$date <- seq(as.Date("2013-04-29"),as.Date("2022-04-24"),by = 1)





#avr
nacounter <- 0
y <- 1
x <- 0
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  numrow <- nrow(tempdata) - 350
  while (x<numrow) {
    subtempdata <- subset(tempdata[c(x:(350+x)),])
    r <- subtempdata$return
    r <- na.omit(r)
    resultlist <- AutoBoot.test(r, nboot = 75, wild = "Normal",prob=c(0.025,0.975))
    
   # if(is.na(resultlist$pval)){
   #   nacounter <- nacounter+1
   # }
    
    ravr[ravr$date== (min(tempdata$time_close))+x+350,y] <- resultlist$pval
    x <- x+1
  }
  
  x <- 0
  y <- y+1
}


plot.default(ravr$date, ravr$BTC, type = "l", xlim = c(16200,18000))

#creating a percentage efficiency indicator
avrp <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(avrp) <- cryptlist


y <- 1
while(y<8){
  tempdata <- as.data.frame(ravr[,y])
  
  tempdata <- na.omit(tempdata)
  colnames(tempdata) <- c("r")
  percentage <- length(which(tempdata$r> 0.05))/nrow(tempdata)
  
  avrp[1,y] <- percentage
  
  
  y <- y+1
}


plot(ravr$date, ravr$BTC, xlim = c(16250,19000), ylab="AVR Value", xlab="Date", main="AVR-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1))
lines(ravr$date,ravr$ETH, col="green")
lines(ravr$date,ravr$DOGE, col="yellow")
lines(ravr$date,ravr$XMR, col="brown")
lines(ravr$date,ravr$LTC, col="blue")
lines(ravr$date,ravr$XLM, col="pink")
lines(ravr$date,ravr$XRP, col="red")
abline(a=0.05,b=0)



plot(ravr$date, ravr$XRP, xlim = c(16250,19000), ylab="AVR Value", xlab="Date", main="AVR-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="red")
lines(ravr$date,ravr$LTC, col="blue")
lines(ravr$date,ravr$XLM, col="pink")
abline(a=0.05,b=0)

plot(ravr$date, ravr$DOGE, xlim = c(16250,19000), ylab="AVR Value", xlab="Date", main="AVR-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="yellow")
lines(ravr$date,ravr$XMR, col="brown")
abline(a=0.05,b=0)












#creating dataframe with results for hurst
rhurst <- data.frame(matrix(ncol = 7, nrow = length(seq(from=as.Date("2013-04-29"), to=as.Date("2022-04-24"), by="day"))))
colnames(rhurst) <- cryptlist
rhurst$date <- seq(as.Date("2013-04-29"),as.Date("2022-04-24"),by = 1)



#rhurst
nacounter <- 0
y <- 1
x <- 0
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  numrow <- nrow(tempdata) - 350
  while (x<numrow) {
    subtempdata <- subset(tempdata[c(x:(350+x)),])
    r <- subtempdata$return
    r <- na.omit(r)
    resultlist <- hurstexp(r, d = 50)
    
     if(is.na(resultlist$Hs)){
       nacounter <- nacounter+1
     }
    
    
    rhurst[rhurst$date== (min(tempdata$time_close))+x+350,y] <- resultlist$Hs
    x <- x+1
  }
  
  x <- 0
  y <- y+1
}


plot(rhurst$date, rhurst$BTC, xlim = c(16250,19000), ylab="Hurst Exponent Value", xlab="Date", main="Hurst Exponent", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0.4,0.7))
lines(rhurst$date,rhurst$ETH, col="green")
lines(rhurst$date,rhurst$DOGE, col="yellow")
lines(rhurst$date,rhurst$XMR, col="brown")
lines(rhurst$date,rhurst$LTC, col="blue")
lines(rhurst$date,rhurst$XLM, col="pink")
lines(rhurst$date,rhurst$XRP, col="red")
abline(a=0.45,b=0)
abline(a=0.55,b=0)


plot(rhurst$date, rhurst$XRP, xlim = c(16250,19000), ylab="Hurst Exponent Value", xlab="Date", main="Hurst Exponent", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="red")
lines(rhurst$date,rhurst$LTC, col="blue")
lines(rhurst$date,rhurst$XLM, col="pink")

plot(rhurst$date, rhurst$DOGE, xlim = c(16250,19000), ylab="Hurst Exponent Value", xlab="Date", main="Hurst Exponent", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="yellow")
lines(rhurst$date,rhurst$DOGE, col="yellow")
lines(rhurst$date,rhurst$XMR, col="brown")

help("abline")


plot.default(rhurst$date, rhurst$BTC, type = "l", xlim = c(16400,18000))


#creating a percentage efficiency indicator
hurstp <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(hurstp) <- cryptlist


y <- 1
while(y<8){
  tempdata <- as.data.frame(rhurst[,y])
  
  tempdata <- na.omit(tempdata)
  colnames(tempdata) <- c("r")
  nr <- nrow(tempdata)
  percentage <- (length(which(tempdata$r> 0.45 & tempdata$r< 0.55)))/nr
  
  hurstp[1,y] <- percentage
  
  
  y <- y+1
}



#creating dataframe with results for runs
rruns <- data.frame(matrix(ncol = 7, nrow = length(seq(from=as.Date("2013-04-29"), to=as.Date("2022-04-24"), by="day"))))
colnames(rruns) <- cryptlist
rruns$date <- seq(as.Date("2013-04-29"),as.Date("2022-04-24"),by = 1)



#runs-test
nacounter <- 0
y <- 1
x <- 0
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  numrow <- nrow(tempdata) - 350
  while (x<numrow) {
    subtempdata <- subset(tempdata[c(x:(350+x)),])
    tempdat <- subtempdata$return
    resultlist <- runs.test(tempdat, pvalue = "exact")
    
    if(is.na(resultlist$p.value)){
      nacounter <- nacounter+1
    }
    
    rruns[rruns$date== (min(tempdata$time_close))+x+350,y] <- resultlist$p.value
    x <- x+1
  }
  
  x <- 0
  y <- y+1
}


plot(rruns$date, rruns$BTC, xlim = c(16250,19000), ylab="Runs-test p-value", xlab="Date", main="Runs-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1))
lines(rruns$date,rruns$ETH, col="green")
lines(rruns$date,rruns$DOGE, col="yellow")
lines(rruns$date,rruns$XMR, col="brown")
lines(rruns$date,rruns$LTC, col="blue")
lines(rruns$date,rruns$XLM, col="pink")
lines(rruns$date,rruns$XRP, col="red")
abline(a=0.05,b=0)



plot(rruns$date, rruns$XRP, xlim = c(16250,19000), ylab="Runs-test p-value", xlab="Date", main="Runs-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="red")
lines(rruns$date,rruns$LTC, col="blue")
lines(rruns$date,rruns$XLM, col="pink")
abline(a=0.05,b=0)

plot(rruns$date, rruns$DOGE, xlim = c(16250,19000), ylab="Runs-test p-value", xlab="Date", main="Runs-Test", type="l", 
     sub="BTC=black, ETH=green, XRP=red, LTC=blue, DOGE=yellow, XLM=pink, XMR=brown",ylim=c(0,1),col="yellow")
lines(rruns$date,rruns$XMR, col="brown")
abline(a=0.05,b=0)




plot.default(rruns$date, rruns$BTC, type = "l", xlim = c(16400,18000))

#creating a percentage efficiency indicator
runsp <- data.frame(matrix(ncol = 7, nrow = 1))
colnames(runsp) <- cryptlist


y <- 1
while(y<8){
  tempdata <- as.data.frame(rruns[,y])
  
  tempdata <- na.omit(tempdata)
  
  
  colnames(tempdata) <- c("r")
  percentage <- length(which(tempdata$r< 0.05))/nrow(tempdata)
  
  
  runsp[1,y] <- percentage
  
  
  y <- y+1
}






benchmarks <- c("Wilcox","Sharpe Ratio Strategy","Sharpe Ratio BH","p-value diff sharpe","Sortino Ratio Strategy","Sortino Ratio BH")


#trading with dvi
dvi <- data.frame(matrix(ncol = 7, nrow = 6))
colnames(dvi) <- cryptlist
rownames(dvi) <- benchmarks

y <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  r <- tempdata$return/100
  r <- na.omit(r)
  r <- as.data.frame(r)
  
  p <- tempdata$close
  p <- na.omit(p)
  dvival <- DVI(p)
  signal <- lag(ifelse(dvival < 0.5, -1, 1))
  ret <- as.data.frame(ROC(p)*signal)
  ret <- na.omit(ret)
  r <- r[c((nrow(r)-nrow(ret)+1):nrow(r)),]
  res <- as.data.frame(exp(cumsum(ret)))
  resbh <- as.data.frame(exp(cumsum(r)))
  colnames(resbh) <- c("r")


  
  dates <- na.omit(tempdata)
  dates <- tempdata$time_close
  dates <- dates[c((length(r)-nrow(ret)+1):length(r))]
  
  plot(dates,res$dvi, ylab="Wert Strategie", xlab="Date",main="DVI", type="l")
  lines(dates,resbh$r, col="green")
 # plot(dates,resbh$r, ylab="Wert Strategie", xlab="Date",main="BH", type="l")

  wilc     <-  wilcox.test(r,ret$dvi, paired = TRUE, alternative = "less")
  dvi[1,y] <- wilc$p.value
  
#  test <- shapiro.test(r)
 # dvi[2,y] <- test$p.value
 # test <- shapiro.test(ret$dvi)
 # dvi[3,y] <- test$p.value
  
#  test <- t.test(r, ret$dvi,paired = TRUE, alternative = "less" )
 # dvi[4,y] <- test$p.value
  
  dvi[2,y] <- mean(ret$dvi)/sd(ret$dvi)*sqrt(250)
  dvi[3,y] <- mean(r)/sd(r)*sqrt(250)
  
  test <- 0
  test <- as.data.frame(r)
  test$dvi <- ret$dvi
  test <- sr_equality_test(as.matrix(test))
  dvi[4,y] <- test$p.value

  
  dvi[5,y] <- mean(ret$dvi)/sd(ret$dvi[ret$dvi<0])*sqrt(250)
  dvi[6,y] <- mean(r)/sd(r[r<0])*sqrt(250)
  
 # dvi[6,y] <- kurtosis(ret$dvi)
 # dvi[7,y] <- kurtosis(r)
  
#  dvi[8,y] <- mad(ret$dvi[ret$dvi<0])
#  dvi[9,y] <- mad(r[r<0])
  
  
  y <- y+1
}



#trading with cmo
cmo <- data.frame(matrix(ncol = 7, nrow = 6))
colnames(cmo) <- cryptlist
rownames(cmo) <- benchmarks

y <- 1
while(y<8){
  runner <- cryptlist[c(y)]  
  tempdata <- data[data$symbol== runner,]
  
  r <- tempdata$return/100
  r <- na.omit(r)
  r <- as.data.frame(r)
  
  p <- tempdata$close
  p <- na.omit(p)
  cmoval <- CMO(p, n=10)
  runningav <- runMean(cmoval)
  signal <- lag(ifelse(cmoval > runningav, 1, -1))
  ret <- as.data.frame(ROC(p)*signal)
  ret <- na.omit(ret)
  r <- r[c((nrow(r)-nrow(ret)+1):nrow(r)),]
  res <- as.data.frame(exp(cumsum(ret)))
  resbh <- as.data.frame(exp(cumsum(r)))
  colnames(resbh) <- c("r")
  
  colnames(res) <- c("cmo")
  colnames(ret) <- c("cmo")
  
  dates <- na.omit(tempdata)
  dates <- tempdata$time_close
  dates <- dates[c((length(r)-nrow(ret)+1):length(r))]
  
 # plot(dates,res$cmo)
  #plot(dates,resbh$r)
  
  plot(dates,res$cmo, ylab="Wert Strategie", xlab="Date",main="CMO", type="l")
  lines(dates,resbh$r, col="green")
  
  wilc     <-  wilcox.test(r,ret$cmo, paired = TRUE, alternative = "less")
  cmo[1,y] <- wilc$p.value
  
###  test <- shapiro.test(r)
##  cmo[2,y] <- test$p.value
#  test <- shapiro.test(ret$cmo)
 # cmo[3,y] <- test$p.value
  
 # test <- t.test(r, ret$cmo,paired = TRUE, alternative = "less" )
#  cmo[4,y] <- test$p.value
  
  cmo[2,y] <- mean(ret$cmo)/sd(ret$cmo)*sqrt(250)
  cmo[3,y] <- mean(r)/sd(r)*sqrt(250)
  
  test <- 0
  test <- as.data.frame(r)
  test$cmo <- ret$cmo
  test <- sr_equality_test(as.matrix(test))
  cmo[4,y] <- test$p.value
  

  
  cmo[5,y] <- mean(ret$cmo)/sd(ret$cmo[ret$cmo<0])*sqrt(250)
  cmo[6,y] <- mean(r)/sd(r[r<0])*sqrt(250)
  
  # cmo[6,y] <- kurtosis(ret$cmo)
  # cmo[7,y] <- kurtosis(r)
  
  #  cmo[8,y] <- mad(ret$cmo[ret$cmo<0])
  #  cmo[9,y] <- mad(r[r<0])
  
  
  y <- y+1
}




#skewness(log(ret$cmo+100))



#skewness(ln(r))

#abs(r)

#skewness(sort(abs(ret$cmo))-sort(abs(r)))


#sort(abs(ret$cmo),decreasing = TRUE)
#sort(abs(r),decreasing = TRUE)

#plot(sort(abs(ret$cmo))-sort(abs(r)))
#plot(sort(abs(r)))
#plot(sort(abs(ret$cmo))-sort(abs(r)))

#boxplot(ret$cmo)

#sig

#help("DVI")
#print(signal)
#sort
