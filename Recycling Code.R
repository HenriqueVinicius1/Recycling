 tim = read.table("C:/Users/henri/Desktop/NCI/Project Final Data Analystics/austin_waste_and_diversion.csv", head=TRUE,sep=",")
 summary(tim)
 describe(tim)
 
 #Determining the top waste load types#
 
 #Top 5 load types 
 
 load_type_table <- as.data.frame(table(tim$load_type))
 sorted <- load_type_table[order(load_type_table$Freq, decreasing = TRUE),]
 top5 <- sorted[c(1:5),]
 op <- par(mar=c(5,14,4,2))
 barplot(top5$Freq, names.arg = top5$Var1, las=2, horiz = T, col = "pink", main = "Top 5 Load-Types")
 rm(op)
 
 #Determining the top waste route types#
 
 route_type_table <- as.data.frame(table(tim$route_type))
 sorted <- route_type_table[order(route_type_table$Freq, decreasing = TRUE),]
 top5 <- sorted[c(1:5),]
 op <- par(mar=c(5,14,4,2))
 barplot(top5$Freq, names.arg = top5$Var1, las=2, horiz = T, col = "green", main = "Top 5 route types")
 rm(op)
 
 #Performing multiple Regression of independent variables and dependent variable#
 
 result = lm(load_weight ~ load_type + route_type + load_id,  data=tim) # Linear Regression Model#
 
 summary(result)

 #Forecasting load weight#
 
 
 install.packages("forecast")
 
 library(forecast)
 
 tim = read.table("C:/Users/henri/Desktop/NCI/Project Final Data Analystics/austin_waste_and_diversion.csv", head=TRUE,sep=",")
 tim1.ts <- ts(tim$load_weight, start = c(2003,1), end = c(2017, 3), freq = 12)
 tim1.lm <- tslm(tim1.ts ~ poly(trend, 2))
 par(mfrow = c(2, 1))
 plot(tim1.ts, xlab = "report_date", ylab = "load_weight", ylim = c(1300, 2300), bty = "l")
 box()
 lines(tim1.lm$fitted, lwd = 2)
 tim1.ts.zoom <- window(tim1.ts, start = c(2003, 1), end = c(2017, 12))
 plot(tim1.ts.zoom, xlab = "report_date", ylab = "load_weight", ylim = c(1300, 2300), bty = "l")
 box()
 


 
 #ACF ( Autocorrelation function) Lag plot for load weight#
 par(mfrow = c(1,2))
 Acf(close.ts, lag.max = 10, main = "ACF Plot for tim1")
 Acf(diff(tim1.ts,1), lag.max = 10, main = "ACF Plot for Differenced Series")
 Arima(tim1.ts, order = c(1,0,0))
 
 
 