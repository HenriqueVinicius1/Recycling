# Recycling

#Finding dataset
tbl =  "C:/Users/henri/Desktop/NCI/Project Final Data Analystics/austin_waste_and_diversion.csv"

tim = read.table(tbl, head=TRUE,sep=",")
summary(tim)


#Determining the 5 tops waste route types#
load_type_table <- as.data.frame(table(tim$load_type))
sorted_load_type <- load_type_table[order(load_type_table$Freq, decreasing = TRUE),]
max_value_load_type <- sorted_load_type[c(1:5),]
max_name_load_type <- max_value_load_type$Var1
op <- par(mar=c(5,14,4,2))
barplot(max_value_load_type$Freq, names.arg = max_value_load_type$Var1, las=2, horiz = T, col = "red", main = "Top Load type")
rm(op)


#Descoverying the 5 top Route Number
route_number_table <- as.data.frame(table(tim$route_number))
sorted_route_number <- route_number_table[order(route_number_table$Freq, decreasing = TRUE),]
route_number <- sorted_route_number[c(1:5),]
op <- par(mar=c(5,14,4,2))
barplot(route_number$Freq, names.arg = route_number$Var1, las=2, horiz = T, col = "blue", main = "Top 5 route types")
rm(op)



#####
install.packages('dplyr')

tbl =  "C:/Users/henri/Desktop/NCI/Project Final Data Analystics/austin_waste_and_diversion.csv"
tim = read.table(tbl, head=TRUE,sep=",")


#Load Type and Max Value
load_type_table <- as.data.frame(table(tim$load_type))
sorted_load_type <- load_type_table[order(load_type_table$Freq, decreasing = TRUE),]
max_value_load_type <- sorted_load_type[c(1:1),]
max_name_load_type <- max_value_load_type$Var1

library(dplyr)
filtered_load_type <- tim%>%filter(load_type == max_name_load_type)
filtered_load_type2 <- tim%>%filter(load_weight == "1562821")

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


