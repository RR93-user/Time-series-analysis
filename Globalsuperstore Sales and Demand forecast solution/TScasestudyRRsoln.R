#Loading required libraries
library(lubridate)
library(graphics)
library(forecast)
library(tidyr)
library(tseries)
#Loading the data in csv form in R
store_main <- read.csv("Global Superstore.csv", stringsAsFactors = F)
str(store_main)
store_main<- store_main[,-1] # As RowID is not useful for analysis, we remove it
sum(is.na(store_main)) # Postal code for all markets except US is absent
#Converting into date format
store_main$Order.Date<- dmy(store_main$Order.Date)
store_main$Ship.Date<- dmy(store_main$Ship.Date)

store_main<- separate(store_main,col = Order.Date, into = c("year", "month"), sep = "-")



#Note: Except for US Postal code is Not Available(NA) for other Markets, so we would be removing Postal code from all the Market 

#cmer stands for consumer
cmer_apac_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "APAC") 
cmer_apac_store <- cmer_apac_store[,-12]

cmer_US_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "US") 
cmer_US_store<- cmer_US_store[,-12]
cmer_africa_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "Africa") 
cmer_africa_store <- cmer_africa_store[,-12]

cmer_canada_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "Canada") 
cmer_canada_store <- cmer_canada_store[,-12]

cmer_EMEA_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "EMEA") 
cmer_EMEA_store  <- cmer_EMEA_store[,-12]

cmer_EU_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "EU")
cmer_EU_store <- cmer_EU_store[,-12]

cmer_latam_store <- subset(store_main, store_main$Segment == "Consumer" & store_main$Market == "LATAM")
cmer_latam_store<- cmer_latam_store[,-12]

#corp stands for corporate
corp_apac_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "APAC") 
corp_apac_store<- corp_apac_store[,-12]

corp_US_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "US") 
corp_US_store<-corp_US_store[,-12]

corp_africa_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "Africa") 
corp_africa_store<-corp_africa_store[,-12]

corp_canada_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "Canada") 
corp_canada_store <-corp_canada_store[,-12] 

corp_EMEA_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "EMEA") 
corp_EMEA_store<- corp_EMEA_store[,-12]

corp_EU_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "EU") 
corp_EU_store<- corp_EU_store[,-12]

corp_latam_store <- subset(store_main, store_main$Segment == "Corporate" & store_main$Market == "LATAM") 
corp_latam_store<-corp_latam_store[,-12]

#ho stands for Home Office.
ho_apac_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "APAC") 
ho_apac_store<- ho_apac_store[,-12]

ho_US_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "US") 
ho_US_store<- ho_US_store[,-12]

ho_africa_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "Africa") 
ho_africa_store<-ho_africa_store[,-12]

ho_canada_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "Canada") 
ho_canada_store <-ho_canada_store[,-12] 

ho_EMEA_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "EMEA") 
ho_EMEA_store<- ho_EMEA_store[,-12]

ho_EU_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "EU") 
ho_EU_store<- ho_EU_store[,-12]

ho_latam_store <- subset(store_main, store_main$Segment == "Home Office" & store_main$Market == "LATAM") 
ho_latam_store<-ho_latam_store[,-12]

#Performing roll up operation by profit, quantity and sales to check which market should be considered for analysis.

#consumer segment aggregation across all markets
p.s.q_apac_cmer <- aggregate(cmer_apac_store[,c("Profit", "Sales", "Quantity")], list(cmer_apac_store$month,cmer_apac_store$year),sum)
colnames(p.s.q_apac_cmer) <- c('Month', 'Year','Profit','Sales', 'Quantity')

p.s.q_US_cmer <- aggregate(cmer_US_store[,c("Profit", "Sales", "Quantity")], list(cmer_US_store$month,cmer_US_store$year),sum)

p.s.q_africa_cmer <- aggregate(cmer_africa_store[,c("Profit", "Sales", "Quantity")], list(cmer_africa_store$month,cmer_africa_store$year),sum)

p.s.q_canada_cmer <- aggregate(cmer_canada_store[,c("Profit", "Sales", "Quantity")], list(cmer_canada_store$month,cmer_canada_store$year),sum)

p.s.q_EMEA_cmer <- aggregate(cmer_EMEA_store[,c("Profit", "Sales", "Quantity")], list(cmer_EMEA_store$month,cmer_EMEA_store$year),sum)

p.s.q_EU_cmer <- aggregate(cmer_EU_store[,c("Profit", "Sales", "Quantity")], list(cmer_EU_store$month,cmer_EU_store$year),sum)
colnames(p.s.q_EU_cmer) <- c('Month', 'Year','Profit','Sales', 'Quantity')

p.s.q_latam_cmer <- aggregate(cmer_latam_store[,c("Profit", "Sales", "Quantity")], list(cmer_latam_store$month,cmer_latam_store$year),sum)

#Corporate segment aggregation across all markets
p.s.q_apac_corp <- aggregate(corp_apac_store[,c("Profit", "Sales", "Quantity")], list(corp_apac_store$month,corp_apac_store$year),sum)

p.s.q_US_corp <- aggregate(corp_US_store[,c("Profit", "Sales", "Quantity")], list(corp_US_store$month,corp_US_store$year),sum)

p.s.q_africa_corp <- aggregate(corp_africa_store[,c("Profit", "Sales", "Quantity")], list(corp_africa_store$month,corp_africa_store$year),sum)

p.s.q_canada_corp <- aggregate(corp_canada_store[,c("Profit", "Sales", "Quantity")], list(corp_canada_store$month,corp_canada_store$year),sum)

p.s.q_EMEA_corp <- aggregate(corp_EMEA_store[,c("Profit", "Sales", "Quantity")], list(corp_EMEA_store$month,corp_EMEA_store$year),sum)

p.s.q_EU_corp <- aggregate(corp_EU_store[,c("Profit", "Sales", "Quantity")], list(corp_EU_store$month,corp_EU_store$year),sum)

p.s.q_latam_corp <- aggregate(corp_latam_store[,c("Profit", "Sales", "Quantity")], list(corp_latam_store$month,corp_latam_store$year),sum)


#Home office segment aggregation across all markets
p.s.q_apac_ho <- aggregate(ho_apac_store[,c("Profit", "Sales", "Quantity")], list(ho_apac_store$month,ho_apac_store$year),sum)

p.s.q_US_ho <- aggregate(ho_US_store[,c("Profit", "Sales", "Quantity")], list(ho_US_store$month,ho_US_store$year),sum)

p.s.q_africa_ho <- aggregate(ho_africa_store[,c("Profit", "Sales", "Quantity")], list(ho_africa_store$month,ho_africa_store$year),sum)

p.s.q_canada_ho <- aggregate(ho_canada_store[,c("Profit", "Sales", "Quantity")], list(ho_canada_store$month,ho_canada_store$year),sum)

p.s.q_EMEA_ho <- aggregate(ho_EMEA_store[,c("Profit", "Sales", "Quantity")], list(ho_EMEA_store$month,ho_EMEA_store$year),sum)

p.s.q_EU_ho <- aggregate(ho_EU_store[,c("Profit", "Sales", "Quantity")], list(ho_EU_store$month,ho_EU_store$year),sum)

p.s.q_latam_ho <- aggregate(ho_latam_store[,c("Profit", "Sales", "Quantity")], list(ho_latam_store$month,ho_latam_store$year),sum)

#Note: max function is used because we want to find out which market and segment are most profitable
# comparing consumer segment profit in the 7 markets
cmer_profit<- c(max(p.s.q_africa_cmer$Profit),max(p.s.q_apac_cmer$Profit),max(p.s.q_canada_cmer$Profit),
                max(p.s.q_EMEA_cmer$Profit),max(p.s.q_EU_cmer$Profit),max(p.s.q_latam_cmer$Profit),
                max(p.s.q_US_cmer$Profit))
cmer_profit<- data.frame(cmer_profit)
names(cmer_profit) <- "Profit"
Market <- c("Africa", "APAC", "CANADA", "EMEA", "EU", "LATAM", "US") 

cmer_profit<- cbind(cmer_profit, Market) 

# comparing corporate segment profit in the 7 markets
corp_profit<- c(max(p.s.q_africa_corp$Profit),max(p.s.q_apac_corp$Profit),max(p.s.q_canada_corp$Profit),
                max(p.s.q_EMEA_corp$Profit),max(p.s.q_EU_corp$Profit),max(p.s.q_latam_corp$Profit),
                max(p.s.q_US_corp$Profit))
corp_profit<- data.frame(corp_profit)
names(corp_profit) <- "Profit"
Market <- c("Africa", "APAC", "CANADA", "EMEA", "EU", "LATAM", "US") 
corp_profit<- cbind(corp_profit, Market) 


# comparing home office segment profit in the 7 markets
ho_profit<- c(max(p.s.q_africa_ho$Profit),max(p.s.q_apac_ho$Profit),max(p.s.q_canada_ho$Profit),
              max(p.s.q_EMEA_ho$Profit),max(p.s.q_EU_ho$Profit),max(p.s.q_latam_ho$Profit),
              max(p.s.q_US_ho$Profit))
ho_profit<- data.frame(ho_profit)
names(ho_profit) <- "Profit"
Market <- c("Africa", "APAC", "CANADA", "EMEA", "EU", "LATAM", "US") 

ho_profit<- cbind(ho_profit, Market) 
#creating a function to calculate Coefficient Variation(CV)
cv<- function(p)
{sd(p)/mean(p)
}
cv(cmer_profit$Profit) #Test run 
# calculating Coefficient Variations for all the segments and markets
#Consumer
Cv_cmer <- c(cv(p.s.q_africa_cmer$Profit),cv(p.s.q_apac_cmer$Profit),cv(p.s.q_canada_cmer$Profit),
             cv(p.s.q_EMEA_cmer$Profit),cv(p.s.q_EU_cmer$Profit),cv(p.s.q_latam_cmer$Profit),cv(p.s.q_US_cmer$Profit))
CV_cmer <- data.frame(Cv_cmer)
names(CV_cmer)<- "Coefficient Variation"
CV_cmer<- cbind(CV_cmer, Market)

#Corporate
Cv_corp <- c(cv(p.s.q_africa_corp$Profit),cv(p.s.q_apac_corp$Profit),cv(p.s.q_canada_corp$Profit),
             cv(p.s.q_EMEA_corp$Profit),cv(p.s.q_EU_corp$Profit),cv(p.s.q_latam_corp$Profit),cv(p.s.q_US_corp$Profit))
CV_corp<- data.frame(Cv_corp)
names(CV_corp)<- "Coefficient Variation"
CV_corp<- cbind(CV_corp, Market)

#Home office
Cv_ho <- c(cv(p.s.q_africa_ho$Profit),cv(p.s.q_apac_ho$Profit),cv(p.s.q_canada_ho$Profit),
           cv(p.s.q_EMEA_ho$Profit),cv(p.s.q_EU_ho$Profit),cv(p.s.q_latam_ho$Profit),cv(p.s.q_US_ho$Profit))
CV_ho<- data.frame(Cv_ho)
names(CV_ho)<- "Coefficient Variation"
CV_ho<- cbind(CV_ho, Market)

# As per the aggregation on profit and CV values the most profitable segment is Consumer and the most profitable markets are EU ans APAC
#  Market    Profit        CV
#   APAC    12869.988     0.6321323
#   EU      9614.156      0.6243052

#------------------------Now we convert the required data in time series for APAC market-------------------------#
#We will create seperate dataframe frames for Demand and Sales. 

#-----------We will start by forecasting Demand for APAC market---------------------------#
rawdata_dmand_apac <- as.data.frame(p.s.q_apac_cmer[,c("Month", "Quantity")])
rawdata_dmand_apac$Month<- seq(1:48)
nrow(rawdata_dmand_apac) #which is 48

#Let's create the model using the first 42 rows as instructed
#Then we can test the model on the remaining 6 rows later

ttimeser_dmand_apac<- ts(rawdata_dmand_apac$Quantity) #ttimser = total timseries
indata_dmand_apac <- rawdata_dmand_apac[1:42,]
timeser_dmand_apac<- ts(indata_dmand_apac$Quantity)
plot(timeser_dmand_apac)

#Smoothing the series - Moving Average Smoothing
w <-2
smoothedseries_dmand_apac <- filter(timeser_dmand_apac, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)
lines(smoothedseries_dmand_apac, col="blue", lwd=2)

#Smoothing left end of the time series

diff <- smoothedseries_dmand_apac[w+2] - smoothedseries_dmand_apac[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_dmand_apac[i] <- smoothedseries_dmand_apac[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_dmand_apac)
diff <- smoothedseries_dmand_apac[n-w] - smoothedseries_dmand_apac[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_dmand_apac[i] <- smoothedseries_dmand_apac[i-1] + diff
}

#Plot the smoothed time series
timevals_dmand_apac <- indata_dmand_apac$Month
lines(smoothedseries_dmand_apac, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_dmand_apac <- as.data.frame(cbind(timevals_dmand_apac, as.vector(smoothedseries_dmand_apac)))
colnames(smootheddf_dmand_apac) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_dmand_apac <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=smootheddf_dmand_apac)
global_pred_dmand_apac <- predict(lmfit_dmand_apac, Month=timevals_dmand_apac)
summary(global_pred_dmand_apac)
lines(timevals_dmand_apac, global_pred_dmand_apac, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_dmand_apac <- timeser_dmand_apac-global_pred_dmand_apac
plot(local_pred_dmand_apac, col='red', type = "l")
acf(local_pred_dmand_apac)
acf(local_pred_dmand_apac, type="partial")
armafit_dmand_apac <- auto.arima(local_pred_dmand_apac)

tsdiag(armafit_dmand_apac)
armafit_dmand_apac

#We'll check if the residual series is white noise
resi_dmand_apac <- local_pred_dmand_apac-fitted(armafit_dmand_apac)

adf.test(resi_dmand_apac,alternative = "stationary")
kpss.test(resi_dmand_apac)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_dmand_apac <- rawdata_dmand_apac[43:48,]
timevals_out_dmand_apac <- outdata_dmand_apac$Month

global_pred_out_dmand_apac <- predict(lmfit_dmand_apac,data.frame(Month =timevals_out_dmand_apac))

fcast_dmand_apac <- global_pred_out_dmand_apac

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast_dmand_apac,outdata_dmand_apac[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_dmand_apac <- c(ts(global_pred_dmand_apac),ts(global_pred_out_dmand_apac))
plot(ttimeser_dmand_apac, col = "black")
lines(class_dec_pred_dmand_apac, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_apac_dmand <- auto.arima(timeser_dmand_apac)
autoarima_apac_dmand
tsdiag(autoarima_apac_dmand)
plot(autoarima_apac_dmand$x, col="black")
lines(fitted(autoarima_apac_dmand), col="red")

#Again, let's check if the residual series is white noise

resi_autoarima_apac_dmand <- timeser_dmand_apac - fitted(autoarima_apac_dmand)

adf.test(resi_autoarima_apac_dmand,alternative = "stationary")
kpss.test(resi_autoarima_apac_dmand)

#Also, let's evaluate the model using MAPE
fcast_autoarima_dmand_apac <- predict(autoarima_apac_dmand, n.ahead = 6)

MAPE_autoarima_dmand_apac <- accuracy(fcast_autoarima_dmand_apac$pred,outdata_dmand_apac[,2])[5]
MAPE_autoarima_dmand_apac

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

autoarima_pred_dmand_apac <- c(fitted(autoarima_apac_dmand),ts(fcast_autoarima_dmand_apac$pred))
plot(ttimeser_dmand_apac, col = "black")
lines(autoarima_pred_dmand_apac, col = "red")
#As per the MAPE values AutoARIMA model is better for forecasting demand
#---------------------------------Now we will forecast for Sales for APAC market-------------------------------------#

rawdata_sales_apac <- as.data.frame(p.s.q_apac_cmer[,c("Month", "Sales")])
rawdata_sales_apac$Month<- seq(1:48)
nrow(rawdata_sales_apac) #which is 48

#Let's create the model using the first 42 rows as instructed
#Then we can test the model on the remaining 6 rows later

ttimeser_sales_apac<- ts(rawdata_sales_apac$Sales) #ttimser = total timseries
indata_sales_apac <- rawdata_sales_apac[1:42,]
timeser_sales_apac<- ts(indata_sales_apac$Sales)
plot(timeser_sales_apac)

#Smoothing the series - Moving Average Smoothing
w <-2
smoothedseries_sales_apac <- filter(timeser_sales_apac, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)
lines(smoothedseries_sales_apac, col="blue", lwd=2)

#Smoothing left end of the time series
diff <- smoothedseries_sales_apac[w+2] - smoothedseries_sales_apac[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_sales_apac[i] <- smoothedseries_sales_apac[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeser_sales_apac)
diff <- smoothedseries_sales_apac[n-w] - smoothedseries_sales_apac[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_sales_apac[i] <- smoothedseries_sales_apac[i-1] + diff
}

#Plot the smoothed time series

timevals_sales_apac <- indata_sales_apac$Month
lines(smoothedseries_sales_apac, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_sales_apac <- as.data.frame(cbind(timevals_sales_apac, as.vector(smoothedseries_sales_apac)))
colnames(smootheddf_sales_apac) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_sales_apac <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=smootheddf_sales_apac)
global_pred_sales_apac <- predict(lmfit_sales_apac, Month=timevals_sales_apac)
summary(global_pred_sales_apac)
lines(timevals_sales_apac, global_pred_sales_apac, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_sales_apac <- timeser_sales_apac-global_pred_sales_apac
plot(local_pred_sales_apac, col='red', type = "l")
acf(local_pred_sales_apac)
acf(local_pred_sales_apac, type="partial")
armafit_sales_apac <- auto.arima(local_pred_sales_apac)

tsdiag(armafit_sales_apac)
armafit_sales_apac

#We'll check if the residual series is white noise

resi_sales_apac <- local_pred_sales_apac-fitted(armafit_sales_apac)

adf.test(resi_sales_apac,alternative = "stationary")
kpss.test(resi_sales_apac)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_sales_apac <- rawdata_sales_apac[43:48,]
timevals_out_sales_apac <- outdata_sales_apac$Month

global_pred_out_sales_apac <- predict(lmfit_sales_apac,data.frame(Month =timevals_out_sales_apac))

fcast_sales_apac_apac <- global_pred_out_sales_apac

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast_sales_apac_apac,outdata_sales_apac[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_sales_apac <- c(ts(global_pred_sales_apac),ts(global_pred_out_sales_apac))
plot(ttimeser_sales_apac, col = "black")
lines(class_dec_pred_sales_apac, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_apac_sales <- auto.arima(timeser_sales_apac)
autoarima_apac_sales
tsdiag(autoarima_apac_sales)
plot(autoarima_apac_sales$x, col="black")
lines(fitted(autoarima_apac_sales), col="red")

#Again, let's check if the residual series is white noise

resi_autoarima_apac_sales <- timeser_sales_apac - fitted(autoarima_apac_sales)

adf.test(resi_autoarima_apac_sales,alternative = "stationary")
kpss.test(resi_autoarima_apac_sales)

#Also, let's evaluate the model using MAPE
fcast_autoarima_sales_apac <- predict(autoarima_apac_sales, n.ahead = 6)

MAPE_autoarima_sales_apac <- accuracy(fcast_autoarima_sales_apac$pred,outdata_sales_apac[,2])[5]
MAPE_autoarima_sales_apac

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

autoarima_pred_sales_apac <- c(fitted(autoarima_apac_sales),ts(fcast_autoarima_sales_apac$pred))
plot(ttimeser_sales_apac, col = "black")
lines(autoarima_pred_sales_apac, col = "red")
#As per the MAPE values classical decomposition model is better for forecating sales
#---------------------Now we will forecast Demand for EU Market------------------------------------#

rawdata_dmand_EU <- as.data.frame(p.s.q_EU_cmer[,c("Month", "Quantity")])
rawdata_dmand_EU$Month<- seq(1:48)
nrow(rawdata_dmand_EU) #which is 48

#Let's create the model using the first 42 rows as instructed
#Then we can test the model on the remaining 6 rows later

ttimeser_dmand_EU<- ts(rawdata_dmand_EU$Quantity) #ttimser = total timseries
indata_dmand_EU <- rawdata_dmand_EU[1:42,]
timeser_dmand_EU<- ts(indata_dmand_EU$Quantity)
plot(timeser_dmand_EU)

#Smoothing the series - Moving Average Smoothing
w <-2
smoothedseries_dmand_EU <- filter(timeser_dmand_EU, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
lines(smoothedseries_dmand_EU, col="blue", lwd=2)

#Smoothing left end of the time series

diff <- smoothedseries_dmand_EU[w+2] - smoothedseries_dmand_EU[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_dmand_EU[i] <- smoothedseries_dmand_EU[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_dmand_EU)
diff <- smoothedseries_dmand_EU[n-w] - smoothedseries_dmand_EU[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_dmand_EU[i] <- smoothedseries_dmand_EU[i-1] + diff
}

#Plot the smoothed time series

timevals_dmand_EU <- indata_dmand_EU$Month
lines(smoothedseries_dmand_EU, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_dmand_EU <- as.data.frame(cbind(timevals_dmand_EU, as.vector(smoothedseries_dmand_EU)))
colnames(smootheddf_dmand_EU) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_dmand_EU <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                     + Month, data=smootheddf_dmand_EU)
global_pred_dmand_EU <- predict(lmfit_dmand_EU, Month=timevals_dmand_EU)
summary(global_pred_dmand_EU)
lines(timevals_dmand_EU, global_pred_dmand_EU, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_dmand_EU <- timeser_dmand_EU-global_pred_dmand_EU
plot(local_pred_dmand_EU, col='red', type = "l")
acf(local_pred_dmand_EU)
acf(local_pred_dmand_EU, type="partial")
armafit_dmand_EU <- auto.arima(local_pred_dmand_EU)

tsdiag(armafit_dmand_EU)
armafit_dmand_EU

#We'll check if the residual series is white noise

resi_dmand_EU <- local_pred_dmand_EU-fitted(armafit_dmand_EU)

adf.test(resi_dmand_EU,alternative = "stationary")
kpss.test(resi_dmand_EU)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_dmand_EU <- rawdata_dmand_EU[43:48,]
timevals_out_dmand_EU <- outdata_dmand_EU$Month

global_pred_out_dmand_EU <- predict(lmfit_dmand_EU,data.frame(Month =timevals_out_dmand_EU))

fcast_dmand_EU <- global_pred_out_dmand_EU

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast_dmand_EU,outdata_dmand_EU[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_dmand_EU <- c(ts(global_pred_dmand_EU),ts(global_pred_out_dmand_EU))
plot(ttimeser_dmand_EU, col = "black")
lines(class_dec_pred_dmand_EU, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_dmand_EU <- auto.arima(timeser_dmand_EU)
autoarima_dmand_EU
tsdiag(autoarima_dmand_EU)
plot(autoarima_dmand_EU$x, col="black")
lines(fitted(autoarima_dmand_EU), col="red")

#Again, let's check if the residual series is white noise

resi_autoarima_dmand_EU <- timeser_dmand_EU - fitted(autoarima_dmand_EU)

adf.test(resi_autoarima_dmand_EU,alternative = "stationary")
kpss.test(resi_autoarima_dmand_EU)

#Also, let's evaluate the model using MAPE
fcast_autoarima_dmand_EU <- predict(autoarima_dmand_EU, n.ahead = 6)

MAPE_autoarima_dmand_EU <- accuracy(fcast_autoarima_dmand_EU$pred,outdata_dmand_EU[,2])[5]
MAPE_autoarima_dmand_EU

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

autoarima_pred_dmand_EU <- c(fitted(autoarima_dmand_EU),ts(fcast_autoarima_dmand_EU$pred))
plot(ttimeser_dmand_EU, col = "black")
lines(autoarima_pred_dmand_EU, col = "red")

#---------------------------------Now we will forecast for Sales for EU market-------------------------------------#
rawdata_sales_EU <- as.data.frame(p.s.q_EU_cmer[,c("Month", "Sales")])
rawdata_sales_EU$Month<- seq(1:48)
nrow(rawdata_sales_EU) #which is 48

#Let's create the model using the first 42 rows as instructed
#Then we can test the model on the remaining 6 rows later

ttimeser_sales_EU<- ts(rawdata_sales_EU$Sales) #ttimser = total timseries
indata_sales_EU <- rawdata_sales_EU[1:42,]
timeser_sales_EU<- ts(indata_sales_EU$Sales)
plot(timeser_sales_EU)

#Smoothing the series - Moving Average Smoothing
w <-2
smoothedseries_sales_EU <- filter(timeser_sales_EU, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
lines(smoothedseries_sales_EU, col="blue", lwd=2)

#Smoothing left end of the time series

diff <- smoothedseries_sales_EU[w+2] - smoothedseries_sales_EU[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_sales_EU[i] <- smoothedseries_sales_EU[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_sales_EU)
diff <- smoothedseries_sales_EU[n-w] - smoothedseries_sales_EU[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_sales_EU[i] <- smoothedseries_sales_EU[i-1] + diff
}

#Plot the smoothed time series

timevals_sales_EU <- indata_sales_EU$Month
lines(smoothedseries_sales_EU, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_sales_EU <- as.data.frame(cbind(timevals_sales_EU, as.vector(smoothedseries_sales_EU)))
colnames(smootheddf_sales_EU) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_sales_EU <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                     + Month, data=smootheddf_sales_EU)
global_pred_sales_EU <- predict(lmfit_sales_EU, Month=timevals_sales_EU)
summary(global_pred_sales_EU)
lines(timevals_sales_EU, global_pred_sales_EU, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_sales_EU <- timeser_sales_EU-global_pred_sales_EU
plot(local_pred_sales_EU, col='red', type = "l")
acf(local_pred_sales_EU)
acf(local_pred_sales_EU, type="partial")
armafit_sales_EU <- auto.arima(local_pred_sales_EU)

tsdiag(armafit_sales_EU)
armafit_sales_EU

#We'll check if the residual series is white noise

resi_sales_EU <- local_pred_sales_EU-fitted(armafit_sales_EU)

adf.test(resi_sales_EU,alternative = "stationary")
kpss.test(resi_sales_EU)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_sales_EU <- rawdata_sales_EU[43:48,]
timevals_out_sales_EU <- outdata_sales_EU$Month

global_pred_out_sales_EU <- predict(lmfit_sales_EU,data.frame(Month =timevals_out_sales_EU))

fcast_sales_EU_EU <- global_pred_out_sales_EU

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast_sales_EU_EU,outdata_sales_EU[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_sales_EU <- c(ts(global_pred_sales_EU),ts(global_pred_out_sales_EU))
plot(ttimeser_sales_EU, col = "black")
lines(class_dec_pred_sales_EU, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_EU_sales <- auto.arima(timeser_sales_EU)
autoarima_EU_sales
tsdiag(autoarima_EU_sales)
plot(autoarima_EU_sales$x, col="black")
lines(fitted(autoarima_EU_sales), col="red")

#Again, let's check if the residual series is white noise

resi_autoarima_EU_sales <- timeser_sales_EU - fitted(autoarima_EU_sales)

adf.test(resi_autoarima_EU_sales,alternative = "stationary")
kpss.test(resi_autoarima_EU_sales)

#Also, let's evaluate the model using MAPE
fcast_autoarima_sales_EU <- predict(autoarima_EU_sales, n.ahead = 6)

MAPE_autoarima_sales_EU <- accuracy(fcast_autoarima_sales_EU$pred,outdata_sales_EU[,2])[5]
MAPE_autoarima_sales_EU

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

autoarima_pred_sales_EU <- c(fitted(autoarima_EU_sales),ts(fcast_autoarima_sales_EU$pred))
plot(ttimeser_sales_EU, col = "black")
lines(autoarima_pred_sales_EU, col = "red")
#Clearly you can see from the mape 