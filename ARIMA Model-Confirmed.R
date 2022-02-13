###China
library(fpp2)
cq=read.csv("Chongqing_china.csv")
gd=read.csv("Guangdong_china.csv")
hb=read.csv("Hubei_china.csv")
sc=read.csv("Sichuan_china.csv")
ca=read.csv("CA_usa.csv")
mg=read.csv("Michigan_usa.csv")
nj=read.csv("New Jersey_usa.csv")
ny=read.csv("NY_usa.csv")
ct=read.csv("CT_usa.csv")
fl=read.csv("FL_usa.csv")
la=read.csv("LA_usa.csv")
ma=read.csv("MA_usa.csv")


cq_ts = ts(cq$Confirmed, start=c(1,1), frequency=365)
gd_ts = ts(gd$Confirmed, start=c(1,1), frequency=365)
hb_ts = ts(hb$Confirmed, start=c(1,1), frequency=365)
sc_ts = ts(sc$Confirmed, start=c(1,1), frequency=365)
ca_ts = ts(ca$Confirmed, start=c(1,1), frequency=365)
mg_ts = ts(mg$Confirmed, start=c(1,1), frequency=365)
nj_ts = ts(nj$Confirmed, start=c(1,1), frequency=365)
ny_ts = ts(ny$Confirmed, start=c(1,1), frequency=365)
ct_ts = ts(ct$Confirmed, start=c(1,1), frequency=365)
fl_ts = ts(fl$Confirmed, start=c(1,1), frequency=365)
la_ts = ts(la$Confirmed, start=c(1,1), frequency=365)
ma_ts = ts(ma$Confirmed, start=c(1,1), frequency=365)

##chongqing
test = tail(cq_ts,7) 
training = head(cq_ts,length(cq_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(cq_ts,order=c(0,2,4)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 





######guangdong
test = tail(gd_ts,7) 
training = head(gd_ts,length(gd_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(gd_ts,order=c(0,2,4)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 




####hubei
test = tail(hb_ts,7) 
training = head(hb_ts,length(hb_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(hb_ts,order=c(0,2,1)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 




####sichuan
test = tail(sc_ts,7) 
training = head(sc_ts,length(sc_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(hb_ts,order=c(5,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 



####california
test = tail(ca_ts,7) 
training = head(ca_ts,length(ca_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(ca_ts,order=c(2,2,2)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 


####michigan
test = tail(mg_ts,7) 
training = head(mg_ts,length(mg_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(ca_ts,order=c(1,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 


####new jersey
test = tail(nj_ts,7) 
training = head(nj_ts,length(nj_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(nj_ts,order=c(0,2,1)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 


####new york
test = tail(ny_ts,7) 
training = head(ny_ts,length(ny_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(nj_ts,order=c(0,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 


####CT
test = tail(ct_ts,7) 
training = head(ct_ts,length(ct_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(ct_ts,order=c(0,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 

#####fl
test = tail(fl_ts,7) 
training = head(fl_ts,length(fl_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(fl_ts,order=c(0,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 



####la
test = tail(la_ts,7) 
training = head(la_ts,length(la_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(la_ts,order=c(0,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 


####MA
test = tail(ma_ts,7) 
training = head(ma_ts,length(ma_ts) - length(test))

Model1=auto.arima(training, seasonal = FALSE, stepwise = FALSE, 
                  approximation = FALSE)
Model1


Model2=auto.arima(training, seasonal = FALSE)
Model2
#build the arima model with low AIC, rough search
#two models are the same

Model1pred = forecast(Model1,length(test)) 
Model2pred = forecast(Model2,length(test))
#predict on test set

accuracy(Model1pred,test)[2,]  #return errors
accuracy(Model2pred,test)[2,]


#rebuild arima odel on ts object after testing
Model = Arima(ma_ts,order=c(0,2,0)) 


predictions = forecast(Model, 7)  
predictions

autoplot(predictions) + 
  autolayer (fitted (predictions),series = "Fitted") + 
  ggtitle("Quarterly Loans") + ylab("Loans") +
  xlab("Year") 

