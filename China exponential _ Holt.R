library(fpp2)
cq=read.csv("Chongqing_china.csv")
gd=read.csv("Guangdong_china.csv")
hb=read.csv("Hubei_china.csv")
sc=read.csv("Sichuan_china.csv")

###########################confirmed
ts_cq = ts(cq$Confirmed,start = c(1,1),frequency=365)
ts_gd = ts(gd$Confirmed,start = c(1,1),frequency=365)
ts_hb = ts(hb$Confirmed,start = c(1,1),frequency=365)
ts_sc = ts(sc$Confirmed,start = c(1,1),frequency=365)


test_cq = tail(ts_cq, max(length(ts_cq)*0.2, 7)) 
training_cq =head(ts_cq,length(ts_cq) - length(test_cq))

test_gd = tail(ts_gd, max(length(ts_gd)*0.2, 7)) 
training_gd =head(ts_gd,length(ts_gd) - length(test_gd))

test_hb = tail(ts_hb, max(length(ts_hb)*0.2, 7)) 
training_hb =head(ts_hb,length(ts_hb) - length(test_hb))

test_sc = tail(ts_sc, max(length(ts_sc)*0.2, 7)) 
training_sc =head(ts_sc,length(ts_sc) - length(test_sc))

####exponential confirmed  
ses_cq_confirmed=ses(training_cq,length(test_cq))
ses_cq_confirmed
checkresiduals(ses_cq_confirmed)

ses_gd_confirmed=ses(training_gd,length(test_gd))
ses_gd_confirmed
checkresiduals(ses_gd_confirmed)

ses_hb_confirmed=ses(training_hb,length(test_hb))
ses_hb_confirmed
checkresiduals(ses_hb_confirmed)

ses_sc_confirmed=ses(training_sc,length(test_sc))
ses_sc_confirmed
checkresiduals(ses_sc_confirmed)


#####Holt confirmed 
Holt_cq_confirmed = holt(training_cq, length(test_cq)) 
Holt_cq_confirmed 
checkresiduals (Holt_cq_confirmed) #adequate

Holt_gd_confirmed = holt(training_gd, length(test_gd)) 
Holt_gd_confirmed
checkresiduals (Holt_gd_confirmed) #adequate

Holt_hb_confirmed = holt(training_hb, length(test_hb)) 
Holt_hb_confirmed
checkresiduals (Holt_hb_confirmed) #adequate


Holt_sc_confirmed= holt(training_sc, length(test_sc))
Holt_sc_confirmed
checkresiduals (Holt_sc_confirmed)#non-adequate 

##########confirmed accuracy 
#chongqing
accuracy(ses_cq_confirmed,test_cq)[2,] 
accuracy(Holt_cq_confirmed,test_cq)[2,] #better

#guangdong
accuracy(ses_gd_confirmed,test_gd)[2,]
accuracy(Holt_gd_confirmed,test_gd)[2,] #better

#hubei
accuracy(ses_hb_confirmed,test_hb)[2,] #better
accuracy(Holt_hb_confirmed,test_hb)[2,]

#sichuan
accuracy(ses_sc_confirmed,test_sc)[2,]
accuracy(Holt_sc_confirmed,test_sc)[2,]#better

#####################deaths

ts_cq1 = ts(cq$Deaths,start = c(1,1),frequency=365)
ts_gd1 = ts(gd$Deaths,start = c(1,1),frequency=365)
ts_hb1 = ts(hb$Deaths,start = c(1,1),frequency=365)
ts_sc1 = ts(sc$Deaths,start = c(1,1),frequency=365)


test_cq1 = tail(ts_cq1, max(length(ts_cq1)*0.2, 7)) 
training_cq1 =head(ts_cq1,length(ts_cq1) - length(test_cq1))

test_gd1 = tail(ts_gd1, max(length(ts_gd1)*0.2, 7)) 
training_gd1 =head(ts_gd1,length(ts_gd1) - length(test_gd1))

test_hb1 = tail(ts_hb1, max(length(ts_hb1)*0.2, 7)) 
training_hb1 =head(ts_hb1,length(ts_hb1) - length(test_hb1))

test_sc1 = tail(ts_sc1, max(length(ts_sc1)*0.2, 7)) 
training_sc1 =head(ts_sc1,length(ts_sc1) - length(test_sc1))

####exponential Deaths  
ses_cq_deaths=ses(training_cq1,length(test_cq1))
ses_cq_deaths
checkresiduals(ses_cq_deaths)

ses_gd_deaths=ses(training_gd1,length(test_gd1))
ses_gd_deaths
checkresiduals(ses_gd_deaths)

ses_hb_deaths=ses(training_hb1,length(test_hb1))
ses_hb_deaths
checkresiduals(ses_hb_deaths)


ses_sc_deaths=ses(training_sc1,length(test_sc1))
ses_sc_deaths
checkresiduals(ses_gd_deaths)


#####Holt Deaths
Holt_cq_deaths = holt(training_cq1, length(test_cq1)) 
Holt_cq_deaths
checkresiduals (Holt_cq_deaths)

Holt_gd_deaths = holt(training_gd1, length(test_gd1)) 
Holt_gd_deaths
checkresiduals (Holt_gd_deaths)

Holt_hb_deaths = holt(training_hb1, length(test_hb1)) 
Holt_hb_deaths
checkresiduals (Holt_hb_deaths)

Holt_sc_deaths= holt(training_sc1, length(test_sc1)) 
Holt_sc_deaths
checkresiduals (Holt_sc_deaths)

########## deaths accuracy 
#chongqing
accuracy(ses_cq_deaths,test_cq1)[2,] 
accuracy(Holt_cq_deaths,test_cq1)[2,] 

#guangdong
accuracy(ses_gd_deaths,test_gd1)[2,]
accuracy(Holt_gd_deaths,test_gd1)[2,]

#hubei
accuracy(ses_hb_deaths,test_hb1)[2,] 
accuracy(Holt_hb_deaths,test_hb1)[2,]

#sichuan
accuracy(ses_sc_deaths,test_sc1)[2,]
accuracy(Holt_sc_deaths,test_sc1)[2,]


