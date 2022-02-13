library(fpp2)
cq=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/Chongqing_china .csv")
gd=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/Guangdong_china.csv")
hb=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/Hubei_china.csv")
sc=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/Sichuan_china.csv")

##################################
#Confirmed 
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

###############################
# Naive-Confirmed 
Naive_cq_confirmed = naive(training_cq, length(test_cq))  
Naive_cq_confirmed
checkresiduals (Naive_cq_confirmed)  
# not adequate

Naive_gd_confirmed = naive(training_gd, length(test_gd))  
Naive_gd_confirmed
checkresiduals (Naive_gd_confirmed)  
#not adequate 

Naive_hb_confirmed = naive(training_hb, length(test_hb))  
Naive_hb_confirmed
checkresiduals (Naive_hb_confirmed)  
#not adequate

Naive_sc_confirmed = naive(training_sc, length(test_sc))  
Naive_sc_confirmed
checkresiduals (Naive_sc_confirmed)  
#not adequate
##########################
#Drift-confirmed 
Drift_cq_confirmed = rwf(training_cq, length(test_cq), drift=TRUE) 
Drift_cq_confirmed
checkresiduals (Drift_cq_confirmed) 
#not adequate

Drift_gd_confirmed = rwf(training_gd, length(test_gd), drift=TRUE) 
Drift_gd_confirmed
checkresiduals (Drift_gd_confirmed) 
#not adequate

Drift_hb_confirmed = rwf(training_hb, length(test_hb), drift=TRUE) 
Drift_hb_confirmed
checkresiduals (Drift_hb_confirmed) 
#not adequate

Drift_sc_confirmed = rwf(training_sc, length(test_sc), drift=TRUE) 
Drift_sc_confirmed
checkresiduals (Drift_sc_confirmed) 
#not adequate

###########################
#accuracy-confirmed  
#chongqing
accuracy(Naive_cq_confirmed,test_cq)[2,] #better
accuracy(Drift_cq_confirmed,test_cq)[2,] 

#guangdong
accuracy(Naive_gd_confirmed,test_gd)[2,]#better
accuracy(Drift_gd_confirmed,test_gd)[2,] 

#hubei
accuracy(Naive_hb_confirmed,test_hb)[2,]#better
accuracy(Drift_hb_confirmed,test_hb)[2,]

#sichuan
accuracy(Naive_sc_confirmed,test_sc)[2,]#better
accuracy(Drift_sc_confirmed,test_sc)[2,]

##################################
#Deaths 
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
###################
#Naive-Deaths 
Naive_cq_death = naive(training_cq1, length(test_cq1))  
Naive_cq_death
checkresiduals (Naive_cq_death)
#not adequate

Naive_gd_death = naive(training_gd1, length(test_gd1))  
Naive_gd_death
checkresiduals (Naive_gd_death)  
#not adequate

Naive_hb_death = naive(training_hb1, length(test_hb1))  
Naive_hb_death
checkresiduals (Naive_hb_death)  
#not adequate

Naive_sc_death = naive(training_sc1, length(test_sc1))  
Naive_sc_death
checkresiduals (Naive_sc_death)  
#not adequate

##################
#Drift-Deaths
Drift_cq_death = rwf(training_cq1, length(test_cq1), drift=TRUE) 
Drift_cq_death
checkresiduals (Drift_cq_death) 
#not adequate

Drift_gd_death = rwf(training_gd1, length(test_gd1), drift=TRUE) 
Drift_gd_death
checkresiduals (Drift_gd_death) 
#not adequate

Drift_hb_death = rwf(training_hb1, length(test_hb1), drift=TRUE) 
Drift_hb_death
checkresiduals (Drift_hb_death) 
#not adequate

Drift_sc_death = rwf(training_sc1, length(test_sc1), drift=TRUE) 
Drift_sc_death
checkresiduals (Drift_sc_death)
#not adequate
###############################
#accuracy-Death
#Naive always better(generating smaller values)
#chongqing
accuracy(Naive_cq_death,test_cq1)[2,] 
accuracy(Drift_cq_death,test_cq1)[2,] 

#guangdong
accuracy(Naive_gd_death,test_gd1)[2,]
accuracy(Drift_gd_death,test_gd1)[2,] 

#hubei
accuracy(Naive_hb_death,test_hb1)[2,] 
accuracy(Drift_hb_death,test_hb1)[2,]

#sichuan
accuracy(Naive_sc_death,test_sc1)[2,]
accuracy(Drift_sc_death,test_sc1)[2,]