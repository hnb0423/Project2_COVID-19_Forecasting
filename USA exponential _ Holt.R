library(fpp2)
ca=read.csv("CA_usa.csv")
mich=read.csv("Michigan_usa.csv")
nj=read.csv("New Jersey_usa.csv")
ny=read.csv("NY_usa.csv")
ma=read.csv("MA_usa.csv")
la=read.csv("LA_usa.csv")
fl=read.csv("FL_usa.csv")
ct=read.csv("CT_usa.csv")

########confirmed 
ts_ca = ts(ca$Confirmed,start = c(1,1),frequency=365)
ts_mich = ts(mich$Confirmed,start = c(1,1),frequency=365)
ts_nj = ts(nj$Confirmed,start = c(1,1),frequency=365)
ts_ny = ts(ny$Confirmed,start = c(1,1),frequency=365)
ts_ma = ts(ma$Confirmed,start = c(1,1),frequency=365)
ts_la = ts(la$Confirmed,start = c(1,1),frequency=365)
ts_fl = ts(fl$Confirmed,start = c(1,1),frequency=365)
ts_ct = ts(ct$Confirmed,start = c(1,1),frequency=365)

test_ca = tail(ts_ca, max(length(ts_ca)*0.2, 7)) 
training_ca =head(ts_ca,length(ts_ca) - length(test_ca))

test_mich = tail(ts_mich, max(length(ts_mich)*0.2, 7)) 
training_mich =head(ts_mich,length(ts_mich) - length(test_mich))

test_nj = tail(ts_nj, max(length(ts_nj)*0.2, 7)) 
training_nj =head(ts_nj,length(ts_nj) - length(test_nj))

test_ny = tail(ts_ny, max(length(ts_ny)*0.2, 7)) 
training_ny =head(ts_ny,length(ts_ny) - length(test_ny))

test_ma= tail(ts_ma, max(length(ts_ma)*0.2, 7)) 
training_ma =head(ts_ma,length(ts_ma) - length(test_ma))

test_la = tail(ts_la, max(length(ts_la)*0.2, 7)) 
training_la =head(ts_la,length(ts_la) - length(test_la))

test_fl = tail(ts_fl, max(length(ts_fl)*0.2, 7)) 
training_fl =head(ts_fl,length(ts_fl) - length(test_fl))

test_ct = tail(ts_ct, max(length(ts_ct)*0.2, 7)) 
training_ct =head(ts_ct,length(ts_ct) - length(test_ct))

####exponential confirmed  
ses_ca_confirmed=ses(training_ca,length(test_ca))
ses_ca_confirmed
checkresiduals(ses_ca_confirmed)

ses_mich_confirmed=ses(training_mich,length(test_mich))
ses_mich_confirmed
checkresiduals(ses_mich_confirmed)

ses_nj_confirmed=ses(training_nj,length(test_nj))
ses_nj_confirmed
checkresiduals(ses_nj_confirmed)


ses_ny_confirmed=ses(training_ny,length(test_ny))
ses_ny_confirmed
checkresiduals(ses_ny_confirmed)


ses_ma_confirmed=ses(training_ma,length(test_ma))
ses_ma_confirmed
checkresiduals(ses_ma_confirmed)

ses_la_confirmed=ses(training_la,length(test_la))
ses_la_confirmed
checkresiduals(ses_la_confirmed)

ses_fl_confirmed=ses(training_fl,length(test_fl))
ses_fl_confirmed
checkresiduals(ses_fl_confirmed)

ses_ct_confirmed=ses(training_ct,length(test_ct))
ses_ct_confirmed
checkresiduals(ses_ct_confirmed)

#####Holt confirmed 
Holt_ca_confirmed = holt(training_ca, length(test_ca)) 
Holt_ca_confirmed 
checkresiduals (Holt_ca_confirmed) 

Holt_mich_confirmed = holt(training_mich, length(test_mich)) 
Holt_mich_confirmed
checkresiduals (Holt_mich_confirmed) 


Holt_nj_confirmed = holt(training_nj, length(test_nj)) 
Holt_nj_confirmed
checkresiduals (Holt_nj_confirmed)


Holt_ny_confirmed= holt(training_ny, length(test_ny)) 
Holt_ny_confirmed
checkresiduals (Holt_ny_confirmed) 

Holt_ma_confirmed= holt(training_ma, length(test_ma)) 
Holt_ma_confirmed
checkresiduals (Holt_ma_confirmed) 

Holt_la_confirmed= holt(training_la, length(test_la)) 
Holt_la_confirmed
checkresiduals (Holt_la_confirmed) 

Holt_fl_confirmed= holt(training_fl, length(test_fl)) 
Holt_fl_confirmed
checkresiduals (Holt_fl_confirmed) 

Holt_ct_confirmed= holt(training_ct, length(test_ct)) 
Holt_ct_confirmed
checkresiduals (Holt_ct_confirmed) 

#####confirmed accuracy
#ca
accuracy(ses_ca_confirmed,test_ca)[2,] 
accuracy(Holt_ca_confirmed,test_ca)[2,]
#mich
accuracy(ses_mich_confirmed,test_mich)[2,] 
accuracy(Holt_mich_confirmed,test_mich)[2,]
#nj
accuracy(ses_nj_confirmed,test_nj)[2,] 
accuracy(Holt_nj_confirmed,test_nj)[2,]
#ny
accuracy(ses_ny_confirmed,test_ny)[2,] 
accuracy(Holt_ny_confirmed,test_ny)[2,]
#ma
accuracy(ses_ma_confirmed,test_ma)[2,]
accuracy(Holt_ma_confirmed,test_ma)[2,]
#la
accuracy(ses_la_confirmed,test_la)[2,] 
accuracy(Holt_la_confirmed,test_la)[2,]
#fl
accuracy(ses_fl_confirmed,test_fl)[2,]
accuracy(Holt_fl_confirmed,test_fl)[2,]
#ct
accuracy(ses_ct_confirmed,test_ct)[2,] 
accuracy(Holt_ct_confirmed,test_ct)[2,]

####predictions
predictions1 = holt(ts_nj, 7)
predictions1

predictions2 = holt(ts_la, 7)
predictions2

predictions3 = holt(ts_fl, 7)
predictions3
#####deaths
ts_ca1 = ts(ca$Deaths,start = c(1,1),frequency=365)
ts_mich1 = ts(mich$Deaths,start = c(1,1),frequency=365)
ts_nj1= ts(nj$Deaths,start = c(1,1),frequency=365)
ts_ny1 = ts(ny$Deaths,start = c(1,1),frequency=365)
ts_ma1 = ts(ma$Deaths,start = c(1,1),frequency=365)
ts_la1 = ts(la$Deaths,start = c(1,1),frequency=365)
ts_fl1 = ts(fl$Deaths,start = c(1,1),frequency=365)
ts_ct1 = ts(ct$Deaths,start = c(1,1),frequency=365)


test_ca1 = tail(ts_ca1, max(length(ts_ca1)*0.2, 7)) 
training_ca1 =head(ts_ca1,length(ts_ca1) - length(test_ca1))

test_mich1 = tail(ts_mich1, max(length(ts_mich1)*0.2, 7)) 
training_mich1 =head(ts_mich1,length(ts_mich1) - length(test_mich1))

test_nj1 = tail(ts_nj1, max(length(ts_nj1)*0.2, 7)) 
training_nj1 =head(ts_nj1,length(ts_nj1) - length(test_nj1))

test_ny1 = tail(ts_ny1, max(length(ts_ny1)*0.2, 7)) 
training_ny1 =head(ts_ny1,length(ts_ny1) - length(test_ny1))

test_ma1= tail(ts_ma1, max(length(ts_ma1)*0.2, 7)) 
training_ma1 =head(ts_ma1,length(ts_ma1) - length(test_ma1))

test_la1 = tail(ts_la1, max(length(ts_la1)*0.2, 7)) 
training_la1 =head(ts_la1,length(ts_la1) - length(test_la1))

test_fl1 = tail(ts_fl1, max(length(ts_fl1)*0.2, 7)) 
training_fl1 =head(ts_fl1,length(ts_fl1) - length(test_fl1))

test_ct1 = tail(ts_ct1, max(length(ts_ct1)*0.2, 7)) 
training_ct1 =head(ts_ct1,length(ts_ct1) - length(test_ct1))

####exponential deaths 
ses_ca_deaths=ses(training_ca1,length(test_ca1))
ses_ca_deaths
checkresiduals(ses_ca_deaths)

ses_mich_deaths=ses(training_mich1,length(test_mich1))
ses_mich_deaths
checkresiduals(ses_mich_deaths)

ses_nj_deaths=ses(training_nj1,length(test_nj1))
ses_nj_deaths
checkresiduals(ses_nj_deaths)


ses_ny_deaths=ses(training_ny1,length(test_ny1))
ses_ny_deaths
checkresiduals(ses_ny_deaths)


ses_ma_deaths=ses(training_ma1,length(test_ma1))
ses_ma_deaths
checkresiduals(ses_ma_deaths)

ses_la_deaths=ses(training_la1,length(test_la1))
ses_la_deaths
checkresiduals(ses_la_deaths)

ses_fl_deaths=ses(training_fl1,length(test_fl1))
ses_fl_deaths
checkresiduals(ses_fl_deaths)

ses_ct_deaths=ses(training_ct1,length(test_ct1))
ses_ct_deaths
checkresiduals(ses_ct_deaths)



########holt deaths
Holt_ca_deaths = holt(training_ca1, length(test_ca1)) 
Holt_ca_deaths 
checkresiduals (Holt_ca_deaths) 

Holt_mich_deaths = holt(training_mich1, length(test_mich1)) 
Holt_mich_deaths
checkresiduals (Holt_mich_deaths) 


Holt_nj_deaths= holt(training_nj1, length(test_nj1)) 
Holt_nj_deaths
checkresiduals (Holt_nj_deaths)


Holt_ny_deaths= holt(training_ny1, length(test_ny1)) 
Holt_ny_deaths
checkresiduals (Holt_ny_deaths) 

Holt_ma_deaths= holt(training_ma1, length(test_ma1)) 
Holt_ma_deaths
checkresiduals (Holt_ma_deaths) 

Holt_la_deaths= holt(training_la1, length(test_la1)) 
Holt_la_deaths
checkresiduals (Holt_la_deaths) 

Holt_fl_deaths= holt(training_fl1, length(test_fl1)) 
Holt_fl_deaths
checkresiduals (Holt_fl_deaths) 

Holt_ct_deaths= holt(training_ct1, length(test_ct1)) 
Holt_ct_deaths
checkresiduals (Holt_ct_deaths) 

####deaths accuracy 
#ca
accuracy(ses_ca_deaths,test_ca1)[2,] 
accuracy(Holt_ca_deaths,test_ca1)[2,]
#mich
accuracy(ses_mich_deaths,test_mich1)[2,] 
accuracy(Holt_mich_deaths,test_mich1)[2,]
#nj
accuracy(ses_nj_deaths,test_nj1)[2,] 
accuracy(Holt_nj_deaths,test_nj1)[2,]
#ny
accuracy(ses_ny_deaths,test_ny1)[2,] 
accuracy(Holt_ny_deaths,test_ny1)[2,]
#ma
accuracy(ses_ma_deaths,test_ma1)[2,]
accuracy(Holt_ma_deaths,test_ma1)[2,]
#la
accuracy(ses_la_deaths,test_la1)[2,] 
accuracy(Holt_la_deaths,test_la1)[2,]
#fl
accuracy(ses_fl_deaths,test_fl1)[2,]
accuracy(Holt_fl_deaths,test_fl1)[2,]
#ct
accuracy(ses_ct_deaths,test_ct1)[2,] 
accuracy(Holt_ct_deaths,test_ct1)[2,]

predictions4 = holt(ts_nj1, 7) 
predictions4
predictions5= holt(ts_fl1, 7) 
predictions5
predictions6 = holt(ts_la1, 7)
predictions6
predictions7 = holt(ts_ma1, 7) 
predictions7

