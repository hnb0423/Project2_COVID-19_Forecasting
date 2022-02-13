library(fpp2)
CA = read.csv("CA_usa.csv")
NY = read.csv("NY_usa.csv")
NJ = read.csv("NJ_usa.csv")
MI = read.csv("MI_usa.csv")
LA = read.csv("LA_usa.csv")
MA = read.csv("MA_usa.csv")
CT = read.csv("CT_usa.csv")
FL = read.csv("FL_usa.csv")

ts_CA = ts(CA[ ,c("Confirmed","Deaths")])
ts_NY = ts(NY[ ,c("Confirmed","Deaths")])
ts_NJ = ts(NJ[ ,c("Confirmed","Deaths")])
ts_MI = ts(MI[ ,c("Confirmed","Deaths")])
ts_LA = ts(LA[ ,c("Confirmed","Deaths")])
ts_MA = ts(MA[ ,c("Confirmed","Deaths")])
ts_CT = ts(CT[ ,c("Confirmed","Deaths")])
ts_FL = ts(FL[ ,c("Confirmed","Deaths")])

ts_CA =ts(CA$Confirmed, start =c(2020,1))
ts_CA = ts(cbind (ts_CA, t = seq(from = 1, to = length(ts_CA))))
Y = log(ts_CA)
loglog = tslm(ts_CA ~ t, Y)
summary(loglog)
predictions = forecast (loglog,log(nrow(ts_CA)+1)) 
exp(data.frame(predictions))
autoplot(ts_CA)

testca = tail(ts_CA, max(nrow(ts_CA)*0.2, 7)) 
trainingca = head(ts_CA,nrow(ts_CA) - nrow(testca))

testny = tail(ts_NY, max(nrow(ts_NY)*0.2, 7)) 
trainingny = head(ts_NY,nrow(ts_NY) - nrow(testny))

testnj = tail(ts_NJ, max(nrow(ts_NJ)*0.2, 7)) 
trainingnj = head(ts_NJ,nrow(ts_NJ) - nrow(testnj))

testmi = tail(ts_MI, max(nrow(ts_MI)*0.2, 7)) 
trainingmi = head(ts_MI,nrow(ts_MI) - nrow(testmi))

testla = tail(ts_LA, max(nrow(ts_LA)*0.2, 7)) 
trainingla = head(ts_LA,nrow(ts_LA) - nrow(testla))

testma = tail(ts_MA, max(nrow(ts_MA)*0.2, 7)) 
trainingma = head(ts_MA,nrow(ts_MA) - nrow(testma))

testct = tail(ts_CT, max(nrow(ts_CT)*0.2, 7)) 
trainingct = head(ts_CT,nrow(ts_CT) - nrow(testct))

testfl = tail(ts_FL, max(nrow(ts_FL)*0.2, 7)) 
trainingfl = head(ts_FL,nrow(ts_FL) - nrow(testfl))



# linear
linearca = tslm(Deaths ~ Confirmed, trainingca)
summary(linearca)
checkresiduals(linearca)

linearny = tslm(Deaths ~ Confirmed, trainingny)
summary(linearny)
checkresiduals(linearny)

linearnj = tslm(Deaths ~ Confirmed, trainingnj)
summary(linearnj)
checkresiduals(linearnj)

linearmi = tslm(Deaths ~ Confirmed, trainingmi)
summary(linearmi)
checkresiduals(linearmi)

linearla = tslm(Deaths ~ Confirmed, trainingla)
summary(linearla)
checkresiduals(linearla)

linearma = tslm(Deaths ~ Confirmed, trainingma)
summary(linearma)
checkresiduals(linearma)

linearct = tslm(Deaths ~ Confirmed, trainingct)
summary(linearct)
checkresiduals(linearct)

linearfl = tslm(Deaths ~ Confirmed, trainingfl)
summary(linearfl)
checkresiduals(linearfl)


# transformed: 1/x
x_1ca = tslm(Deaths ~ I(1/Confirmed), trainingca)
summary(x_1ca)
checkresiduals(x_1ca)

x_1ny = tslm(Deaths ~ I(1/Confirmed), trainingny)
summary(x_1ny)
checkresiduals(x_1ny)

x_1nj = tslm(Deaths ~ I(1/Confirmed), trainingnj)
summary(x_1nj)
checkresiduals(x_1nj)

x_1mi = tslm(Deaths ~ I(1/Confirmed), trainingmi)
summary(x_1mi)
checkresiduals(x_1mi)

x_1la = tslm(Deaths ~ I(1/Confirmed), trainingla)
summary(x_1la)
checkresiduals(x_1la)

x_1ma = tslm(Deaths ~ I(1/Confirmed), trainingma)
summary(x_1ma)
checkresiduals(x_1ms)

x_1ct = tslm(Deaths ~ I(1/Confirmed), trainingct)
summary(x_1ct)
checkresiduals(x_1ct)

x_1fl = tslm(Deaths ~ I(1/Confirmed), trainingfl)
summary(x_1fl)
checkresiduals(x_1fl)

# transformed: log(x)
logxca = tslm(Deaths ~ I(log(Confirmed)), trainingca)
summary(logxca)
checkresiduals(logxca)

logxny = tslm(Deaths ~ I(log(Confirmed)), trainingny)
summary(logxny)
checkresiduals(logxny)

logxnj = tslm(Deaths ~ I(log(Confirmed)), trainingnj)
summary(logxnj)
checkresiduals(logxnj)

logxmi = tslm(Deaths ~ I(log(Confirmed)), trainingmi)
summary(logxmi)
checkresiduals(logxmi)

logxla = tslm(Deaths ~ I(log(Confirmed)), trainingla)
summary(logxla)
checkresiduals(logxla)

logxma = tslm(Deaths ~ I(log(Confirmed)), trainingma)
summary(logxma)
checkresiduals(logxma)

logxct = tslm(Deaths ~ I(log(Confirmed)), trainingct)
summary(logxct)
checkresiduals(logxct)

logxfl = tslm(Deaths ~ I(log(Confirmed)), trainingfl)
summary(logxfl)
checkresiduals(logxfl)


# transformed: sqrt(x)
sqrt_xca = tslm(Deaths ~ I(sqrt(Confirmed)), trainingca)
summary(sqrt_xca)
checkresiduals(sqrt_xca)

sqrt_xny = tslm(Deaths ~ I(sqrt(Confirmed)), trainingny)
summary(sqrt_xny)
checkresiduals(sqrt_xny)

sqrt_xnj = tslm(Deaths ~ I(sqrt(Confirmed)), trainingnj)
summary(sqrt_xnj)
checkresiduals(sqrt_xnj)

sqrt_xmi = tslm(Deaths ~ I(sqrt(Confirmed)), trainingmi)
summary(sqrt_xmi)
checkresiduals(sqrt_xmi)

sqrt_xla = tslm(Deaths ~ I(sqrt(Confirmed)), trainingla)
summary(sqrt_xla)
checkresiduals(sqrt_xla)

sqrt_xma = tslm(Deaths ~ I(sqrt(Confirmed)), trainingma)
summary(sqrt_xma)
checkresiduals(sqrt_xma)

sqrt_xct = tslm(Deaths ~ I(sqrt(Confirmed)), trainingct)
summary(sqrt_xct)
checkresiduals(sqrt_xct)

sqrt_xfl = tslm(Deaths ~ I(sqrt(Confirmed)), trainingfl)
summary(sqrt_xfl)
checkresiduals(sqrt_xfl)


# transformed: x^2
x_sqca = tslm(Deaths ~ I(Confirmed^2), trainingca)
summary(x_sq)
checkresiduals(x_sq)

x_sqny = tslm(Deaths ~ I(Confirmed^2), trainingny)
summary(x_sq)
checkresiduals(x_sq)

x_sqnj = tslm(Deaths ~ I(Confirmed^2), trainingnj)
summary(x_sq)
checkresiduals(x_sq)

x_sqmi = tslm(Deaths ~ I(Confirmed^2), trainingmi)
summary(x_sq)
checkresiduals(x_sq)

x_sqla = tslm(Deaths ~ I(Confirmed^2), trainingla)
summary(x_sqla)
checkresiduals(x_sqla)

x_sqma = tslm(Deaths ~ I(Confirmed^2), trainingma)
summary(x_sqma)
checkresiduals(x_sqma)

x_sqct = tslm(Deaths ~ I(Confirmed^2), trainingct)
summary(x_sqct)
checkresiduals(x_sqct)

x_sqfl = tslm(Deaths ~ I(Confirmed^2), trainingfl)
summary(x_sqfl)
checkresiduals(x_sqfl)



Pred_linearca = forecast (linearca,data.frame(testca)) 
Pred_linearny = forecast (linearny,data.frame(testny)) 
Pred_linearnj = forecast (linearnj,data.frame(testnj)) 
Pred_linearmi = forecast (linearmi,data.frame(testmi)) 
Pred_linearla = forecast (linearla,data.frame(testla)) 
Pred_linearma = forecast (linearma,data.frame(testma)) 
Pred_linearct = forecast (linearct,data.frame(testct)) 
Pred_linearfl = forecast (linearfl,data.frame(testfl))

Pred_x_1ca = forecast (x_1ca,data.frame(testca)) 
Pred_x_1ny = forecast (x_1ny,data.frame(testny))
Pred_x_1nj = forecast (x_1nj,data.frame(testnj))
Pred_x_1mi = forecast (x_1mi,data.frame(testmi))
Pred_x_1la = forecast (x_1la,data.frame(testla)) 
Pred_x_1ma = forecast (x_1ma,data.frame(testma))
Pred_x_1ct = forecast (x_1ct,data.frame(testct))
Pred_x_1fl = forecast (x_1fl,data.frame(testfl))

Pred_logxca = forecast (logxca,data.frame(testca)) 
Pred_logxny = forecast (logxny,data.frame(testny))
Pred_logxnj = forecast (logxnj,data.frame(testnj))
Pred_logxmi = forecast (logxmi,data.frame(testmi))
Pred_logxla = forecast (logxla,data.frame(testla)) 
Pred_logxma = forecast (logxma,data.frame(testma))
Pred_logxct = forecast (logxct,data.frame(testct))
Pred_logxfl = forecast (logxfl,data.frame(testfl))

Pred_sqrt_xca = forecast (sqrt_xca,data.frame(testca)) 
Pred_sqrt_xny = forecast (sqrt_xny,data.frame(testny)) 
Pred_sqrt_xnj = forecast (sqrt_xnj,data.frame(testnj)) 
Pred_sqrt_xmi = forecast (sqrt_xmi,data.frame(testmi)) 
Pred_sqrt_xla = forecast (sqrt_xla,data.frame(testla)) 
Pred_sqrt_xma = forecast (sqrt_xma,data.frame(testma)) 
Pred_sqrt_xct = forecast (sqrt_xct,data.frame(testct)) 
Pred_sqrt_xfl = forecast (sqrt_xfl,data.frame(testfl)) 

Pred_x_sqca = forecast (x_sqca,data.frame(testca)) 
Pred_x_sqny = forecast (x_sqny,data.frame(testny)) 
Pred_x_sqnj = forecast (x_sqnj,data.frame(testnj)) 
Pred_x_sqmi = forecast (x_sqmi,data.frame(testmi)) 
Pred_x_sqla = forecast (x_sqla,data.frame(testla)) 
Pred_x_sqma = forecast (x_sqma,data.frame(testma)) 
Pred_x_sqct = forecast (x_sqct,data.frame(testct)) 
Pred_x_sqfl = forecast (x_sqfl,data.frame(testfl))

accuracy(Pred_linearca,testca[,"Confirmed"])[2,]
accuracy(Pred_linearny,testny[,"Confirmed"])[2,]
accuracy(Pred_linearnj,testnj[,"Confirmed"])[2,]
accuracy(Pred_linearmi,testmi[,"Confirmed"])[2,]
accuracy(Pred_linearla,testla[,"Confirmed"])[2,]
accuracy(Pred_linearma,testma[,"Confirmed"])[2,]
accuracy(Pred_linearct,testct[,"Confirmed"])[2,]
accuracy(Pred_linearfl,testfl[,"Confirmed"])[2,]

accuracy(Pred_x_1ca,testca[,"Confirmed"])[2,]
accuracy(Pred_x_1ny,testny[,"Confirmed"])[2,]
accuracy(Pred_x_1nj,testnj[,"Confirmed"])[2,]
accuracy(Pred_x_1mi,testmi[,"Confirmed"])[2,]
accuracy(Pred_x_1la,testla[,"Confirmed"])[2,]
accuracy(Pred_x_1ma,testma[,"Confirmed"])[2,]
accuracy(Pred_x_1ct,testct[,"Confirmed"])[2,]
accuracy(Pred_x_1fl,testfl[,"Confirmed"])[2,]

accuracy(Pred_logxca,testca[,"Confirmed"])[2,]
accuracy(Pred_logxny,testny[,"Confirmed"])[2,]
accuracy(Pred_logxnj,testnj[,"Confirmed"])[2,]
accuracy(Pred_logxmi,testmi[,"Confirmed"])[2,]
accuracy(Pred_logxla,testla[,"Confirmed"])[2,]
accuracy(Pred_logxma,testma[,"Confirmed"])[2,]
accuracy(Pred_logxct,testct[,"Confirmed"])[2,]
accuracy(Pred_logxfl,testfl[,"Confirmed"])[2,]

accuracy(Pred_sqrt_xca,testca[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xny,testny[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xnj,testnj[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xmi,testmi[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xla,testla[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xma,testma[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xct,testct[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xfl,testfl[,"Confirmed"])[2,]

accuracy(Pred_x_sqca,testca[,"Confirmed"])[2,]
accuracy(Pred_x_sqny,testny[,"Confirmed"])[2,]
accuracy(Pred_x_sqnj,testnj[,"Confirmed"])[2,]
accuracy(Pred_x_sqmi,testmi[,"Confirmed"])[2,]
accuracy(Pred_x_sqla,testla[,"Confirmed"])[2,]
accuracy(Pred_x_sqma,testma[,"Confirmed"])[2,]
accuracy(Pred_x_sqct,testct[,"Confirmed"])[2,]
accuracy(Pred_x_sqfl,testfl[,"Confirmed"])[2,]


New = data.frame(Confirmed = c(14.5,13))

predictions = forecast (x_1ca,New) 
predictions

#############
library(fpp2)
gd = read.csv("Guangdong_china.csv")
sc = read.csv("Sichuan_china.csv")
hb = read.csv("Hubei_china.csv")
cq = read.csv("Chongqing_china.csv")

ts_gd = ts(gd[ ,c("Confirmed","Deaths")])
ts_sc = ts(sc[ ,c("Confirmed","Deaths")])
ts_hb = ts(hb[ ,c("Confirmed","Deaths")])
ts_cq = ts(cq[ ,c("Confirmed","Deaths")])

autoplot(ts_hb)

testgd = tail(ts_gd, max(nrow(ts_gd)*0.2, 7)) 
traininggd = head(ts_gd,nrow(ts_gd) - nrow(testgd))

testsc = tail(ts_sc, max(nrow(ts_sc)*0.2, 7)) 
trainingsc = head(ts_sc,nrow(ts_sc) - nrow(testsc))

testhb = tail(ts_hb, max(nrow(ts_hb)*0.2, 7)) 
traininghb = head(ts_hb,nrow(ts_hb) - nrow(testhb))

testcq = tail(ts_cq, max(nrow(ts_cq)*0.2, 7)) 
trainingcq = head(ts_cq,nrow(ts_cq) - nrow(testcq))

#linear
lineargd = tslm(Deaths ~ Confirmed, traininggd)
summary(lineargd)
checkresiduals(lineargd)

linearsc = tslm(Deaths ~ Confirmed, trainingsc)
summary(linearsc)
checkresiduals(linearsc)

linearhb = tslm(Deaths ~ Confirmed, traininghb)
summary(linearhb)
checkresiduals(linearhb)

linearcq = tslm(Deaths ~ Confirmed, trainingcq)
summary(linearcq)
checkresiduals(linearcq)

# transformed: 1/x
x_1gd = tslm(Deaths ~ I(1/Confirmed), traininggd)
summary(x_1gd)
checkresiduals(x_1gd)

x_1sc = tslm(Deaths ~ I(1/Confirmed), trainingsc)
summary(x_1sc)
checkresiduals(x_1sc)

x_1hb = tslm(Deaths ~ I(1/Confirmed), traininghb)
summary(x_1hb)
checkresiduals(x_1hb)

x_1cq = tslm(Deaths ~ I(1/Confirmed), trainingcq )
summary(x_1cq )
checkresiduals(x_1cq )

# transformed: log(x)
logxgd = tslm(Deaths ~ I(log(Confirmed)), traininggd)
summary(logxgd)
checkresiduals(logxgd)

logxsc = tslm(Deaths ~ I(log(Confirmed)), trainingsc)
summary(logxsc)
checkresiduals(logxsc)

logxhb = tslm(Deaths ~ I(log(Confirmed)), traininghb)
summary(logxhb)
checkresiduals(logxhb)

logxcq = tslm(Deaths ~ I(log(Confirmed)), trainingcq)
summary(logxcq)
checkresiduals(logxcq)

# transformed: sqrt(x)
sqrt_xgd = tslm(Deaths ~ I(sqrt(Confirmed)), traininggd)
summary(sqrt_xgd)
checkresiduals(sqrt_xgd)

sqrt_xsc = tslm(Deaths ~ I(sqrt(Confirmed)), trainingsc)
summary(sqrt_xsc)
checkresiduals(sqrt_xsc)

sqrt_xhb = tslm(Deaths ~ I(sqrt(Confirmed)), traininghb)
summary(sqrt_xhb)
checkresiduals(sqrt_xhb)

sqrt_xcq = tslm(Deaths ~ I(sqrt(Confirmed)), trainingcq)
summary(sqrt_xcq)
checkresiduals(sqrt_xcq)

# transformed: x^2
x_sqgd = tslm(Deaths ~ I(Confirmed^2), traininggd)
summary(x_sqgd)
checkresiduals(x_sqgd)

x_sqsc = tslm(Deaths ~ I(Confirmed^2), trainingsc)
summary(x_sqsc)
checkresiduals(x_sqsc)

x_sqhb = tslm(Deaths ~ I(Confirmed^2), traininghb)
summary(x_sqhb)
checkresiduals(x_sqhb)

x_sqcq = tslm(Deaths ~ I(Confirmed^2), trainingcq)
summary(x_sqcq)
checkresiduals(x_sqcq)


Pred_lineargd = forecast (lineargd,data.frame(testgd)) 
Pred_linearsc = forecast (linearsc,data.frame(testsc)) 
Pred_linearhb = forecast (linearhb,data.frame(testhb)) 
Pred_linearcq = forecast (linearcq,data.frame(testcq)) 


Pred_x_1gd = forecast (x_1gd,data.frame(testgd)) 
Pred_x_1sc = forecast (x_1sc,data.frame(testsc))
Pred_x_1hb = forecast (x_1hb,data.frame(testhb))
Pred_x_1cq = forecast (x_1cq,data.frame(testcq))


Pred_logxgd = forecast (logxgd,data.frame(testgd)) 
Pred_logxsc = forecast (logxsc,data.frame(testsc))
Pred_logxhb = forecast (logxhb,data.frame(testhb))
Pred_logxcq = forecast (logxcq,data.frame(testcq))


Pred_sqrt_xgd = forecast (sqrt_xgd,data.frame(testgd)) 
Pred_sqrt_xsc = forecast (sqrt_xsc,data.frame(testsc)) 
Pred_sqrt_xhb = forecast (sqrt_xhb,data.frame(testhb)) 
Pred_sqrt_xcq = forecast (sqrt_xcq,data.frame(testcq)) 


Pred_x_sqgd = forecast (x_sqgd,data.frame(testgd)) 
Pred_x_sqsc = forecast (x_sqsc,data.frame(testsc)) 
Pred_x_sqhb = forecast (x_sqhb,data.frame(testhb)) 
Pred_x_sqcq = forecast (x_sqcq,data.frame(testcq)) 



accuracy(Pred_lineargd,testgd[,"Confirmed"])[2,]
accuracy(Pred_linearsc,testsc[,"Confirmed"])[2,]
accuracy(Pred_linearhb,testhb[,"Confirmed"])[2,]
accuracy(Pred_linearcq,testcq[,"Confirmed"])[2,]


accuracy(Pred_x_1gd,testgd[,"Confirmed"])[2,]
accuracy(Pred_x_1sc,testsc[,"Confirmed"])[2,]
accuracy(Pred_x_1hb,testhb[,"Confirmed"])[2,]
accuracy(Pred_x_1cq,testcq[,"Confirmed"])[2,]


accuracy(Pred_logxgd,testgd[,"Confirmed"])[2,]
accuracy(Pred_logxsc,testsc[,"Confirmed"])[2,]
accuracy(Pred_logxhb,testhb[,"Confirmed"])[2,]
accuracy(Pred_logxcq,testcq[,"Confirmed"])[2,]


accuracy(Pred_sqrt_xgd,testgd[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xsc,testsc[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xhb,testhb[,"Confirmed"])[2,]
accuracy(Pred_sqrt_xcq,testcq[,"Confirmed"])[2,]


accuracy(Pred_x_sqgd,testgd[,"Confirmed"])[2,]
accuracy(Pred_x_sqsc,testsc[,"Confirmed"])[2,]
accuracy(Pred_x_sqhb,testhb[,"Confirmed"])[2,]
accuracy(Pred_x_sqcq,testcq[,"Confirmed"])[2,]
