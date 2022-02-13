library(fpp2)
CA=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/CA_usa.csv")
NY=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/NY_usa.csv")
MI=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/Michigan_usa.csv")
NJ=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/New Jersey_usa.csv")
CT=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/CT_usa.csv")
FL=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/FL_usa.csv")
LA=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/LA_usa.csv")
MA=read.csv("C:/Users/yyu3/Documents/QTM3615 Time series/MA_usa.csv")
##################################
#Confirmed 
ts_CA = ts(CA$Confirmed,start = c(1,1),frequency=365)
ts_NY = ts(NY$Confirmed,start = c(1,1),frequency=365)
ts_MI = ts(MI$Confirmed,start = c(1,1),frequency=365)
ts_NJ = ts(NJ$Confirmed,start = c(1,1),frequency=365)
ts_CT = ts(CT$Confirmed,start = c(1,1),frequency=365)
ts_FL = ts(FL$Confirmed,start = c(1,1),frequency=365)
ts_LA = ts(LA$Confirmed,start = c(1,1),frequency=365)
ts_MA = ts(MA$Confirmed,start = c(1,1),frequency=365)

test_CA = tail(ts_CA, max(length(ts_CA)*0.2, 7)) 
training_CA =head(ts_CA,length(ts_CA) - length(test_CA))

test_NY = tail(ts_NY, max(length(ts_NY)*0.2, 7)) 
training_NY =head(ts_NY,length(ts_NY) - length(test_NY))

test_MI = tail(ts_MI, max(length(ts_MI)*0.2, 7)) 
training_MI =head(ts_MI,length(ts_MI) - length(test_MI))

test_NJ = tail(ts_NJ, max(length(ts_NJ)*0.2, 7)) 
training_NJ =head(ts_NJ,length(ts_NJ) - length(test_NJ))

test_CT = tail(ts_CT, max(length(ts_CT)*0.2, 7)) 
training_CT =head(ts_CT,length(ts_CT) - length(test_CT))

test_FL = tail(ts_FL, max(length(ts_FL)*0.2, 7)) 
training_FL =head(ts_FL,length(ts_FL) - length(test_FL))

test_LA = tail(ts_LA, max(length(ts_LA)*0.2, 7)) 
training_LA =head(ts_LA,length(ts_LA) - length(test_LA))

test_MA = tail(ts_MA, max(length(ts_MA)*0.2, 7)) 
training_MA =head(ts_MA,length(ts_MA) - length(test_MA))

###############################
# Naive-Confirmed 
Naive_CA_confirmed = naive(training_CA, length(test_CA))  
Naive_CA_confirmed
checkresiduals (Naive_CA_confirmed)  
# not adequate

Naive_NY_confirmed = naive(training_NY, length(test_NY))  
Naive_NY_confirmed
checkresiduals (Naive_NY_confirmed)  
#not adequate 

Naive_MI_confirmed = naive(training_MI, length(test_MI))  
Naive_MI_confirmed
checkresiduals (Naive_MI_confirmed)  
#not adequate

Naive_NJ_confirmed = naive(training_NJ, length(test_NJ))  
Naive_NJ_confirmed
checkresiduals (Naive_NJ_confirmed)  
#not adequate

Naive_CT_confirmed = naive(training_CT, length(test_CT))  
Naive_CT_confirmed
checkresiduals (Naive_CT_confirmed)
#not adequate

Naive_FL_confirmed = naive(training_FL, length(test_FL))  
Naive_FL_confirmed
checkresiduals (Naive_FL_confirmed)
#not adequate

Naive_LA_confirmed = naive(training_LA, length(test_LA))  
Naive_LA_confirmed
checkresiduals (Naive_LA_confirmed)
#not adequate

Naive_MA_confirmed = naive(training_MA, length(test_MA))  
Naive_MA_confirmed
checkresiduals (Naive_MA_confirmed)
#not adequate
##########################
#Drift-confirmed 
Drift_CA_confirmed = rwf(training_CA, length(test_CA), drift=TRUE) 
Drift_CA_confirmed
checkresiduals (Drift_CA_confirmed) 
#not adequate

Drift_NY_confirmed = rwf(training_NY, length(test_NY), drift=TRUE) 
Drift_NY_confirmed
checkresiduals (Drift_NY_confirmed) 
#not adequate

Drift_MI_confirmed = rwf(training_MI, length(test_MI), drift=TRUE) 
Drift_MI_confirmed
checkresiduals (Drift_MI_confirmed) 
#not adequate

Drift_NJ_confirmed = rwf(training_NJ, length(test_NJ), drift=TRUE) 
Drift_NJ_confirmed
checkresiduals (Drift_NJ_confirmed) 
#not adequate

Drift_CT_confirmed = rwf(training_CT, length(test_CT), drift=TRUE) 
Drift_CT_confirmed
checkresiduals (Drift_CT_confirmed) 
#not adequate

Drift_FL_confirmed = rwf(training_FL, length(test_FL), drift=TRUE) 
Drift_FL_confirmed
checkresiduals (Drift_FL_confirmed) 
#not adequate

Drift_LA_confirmed = rwf(training_LA, length(test_LA), drift=TRUE) 
Drift_LA_confirmed
checkresiduals (Drift_LA_confirmed) 
#not adequate

Drift_MA_confirmed = rwf(training_MA, length(test_MA), drift=TRUE) 
Drift_MA_confirmed
checkresiduals (Drift_MA_confirmed) 
#not adequate
###########################
#accuracy-confirmed  
#California
accuracy(Naive_CA_confirmed,test_CA)[2,] 
accuracy(Drift_CA_confirmed,test_CA)[2,] #BETTER

#New York
accuracy(Naive_NY_confirmed,test_NY)[2,]
accuracy(Drift_NY_confirmed,test_NY)[2,]#better

#Michigan
accuracy(Naive_MI_confirmed,test_MI)[2,]
accuracy(Drift_MI_confirmed,test_MI)[2,]#better

#New Jersey
accuracy(Naive_NJ_confirmed,test_NJ)[2,]
accuracy(Drift_NJ_confirmed,test_NJ)[2,]#better

#CT
accuracy(Naive_CT_confirmed,test_CT)[2,] 
accuracy(Drift_CT_confirmed,test_CT)[2,]#Better 

#FL
accuracy(Naive_FL_confirmed,test_FL)[2,] 
accuracy(Drift_FL_confirmed,test_FL)[2,]#Better  

#LA
accuracy(Naive_LA_confirmed,test_LA)[2,] 
accuracy(Drift_LA_confirmed,test_LA)[2,] #Better 

#MA
accuracy(Naive_MA_confirmed,test_MA)[2,] 
accuracy(Drift_MA_confirmed,test_MA)[2,] #Better 
##################################
#Deaths 
ts_CA1 = ts(CA$Deaths,start = c(1,1),frequency=365)
ts_NY1 = ts(NY$Deaths,start = c(1,1),frequency=365)
ts_MI1 = ts(MI$Deaths,start = c(1,1),frequency=365)
ts_NJ1 = ts(NJ$Deaths,start = c(1,1),frequency=365)
ts_CT1 = ts(CT$Deaths,start = c(1,1),frequency=365)
ts_FL1 = ts(FL$Deaths,start = c(1,1),frequency=365)
ts_LA1 = ts(LA$Deaths,start = c(1,1),frequency=365)
ts_MA1 = ts(MA$Deaths,start = c(1,1),frequency=365)

test_CA1 = tail(ts_CA1, max(length(ts_CA1)*0.2, 7)) 
training_CA1 =head(ts_CA1,length(ts_CA1) - length(test_CA1))

test_NY1 = tail(ts_NY1, max(length(ts_NY1)*0.2, 7)) 
training_NY1 =head(ts_NY1,length(ts_NY1) - length(test_NY1))

test_MI1 = tail(ts_MI1, max(length(ts_MI1)*0.2, 7)) 
training_MI1 =head(ts_MI1,length(ts_MI1) - length(test_MI1))

test_NJ1 = tail(ts_NJ1, max(length(ts_NJ1)*0.2, 7)) 
training_NJ1 =head(ts_NJ1,length(ts_NJ1) - length(test_NJ1))

test_CT1 = tail(ts_CT1, max(length(ts_CT1)*0.2, 7)) 
training_CT1 =head(ts_CT1,length(ts_CT1) - length(test_CT1))

test_FL1 = tail(ts_FL1, max(length(ts_FL1)*0.2, 7)) 
training_FL1 =head(ts_FL1,length(ts_FL1) - length(test_FL1))

test_LA1 = tail(ts_LA1, max(length(ts_LA1)*0.2, 7)) 
training_LA1 =head(ts_LA1,length(ts_LA1) - length(test_LA1))

test_MA1 = tail(ts_MA1, max(length(ts_MA1)*0.2, 7)) 
training_MA1 =head(ts_MA1,length(ts_MA1) - length(test_MA1))
###################
#Naive-Deaths 
Naive_CA_death = naive(training_CA1, length(test_CA1))  
Naive_CA_death
checkresiduals (Naive_CA_death)
#not adequate

Naive_NY_death = naive(training_NY1, length(test_NY1))  
Naive_NY_death
checkresiduals (Naive_NY_death)  
#not adequate

Naive_MI_death = naive(training_MI1, length(test_MI1))  
Naive_MI_death
checkresiduals (Naive_MI_death)  
#not adequate

Naive_NJ_death = naive(training_NJ1, length(test_NJ1))  
Naive_NJ_death
checkresiduals (Naive_NJ_death)  
#not adequate

Naive_CT_death = naive(training_CT1, length(test_CT1))  
Naive_CT_death
checkresiduals (Naive_CT_death)  
#not adequate

Naive_FL_death = naive(training_FL1, length(test_FL1))  
Naive_FL_death
checkresiduals (Naive_FL_death)  
#not adequate

Naive_LA_death = naive(training_LA1, length(test_LA1))  
Naive_LA_death
checkresiduals (Naive_LA_death)  
#not adequate

Naive_MA_death = naive(training_MA1, length(test_MA1))  
Naive_MA_death
checkresiduals (Naive_MA_death)  
#not adequate
##################
#Drift-Deaths
Drift_CA_death = rwf(training_CA1, length(test_CA1), drift=TRUE) 
Drift_CA_death
checkresiduals (Drift_CA_death) 
#not adequate

Drift_NY_death = rwf(training_NY1, length(test_NY1), drift=TRUE) 
Drift_NY_death
checkresiduals (Drift_NY_death) 
#not adequate

Drift_MI_death = rwf(training_MI1, length(test_MI1), drift=TRUE) 
Drift_MI_death
checkresiduals (Drift_MI_death) 
#not adequate

Drift_NJ_death = rwf(training_NJ1, length(test_NJ1), drift=TRUE) 
Drift_NJ_death
checkresiduals (Drift_NJ_death)
#not adequate

Drift_CT_death = rwf(training_CT1, length(test_CT1), drift=TRUE) 
Drift_CT_death
checkresiduals (Drift_CT_death)
#not adequate

Drift_FL_death = rwf(training_FL1, length(test_FL1), drift=TRUE) 
Drift_FL_death
checkresiduals (Drift_FL_death)
#not adequate

Drift_LA_death = rwf(training_LA1, length(test_LA1), drift=TRUE) 
Drift_LA_death
checkresiduals (Drift_LA_death)
#not adequate

Drift_MA_death = rwf(training_MA1, length(test_MA1), drift=TRUE) 
Drift_MA_death
checkresiduals (Drift_MA_death)
#not adequate
###############################
#accuracy-Death

#California
accuracy(Naive_CA_death,test_CA1)[2,] #better
accuracy(Drift_CA_death,test_CA1)[2,] 

#New York
accuracy(Naive_NY_death,test_NY1)[2,]#better
accuracy(Drift_NY_death,test_NY1)[2,] 

#Michigan
accuracy(Naive_MI_death,test_MI1)[2,]#better 
accuracy(Drift_MI_death,test_MI1)[2,]

#New Jersey
accuracy(Naive_NJ_death,test_NJ1)[2,]#better
accuracy(Drift_NJ_death,test_NJ1)[2,]

#CT
accuracy(Naive_CT_death,test_CT1)[2,]#better
accuracy(Drift_CT_death,test_CT1)[2,]

#FL
accuracy(Naive_FL_death,test_FL1)[2,]#better
accuracy(Drift_FL_death,test_FL1)[2,]

#LA
accuracy(Naive_LA_death,test_LA1)[2,]
accuracy(Drift_LA_death,test_LA1)[2,]#better

#MA
accuracy(Naive_MA_death,test_MA1)[2,]
accuracy(Drift_MA_death,test_MA1)[2,]#better
