library('strucchange')
library('astsa')
library('TSA')
library("tseries")
library("ggplot2")

######################################
##data transformation
######################################

#loading the data 
tsdata = read.csv("/Users/yangmingli/Desktop/housing/housing data.csv",header=T)
data<-tsdata[,1]
tsdata=ts(data,frequency=4,start=c(1999,3))
v_data<-as.vector(data)
#find the mean of the data
mean(v_data)
#find the median of the data
median(v_data)
#find the minimum maxmium of the data (index)
mi<-min(v_data)
mi_time<-which.min(v_data)
maxi<-max(v_data)
max_time<-which.max(v_data)



adf.test(tsdata,alternative = "stationary")
log_data<-log(tsdata)
adf.test(log_data,alternative = "stationary")
#plot the data
plot.ts(tsdata,ylab="housing completions",main="housing completions 1999Q3-2019Q3")
#plot the transformation of the logged data
plot.ts(log_data,ylab="housing completions",main="housing completions log transformation")
#dicky fuller test for the logged data
adf.test(log_data,alternative = "stationary")
#dicky fuller test for the logged data with first difference
d_ts<-diff(log_data,1)
#plot the data 
plot(d_ts,main="first difference")
#dicky fuller test with the differenced data
adf.test(d_ts,alternative = "stationary")
#dicky fuller test with seasonal differenced data
s_diff<-diff(d_ts,4)
#plot the seasonal differenced data
plot(s_diff,main="seasonal difference")
#acf test
acf2(s_diff)
#dicky fuller test for the seasonal differenced data
adf.test(s_diff,alternative = "stationary")
#plot the oringinal data, logged data, difference and seasonal differenced data
plot.ts(cbind(tsdata,log_data,d_ts,s_diff),main='transformation plots')

####################################
#model selections & diagnostics
##########################

#try some  models based on the acf
#1.
w_m1<-sarima(log_data,1,1,1,0,1,1,4)
w_m1
shapiro.test(resid(w_m1$fit))
#2
w_m2<-sarima(log_data,1,1,0,0,1,1,4)
w_m2
shapiro.test(resid(w_m1$fit))
#2
w_m3<-sarima(log_data,0,1,1,0,1,1,4)
w_m3
shapiro.test(resid(w_m3$fit))


#final model
#model1 (0,1,0,0,1,1)4
Model1<-sarima(log_data,0,1,0,0,1,1,4)
Model1
shapiro.test(resid(Model1$fit))

#calculate the mean square error
#cut the data from 1 to 71
d_data<-log_data[1:71]
#the oberserved last 10 points
true_data<-log_data[72:81]

#transfer it into the time series types
newdata<-ts(d_data,frequency=4,start=c(1999,3))
#use the sarima for to forecast the 10 points.
model_pre<-sarima.for(newdata,n.ahead=10,0,1,0,0,1,1,4)
#find the predict value
estimator<-model_pre$pred
#predict standard error
estimator_se<-model_pre$se

# find the mean square error of model1(logged )
esti_L<-as.vector(estimator)
length(esti_L)
length(true_data)
total<-1/10*sum((esti_L-true_data)^2)

#total 0.007533216




#Model1=sarima(log_data, 6, 0, 2, P = 0, D = 1, Q = 1, S = 4,  
#              details = TRUE, xreg=Time2008, tol = sqrt(.Machine$double.eps),  
#              no.constant = FALSE)
#Model1



#find the indicator time
Time=time(log_data)
(Time2009=c(Time>=2009))

#find the model2
Model2=sarima(log_data, 1, 0, 0, P = 0, D = 1, Q = 1, S = 4,  
              details = TRUE, xreg=Time2009, tol = sqrt(.Machine$double.eps),  
              no.constant = FALSE)

Model2




#find the mean square error of model2(logged)
d_data<-log_data[1:71]
true_data<-log_data[72:81]

newdata<-ts(d_data,frequency=4,start=c(1999,3))
model_pre<-sarima.for(newdata,n.ahead=10,1,0,0,0,1,1,4)
estimator<-model_pre$pred

esti_L<-as.vector(estimator)
length(esti_L)
length(true_data)


total<-1/10*sum((esti_L-true_data)^2)
total


#compare the two models
plot(log_data,type="o",ylab="house completions",main="model comparison")

fitted1=log_data-resid(Model1$fit)
lines(fitted1, lty=3, col="red")

fitted2=log_data-resid(Model2$fit)
lines(fitted2, lty=2, col="blue")

legend("bottomleft",
       legend=c("Observed data", "Fitted values with the level-shift model",
                "Fitted values without level-shift model"),
       lty=c(1,2,3), col=c("black","blue","red"),cex=0.7)

####################################
#final forecasting
####################################





#prediction for the future values.
final_for<-sarima.for(log_data,n.ahead=10,1, 0, 0, P = 0, D = 1, Q = 1, S = 4)
pred_v<-final_for$pred
se_v<-final_for$se
value<-exp(pred_v)
value
upperpred<-exp(pred_v+2*se_v)
underpred<-exp(pred_v-2*se_v)







#Prediction for  prediction intervals on a graph
plot(tsdata,type="l",lwd=3,ylab="housing completions",main="forecast 2019Q3-2022Q1",ylim=c(0,40000),xlim=c(1999,2022))
lines(value,lwd=6,col="blue")
lines(upperpred,lty=1,lwd=1,col="red") 
lines(underpred,lty=1,lwd=1,col="red")
xx=c(time(upperpred),rev(time(upperpred)))
yy=c(underpred,rev(upperpred))
polygon(xx,yy,border=8,col=gray(0.6,alpha=0.2))

legend("bottomleft",
       legend=c("original plot","forecast value", "prediction intervals"),
       lwd=c(3,6,1), col=c("black","blue","red"),cex=0.7)




#Prediction for  prediction intervals on a graph (scarled)
#
plot(tsdata,type="l",lwd=3,ylab="housing completions",main="forecast 2019Q3-2022Q1",ylim=c(5000,30000),xlim=c(2009,2022))
lines(value,lwd=6,col="blue")
lines(upperpred,lty=1,lwd=1,col="red") 
lines(underpred,lty=1,lwd=1,col="red")
xx=c(time(upperpred),rev(time(upperpred)))
yy=c(underpred,rev(upperpred))
polygon(xx,yy,border=8,col=gray(0.6,alpha=0.2))

legend("bottomleft",
       legend=c("original plot","forecast value", "prediction intervals"),
       lwd=c(3,6,1), col=c("black","blue","red"),cex=0.7)