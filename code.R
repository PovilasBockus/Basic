library(forecast)
library(urca)
library(dynlm)

########economically active

ea=read.delim2("clipboard",header=F)
nea=c("Total","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65+")
ea=ts(t(as.matrix(ea)),start=1990,frequency=1,names=nea)


urea=list(1:12)
area=list(1:12)
a=c(10,6,4) # indexes of empty data


for(k in 2:12)
{
#here we look at ur.df statistics and if it is greater than crit. value, we accept zero hypothesis, that our data has the unit root
urea[k]=ur.df(as.numeric(ea[11:19,k]),lags=0,selectlags="AIC",type="drift");

#here we choose model, for forecasting, knowing that our data has the unit root
area[k]=Arima(as.numeric(ea[11:19,k]), order=c(0,1,0),include.drift=TRUE);
	for(i in a)
		{
		ea[i,k]=round(as.numeric(ea[i+1,k])-as.numeric(area[k]),3);
		ea[i,1]=sum(as.numeric(ea[i,2:12]))

		}
      
}

#########forecasting ea

ureaf=list(1:12)
areaf=list(1:12)
b=c(20:23)

for(k in 2:12)
{
ureaf[k]=ur.df(as.numeric(ea[1:19,k]),lags=0,selectlags="AIC",type="drift")
areaf[k]=Arima(as.numeric(ea[1:19,k]), order=c(0,1,0),include.drift=TRUE);
	for(i in b)
		{
		ea[i,k]=round(as.numeric(ea[i-1,k])+as.numeric(areaf[k]),3);
		ea[i,1]=sum(as.numeric(ea[i,2:12]))

		}
}

############unemployment

u=read.delim2("clipboard",header=F)
u=ts(t(as.matrix(u)),start=1990,frequency=1,names=nea)


uru=list(1:12)
aru=list(1:12)
c=c(10:12)

for(k in 2:12)
{
uru[k]=ur.df(as.numeric(u[1:9,k]),lags=0,selectlags="AIC",type="drift");
aru[k]=Arima(as.numeric(u[1:9,k]), order=c(0,1,0),include.drift=TRUE);
	for(i in c)
		{
		u[i,k]=round(as.numeric(u[i-1,k])+as.numeric(aru[k]),3);
		u[i,1]=sum(as.numeric(u[i,2:12]))

		}
      
}



#########forecasting u

uruf=list(1:12)
aruf=list(1:12)
d=c(20:23)

for(k in 2:12)
{
uruf[k]=ur.df(as.numeric(u[1:19,k]),lags=0,selectlags="AIC",type="drift")
aruf[k]=Arima(as.numeric(u[1:19,k]), order=c(0,1,0),include.drift=TRUE);
	for(i in d)
		{
		u[i,k]=round(as.numeric(u[i-1,k])+as.numeric(aruf[k]),3);
		u[i,1]=sum(as.numeric(u[i,2:12]))

		}
}
#in column "65+" appears negative values, but unemployment always >=0 
for(i in 1:23){if(u[i,12]<0)u[i,12]=0}




#############

write.csv(ea, file="eaf.csv",row.names=TRUE)
write.csv(u, file="uf.csv",row.names=TRUE)

