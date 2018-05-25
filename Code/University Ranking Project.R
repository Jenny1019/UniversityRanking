##Install packages
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)
install.packages("car")
library(VIF)
##Load the data
Ranking=read.csv("C:\\Users\\ein8\\Desktop\\2018Data_Test.csv") 


##Summary of the dataset+Summary of the top 20 dataset+Summary of ZE
summary(Ranking)
summary(Ranking[1:20,3:14])
Ranking[31,3:14]
##Initial analysis--PAS.Score,RDS.Score,NIH,NIH/FM,Tuit.Fees are below the lowest of top 20


##Data Transformation --create dummies for university type,normalize the data and eliminate some
Ranking$PP=0
Ranking$PP[which(Ranking$Public.Private=="Private")]=1
Ranking$PP=as.factor(Ranking$PP)
Ranking$Res.Score=as.numeric(Ranking$Res.Score)
Ranking$MCAT=as.numeric(Ranking$MCAT)
Ranking$Tuit.Fees=as.numeric(Ranking$Tuit.Fees)
Ranking$Enroll=as.numeric(Ranking$Enroll)
pairs.panels(Ranking)

Ranking.v=Ranking[,c(3:8,10:13,15)]
pairs.panels(Ranking.v)

hist((Ranking.v$F.S))
lines(density((Ranking.v$F.S)))
hist(log(Ranking.v$F.S))
hist(log(Ranking.v$F.S))
lines(density(log(Ranking.v$F.S)))
lines(density(log(Ranking.v$NIH...FM)))

hist((Ranking.v$Res.Score))
lines(density((Ranking.v$Res.Score)))
hist(log(Ranking.v$Res.Score))
lines(density(log(Ranking.v$Res.Score)))

Ranking.v$Res.Score.log=log(Ranking.v$Res.Score)
Ranking.v$NIH...FM.log=log(Ranking.v$NIH...FM)
Ranking.v$F.S.log=log(Ranking.v$F.S)


Ranking.vf=Ranking.v[,c(2:6,9:14)]
pairs.panels(Ranking.vf)
###shuffle the data and Split data into training dataset and validation dataset
set.seed(2000)
Train.size=0.8
Train.index=sample.int(length(Ranking.vf$Res.Score.log),round(length(Ranking.vf$Res.Score.log)*Train.size))
Ranking.Training=Ranking.vf[Train.index,]
Ranking.Validation=Ranking.vf[-Train.index,]


###Stepwise approach to further eliminate variables
a=step(lm(Res.Score.log~PAS.Score+RDS.Score+GPA+PP+Accept+NIH...FM.log+F.S.log+MCAT+Tuit.Fees+Enroll,data=Ranking.Training),direction = 'backward')

###drop PP, Tuit.Fees
a=step(lm(Res.Score.log~RDS.Score+GPA+Accept+PAS.Score+NIH...FM.log+F.S.log+MCAT+Enroll,data=Ranking.Training),direction = 'backward')

fit=lm(Res.Score.log~RDS.Score+GPA+Accept+PAS.Score+NIH...FM.log+F.S.log+MCAT+Enroll,data=Ranking.Training)
summary(fit)

###check multicolinearity
vif(fit)


###drop PAS.Score and check model diagnostics
fit=lm(Res.Score.log~RDS.Score+GPA+Accept+NIH...FM.log+F.S.log+MCAT+Enroll,data=Ranking.Training)
###drop RDS.Score and check model diagnostics
fit=lm(Res.Score.log~GPA+Accept+NIH...FM.log+F.S.log+MCAT+Enroll,data=Ranking.Training)
vif(fit)
summary(fit)
###drop Accept and check model diagnostics
fit=lm(Res.Score.log~Accept+MCAT+GPA+NIH...FM.log+F.S.log+Enroll,data=Ranking.Training)
vif(fit)
summary(fit)

plot(rstandard(fit),type='o')
abline(h=c(0,2,-2))
acf(residuals(fit),20,ci.type="ma")
pacf(residuals(fit),20,ci.type="white")
acf(residuals(fit)**2)
pacf(residuals(fit)**2)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))

###model validation using one-leave-out method
combdata=rbind(Ranking.Training,Ranking.Validation)


Forecast=c()
lower<-c()
upper<-c()
for ( i in (52:length(combdata$RDS.Score)))
{ 
  fit=lm(Res.Score.log~MCAT+Accept+NIH...FM.log+F.S.log+as.vector(Enroll)+GPA, data=combdata[1:(i-1),])
  new.data = data.frame(MCAT=combdata[i,]$MCAT,Accept=combdata[i,]$Accept,NIH...FM.log=combdata[i,]$NIH...FM.log
                      ,F.S.log=combdata[i,]$F.S.log, Enroll=combdata[i,]$Enroll,
                      GPA=combdata[i,]$GPA )
  
  f=predict(fit,newdata = new.data,h=1,interval='predict')
  lower[i]<-exp(f[,2])
  upper[i]<-exp(f[,3])
  Forecast[i]<-exp(f[,1])
}
f.actual<-combdata[52:64,]
d.vbf<-data.frame( Forecast_POINT=as.numeric(round(Forecast[52:64],2))
                  ,LOWER_BOUND=as.numeric(round(lower[52:64],2))
                  ,UPPER_BOUND=as.numeric(round(upper[52:64],2))
                  ,Actual_POINT=exp(f.actual$Res.Score.log)
                  ,Diff=exp(f.actual$Res.Score.log)-as.numeric(round(Forecast[52:64],2)))
###RMSE


                 

  
