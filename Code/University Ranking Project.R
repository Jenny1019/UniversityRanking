##load packages
library(psych)
library(dplyr)
library(VIF)
library("ggpubr")
library(TSA)
##Load the data
Ranking=read.csv("C:\\Users\\ein8\\Desktop\\courseware\\Paper\\UniversityRanking\\Data\\rawdata.csv") 


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


#Explanatory Data Analysis
Ranking.e=Ranking[c(1:20,31),]
area.color <- c(rep("withcolor",20),"NA")
 
ggplot(Ranking.e,aes(x=School,y=Res.Score)) + coord_cartesian(ylim=c(50,100))+
  geom_bar(aes(x=School,y=Res.Score,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="Res.Score")+guides(fill=FALSE)+geom_text(aes(label =Res.Score), color = "black", size = 4, vjust=1)+
  geom_hline(yintercept = mean(Ranking.e$Res.Score[1:20]),color="red",linetype="dashed")


ggplot(Ranking.e,aes(x=School,y=PAS.Score)) + coord_cartesian(ylim=c(3,5))+
  geom_bar(aes(x=School,y=PAS.Score,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="PAS.Score")+guides(fill=FALSE)+geom_text(aes(label =PAS.Score), color = "black", size = 4, vjust=1)+
geom_hline(yintercept = mean(Ranking.e$PAS.Score[1:20]),color="red",linetype="dashed")



ggplot(Ranking.e,aes(x=School,y=RDS.Score)) + coord_cartesian(ylim=c(3,5))+
  geom_bar(aes(x=School,y=RDS.Score,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="RDS.Score")+guides(fill=FALSE)+geom_text(aes(label =RDS.Score), color = "black", size = 4, vjust=1)+
geom_hline(yintercept = mean(Ranking.e$RDS.Score[1:20]),color="red",linetype="dashed")



ggplot(Ranking.e,aes(x=School,y=GPA)) + coord_cartesian(ylim=c(3.6,3.95))+
  geom_bar(aes(x=School,y=GPA,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="GPA")+guides(fill=FALSE)+geom_text(aes(label =GPA), color = "black", size = 4, vjust=1)+
geom_hline(yintercept = mean(Ranking.e$GPA[1:20]),color="red",linetype="dashed")

# area.color <- c(rep("withcolor",20),"NA")
# ggplot(Ranking.e,aes(x=School,y=MCAT)) +
#   geom_point(aes(x=School,y=MCAT,color=area.color,size=MCAT)) +
#  theme_classic()+labs(y="MCAT")+guides(fill=FALSE,color=FALSE)+geom_text(aes(label =MCAT), color = "black", size = 4, vjust=1)


ggplot(Ranking.e,aes(x=School,y=MCAT)) + coord_cartesian(ylim=c(500,525))+
  geom_bar(aes(x=School,y=MCAT,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="MCAT")+guides(fill=FALSE)+geom_text(aes(label =MCAT), color = "black", size = 4, vjust=1)+
geom_hline(yintercept = mean(Ranking.e$MCAT[1:20]),color="red",linetype="dashed")

Ranking.e$Accept1=Ranking.e$Accept*100
ggplot(Ranking.e,aes(x=School,y=Accept1)) + coord_cartesian(ylim=c(1,9))+
  geom_bar(aes(x=School,y=Accept1,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="Acceptance rate (%)")+guides(fill=FALSE)+geom_text(aes(label =Accept1), color = "black", size = 4, vjust=1) +
  geom_hline(yintercept = mean(Ranking.e$Accept1[1:20]),color="red",linetype="dashed")

ggplot(Ranking.e,aes(x=School,y=NIH...FM)) + coord_cartesian(ylim=c(50,400))+
  geom_bar(aes(x=School,y=NIH...FM,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="NIH per FM")+guides(fill=FALSE)+geom_text(aes(label =NIH...FM), color = "black", size = 4, vjust=1) +
  geom_hline(yintercept = mean(Ranking.e$NIH...FM[1:20]),color="red",linetype="dashed")

ggplot(Ranking.e,aes(x=School,y=F.S)) + coord_cartesian(ylim=c(1.5,13))+
  geom_bar(aes(x=School,y=F.S,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="F.S")+guides(fill=FALSE)+geom_text(aes(label =F.S), color = "black", size = 4, vjust=1) +
  geom_hline(yintercept = mean(Ranking.e$F.S[1:20]),color="red",linetype="dashed")

Ranking.e$Tuit.Fees1=round(Ranking.e$Tuit.Fees/1000,2)
ggplot(Ranking.e,aes(x=School,y=Tuit.Fees1)) + coord_cartesian(ylim=c(37,65))+
  geom_bar(aes(x=School,y=Tuit.Fees1,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="Tuition Fees (k)")+guides(fill=FALSE)+geom_text(aes(label =Tuit.Fees1), color = "black", size = 4, vjust=1) +
  geom_hline(yintercept = mean(Ranking.e$Tuit.Fees1[1:20]),color="red",linetype="dashed")

ggplot(Ranking.e,aes(x=School,y=Enroll)) + coord_cartesian(ylim=c(150,1100))+
  geom_bar(aes(x=School,y=Enroll,fill=area.color ),stat="identity",alpha=0.5) +
  theme_classic()+labs(y="Enrollment")+guides(fill=FALSE)+geom_text(aes(label =Enroll), color = "black", size = 4, vjust=1) +
  geom_hline(yintercept = mean(Ranking.e$Enroll[1:20]),color="red",linetype="dashed")

ggplot(Ranking.e[1:20,],aes(x=Public.Private,fill=Public.Private  )) + coord_cartesian(ylim=c(1,15))+
  geom_bar(alpha=0.5) +
  theme_classic()+labs(y="Number")+labs(x="University Type")+guides(fill=FALSE)


#Association Analysis
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
par(mfrow=c(2,2))
ggscatter(Ranking.vf, x = "PAS.Score", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PAS", ylab = "RES.log")
ggscatter(Ranking.vf, x = "RDS.Score", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RDS", ylab = "RES.log")

ggscatter(Ranking.vf, x = "GPA", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GPA", ylab = "RES.log")
ggscatter(Ranking.vf, x = "MCAT", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = " MCAT", ylab = "RES.log")
ggscatter(Ranking.vf, x = "NIH...FM.log", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NIH per FM.log", ylab = "RES.log")
ggscatter(Ranking.vf, x = "F.S.log", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "F.S.log", ylab = "RES.log")

ggscatter(Ranking.vf, x = "Accept", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Acceptance rate", ylab = "RES.log")

ggscatter(Ranking.vf, x = "Tuit.Fees", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tuition Fees", ylab = "RES.log")

ggscatter(Ranking.vf, x = "Enroll", y = "Res.Score.log", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Enrollment", ylab = "RES.log")

ggplot(Ranking.vf, aes(x = PP, y = Res.Score.log)) +
  geom_boxplot() + 
  xlab("University Type") + ylab("Res.Score.log")+theme_classic()

###shuffle the data and Split data into trai
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


fit=lm(Res.Score.log~GPA+PAS.Score+Accept+NIH...FM.log+F.S.log+MCAT+Enroll,data=Ranking.Training)
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


                 

  
