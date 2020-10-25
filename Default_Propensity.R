# Libraries ----

library(pacman) #Package Management
p_load(readxl,install=TRUE,
       update=getOption("par_update")) # reading excel files
p_load(DataExplorer,install=TRUE,
       update=getOption("par_update")) # exploratory data analysis
p_load(packHV,install=TRUE,
       update=getOption("par_update")) # Univariate Visualization
p_load(corrplot,install=TRUE,
       update=getOption("par_update")) # Correlation Matrix
p_load(caret,install=TRUE,
       update=getOption("par_update")) # Confusion Matrix
p_load(car,install=TRUE,
       update=getOption("par_update")) # VIF
p_load(caTools,install=TRUE,
       update=getOption("par_update")) # Confusion Matrix
p_load(DescTools,install=TRUE,
       update=getOption("par_update")) # Winsorize
p_load(ROSE,install=TRUE,
       update=getOption("par_update")) # Oversampling
p_load(mlbench,install=TRUE,
       update=getOption("par_update")) # Winsorize
p_load(rpart,install=TRUE,
       update=getOption("par_update")) # CART
p_load(rpart.plot,install=TRUE,
       update=getOption("par_update")) # CART TOOLS
p_load(DMwR,install=TRUE,
       update=getOption("par_update")) # CART TOOLS
p_load(pROC,install=TRUE,
       update=getOption("par_update")) # CART TOOLS
p_load(JOUSBoost,install=TRUE,
       update=getOption("par_update")) # AdaBoost
p_load(randomForest,install=TRUE,
       update=getOption("par_update")) # Random Forest functions
p_load(xgboost,install=TRUE,
       update=getOption("par_update")) # Boosting functions



# Setting Working Directory ----

getwd()
setwd("C:/DSBA_Course/Proper Learning/Module 7 [Capstone Project]/")
Insurance.DataSet <- read_excel("Insurance Premium Default-Dataset.xlsx")
plot_intro(Insurance.DataSet,
           ggtheme = theme_classic(),
           title = "Insurance Data Overview")

str(Insurance.DataSet)

Insurance.DataSet$default <- ifelse(Insurance.DataSet$default==0,1,0)

table(Insurance.DataSet$default)


# Preprocess ----
str(Insurance.DataSet)
names(Insurance.DataSet)<-c("Id","PCash","AgeDays","Income","LateQtr","LateSem",
                            "LateAnn","Marital","VehOwned","Deps","Acc","risk",
                            "NoPremiums","Channel","Urban","PremiumAmount","Default");

table(Insurance.DataSet$Default)

Ins_data_Numeric <- Insurance.DataSet
table(Ins_data_Numeric$Default)
getwd()


#Data Report
dim(Insurance.DataSet)
plot_intro(Insurance.DataSet,
           ggtheme = theme_classic(),
           title = "Insurance Data Overview")
Insurance.DataSet$`Marital Status`<-as.factor(Insurance.DataSet$`Marital Status`)
Insurance.DataSet$residence_area_type<-as.factor(Insurance.DataSet$residence_area_type)
Insurance.DataSet$sourcing_channel<-as.factor(Insurance.DataSet$sourcing_channel)

Insurance.DataSet$Default<-as.factor(Insurance.DataSet$Default)
Insurance.DataSet$Accomodation<-as.factor(Insurance.DataSet$Accomodation)

table(Insurance.DataSet$Default)

str(Insurance.DataSet)

names(Insurance.DataSet)=c("ID",
                           "PER_CASH",
                           "AGE",
                           "INCOME",
                           "L3TO6",
                           "L6TO12",
                           "LMORE12",
                           "MARITAL",
                           "VEH_OWNED",
                           "DEPENDANTS",
                           "ACCOM",
                           "RISK_SCORE",
                           "NO_PREMIUMS",
                           "CHANNEL",
                           "AREA_TYPE",
                           "PREMIUM",
                           "DEFAULT")

table(Insurance.DataSet$DEFAULT)

names(Ins_data_Numeric)=c("ID",
                          "PER_CASH",
                          "AGE",
                          "INCOME",
                          "L3TO6",
                          "L6TO12",
                          "LMORE12",
                          "MARITAL",
                          "VEH_OWNED",
                          "DEPENDANTS",
                          "ACCOM",
                          "RISK_SCORE",
                          "NO_PREMIUMS",
                          "CHANNEL",
                          "AREA_TYPE",
                          "PREMIUM",
                          "DEFAULT")
str(Insurance.DataSet)
str(Ins_data_Numeric)

table(Insurance.DataSet$DEFAULT)
table(Ins_data_Numeric$DEFAULT)

a<-Ins_data_Numeric$CHANNEL
Ins_data_Numeric$CHANNEL=ifelse(a=="A",1,
                                ifelse(a=="B",2,
                                       ifelse(a=="C",3,
                                              ifelse(a=="D",4,5))))
Ins_data_Numeric$AREA_TYPE=ifelse(Ins_data_Numeric$AREA_TYPE=="Urban",
                                  1,0)
Ins_data_Numeric$CHANNEL=as.numeric(Ins_data_Numeric$CHANNEL)
Ins_data_Numeric$AREA_TYPE=as.numeric(Ins_data_Numeric$AREA_TYPE)

par(mfrow=c(1,1),mai = c(1.0, 1.0,1.0, 1.0),bg="white")

# Create the function.
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Univariate ----
attach(Insurance.DataSet)

#PER_CASH
a=PER_CASH
hist(a,col="blue",labels = T,border="white",breaks = 10,
     ylim = c(0,40000))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,5))
polygon(d, col="blue", border="blue")
abline(v=0.019,col="white",lty=3,lwd=2)
abline(h=3.8,col="red",lty=3,lwd=2)
points(x=0.019,y=3.8,pch=19,col="red",cex=1.5)
text(x=0.019,y=3.8,"Credit Payers",pos=3,cex = 1.5)
abline(v=0.99,col="white",lty=3,lwd=2)
abline(h=1.1,col="orange",lty=3,lwd=2)
points(x=0.99,y=1.1,pch=19,col="orange",cex=1.5)
text(x=0.99,y=1.1,"Cash Payers",pos=3,cex = 1.5)
bplt<-boxplot(a,horizontal = T,col="blue",
              notch = T,frame.plot=F)
text(fivenum(a), labels =fivenum(a), y=c(1.3,0.7,1.3,0.7,1.3), 
     col = "blue", cex = 0.9)

summary(PER_CASH)
sd(PER_CASH)
var(PER_CASH)
getmode(PER_CASH)

#AGE
a=AGE/365
hist(a,col="blue",labels = T,border="white",breaks = 10,
     ylim = c(0,25000))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,0.035))
polygon(d, col="blue", border="blue")
abline(v=48,col="white",lty=3,lwd=2)
abline(h=0.027,col="red",lty=3,lwd=2)
points(x=48,y=0.027,pch=19,col="red",cex=1.5)
text(x=48,y=0.027,"Working Customers",pos=3,cex = 1.5)
abline(v=61,col="white",lty=3,lwd=2)
abline(h=0.023,col="orange",lty=3,lwd=2)
points(x=61,y=0.023,pch=19,col="orange",cex=1.5)
text(x=61,y=0.023," Retired Customers",pos=4,cex = 1.5)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

summary(a)
sd(a)
var(a)

#INCOME
a=log10(INCOME)
hist(a,col="blue",labels = F,border="white",breaks =32,
     ylim = c(0,13000),xlim=c(4,8))
points(x=5.262,y=11446,pch=19,col="pink",cex=2)
abline(h=11446,col="pink",lty=3,lwd=2)
text(x=5.3,y=11446,paste0("Income \n[",round(10^5.3,2),"]"),
     pos=3,cex = 0.7)
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,1.6),xlim=c(4,8))
polygon(d, col="blue", border="blue")
abline(v=5.26,col="white",lty=3,lwd=2)
points(x=5.26,y=1.5,pch=19,col="red",cex=1)
text(x=5.26,y=1.5,paste0("Top Peak = ",round(10^5.26,2)),
     pos=3,cex = 1)
abline(v=4.6,col="white",lty=3,lwd=2)
points(x=4.6,y=0.15,pch=19,col="orange",cex=1)
text(x=4.6,y=0.18,paste0("Elbow \n[",round(10^4.6,2),"]"),
     pos=2,cex = 0.7)
abline(v=5.82,col="white",lty=3,lwd=2)
points(x=5.82,y=0.11,pch=19,col="green",cex=1)
text(x=5.82,y=0.13,paste0("Elbow \n[",round(10^5.82,2),"]"),
     pos=4,cex = 0.7)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

summary(a)
sd(a)
var(a)

#VEH_OWNED
a=VEH_OWNED
hist(a,col="blue",labels = T,border="white",ylim = c(0,30000))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,2))
polygon(d, col="blue", border="blue")
abline(v=1,col="white",lty=3,lwd=2)
points(x=1,y=1.75,pch=19,col="red",cex=1)
abline(v=2,col="white",lty=3,lwd=2)
points(x=2,y=1.75,pch=19,col="red",cex=1)
abline(v=3,col="white",lty=3,lwd=2)
points(x=3,y=1.75,pch=19,col="red",cex=1)


bplt<-boxplot(a,horizontal = T,col="blue",notch = T,frame.plot=F,
              outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

summary(a)
sd(a)
var(a)


#3TO6_LATE
a=L3TO6
hist(a,col="blue",labels = T,border="white",ylim = c(0,80000))
summary(a)
sd(a)
var(a)

d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,5))
polygon(d, col="blue", border="blue")
abline(v=0,col="white",lty=3,lwd=2)
abline(v=1,col="white",lty=3,lwd=2)
abline(v=2,col="white",lty=3,lwd=2)
points(x=0,y=4.8,pch=19,col="red",cex=1)
points(x=1,y=0.7,pch=19,col="red",cex=1)
points(x=2,y=0.3,pch=19,col="red",cex=1)
text(x=5.26,y=1.5,paste0("Top Peak = ",round(10^5.26,2)),
     pos=3,cex = 1)

bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

#6TO12_LATE
a=L6TO12
dim(Insurance.DataSet)
hist(a,col="blue",labels = T,border="white",ylim = c(0,85000))
summary(a)
sd(a)
var(a)

78608/79853
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,8))
polygon(d, col="blue", border="blue")
abline(v=0,col="white",lty=3,lwd=2)
abline(v=1,col="white",lty=3,lwd=2)
abline(v=2,col="white",lty=3,lwd=2)
points(x=0,y=7.8,pch=19,col="red",cex=1)
points(x=1,y=0.3,pch=19,col="red",cex=1)
points(x=2,y=0.3,pch=19,col="red",cex=1)





bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)


#MORE12_LATE
a=LMORE12
dim(Insurance.DataSet)
hist(a,col="blue",labels = T,border="white",ylim = c(0,85000))
summary(a)
sd(a)
var(a)

76135/79853
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,12))
polygon(d, col="blue", border="blue")
abline(v=0,col="white",lty=3,lwd=2)
abline(v=1,col="white",lty=3,lwd=2)
abline(v=2,col="white",lty=3,lwd=2)
points(x=0,y=12,pch=19,col="red",cex=1)
points(x=1,y=0.5,pch=19,col="red",cex=1)
points(x=2,y=0.3,pch=19,col="red",cex=1)




bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

#RISK_SCORE
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5),bg="white")
a=RISK_SCORE
summary(a)
sd(a)
var(a)

hist(a,col="blue",labels = T,border="white",ylim = c(0,55000),
     breaks = 10,xlim = c(91,100))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,1.1))
polygon(d, col="blue", border="blue")
abline(v=99.1,col="white",lty=3,lwd=2)
abline(h=0.96,col="red",lty=3,lwd=2)
points(x=99.1,y=0.96,pch=19,col="red",cex=1)
text(x=99.1,y=0.96,paste0("Top Peak \n[99.1,0.96]"),
     pos=3,cex = 1)
abline(v=99.86,col="white",lty=3,lwd=2)
abline(h=0.7,col="orange",lty=3,lwd=2)
points(x=99.86,y=0.7,pch=19,col="orange",cex=1)
text(x=99.86,y=0.7,paste0("2nd Peak \n[99.8,0.7]"),
     pos=3,cex = 1)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)


#NO_PREMIUMS
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5),bg="white")
a=NO_PREMIUMS
summary(a)
sd(a)
var(a)

hist(a,col="blue",labels = T,border="white",
     ylim = c(0,40000),breaks = 10,xlim = c(0,60))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,0.1))
polygon(d, col="blue", border="blue")
abline(v=9,col="white",lty=3,lwd=2)
abline(h=0.091,col="red",lty=3,lwd=2)
points(x=9,y=0.091,pch=19,col="red",cex=1)
text(x=9,y=0.091,paste0(" Top Peak \n [9,0.091]"),
     pos=4,cex = 1)
abline(v=8,col="white",lty=3,lwd=2)
abline(h=0.091,col="orange",lty=3,lwd=2)
points(x=8,y=0.091,pch=19,col="orange",cex=1)
text(x=8,y=0.091,paste0("2nd Peak \n[8,0.091]"),
     pos=2,cex = 1)
bplt<-boxplot(a,horizontal = T,col="blue",
              notch = T,frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)
33473/79853

#PREMIUM
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5),bg="white")
a=PREMIUM
summary(a)
sd(a)
var(a)

hist(a,col="blue",labels = T,border="white",ylim = c(0,40000),
     breaks = 10,xlim = c(0,60000))
d<-density(a)
plot(d,col="blue",frame.plot=F,xlim=c(0,65000),
     ylim=c(0,0.00015))
polygon(d, col="blue", border="blue")
abline(v=5500,col="white",lty=3,lwd=2)
abline(h=0.000115,col="red",lty=3,lwd=2)
points(x=5500,y=0.000115,pch=19,col="red",cex=1)
text(x=5500,y=0.000115,paste0(" Highest Count at \n [5,500]"),
     pos=4,cex = 1)
abline(v=60000,col="white",lty=3,lwd=2)
abline(h=0.000003,col="orange",lty=3,lwd=2)
points(x=60000,y=0.000003,pch=19,col="orange",cex=1)
text(x=60000,y=0.000003,paste0("Oldest Customers \n[60,000]"),
     pos=3,cex = 1)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)
33473/79853


#DEPENDANTS
par(mfrow=c(1,1),mai = c(0.7, 0.7, 0.7, 0.7),bg="white")
a=DEPENDANTS
summary(a)
sd(a)
var(a)

hist(a,col="blue",labels = T,border="white",ylim=c(0,25000))
d<-density(a)
plot(d,col="blue",frame.plot=F)
polygon(d, col="blue", border="blue")
abline(v=1,col="white",lty=3,lwd=2)
abline(v=2,col="white",lty=3,lwd=2)
abline(v=3,col="white",lty=3,lwd=2)
abline(v=4,col="white",lty=3,lwd=2)

points(x=1,y=1.45,pch=19,col="red",cex=1)
points(x=2,y=1.45,pch=19,col="red",cex=1)
points(x=3,y=1.45,pch=19,col="red",cex=1)
points(x=4,y=1.45,pch=19,col="red",cex=1)


text(x=5500,y=0.000115,paste0(" Highest Count at \n [5,500]"),
     pos=4,cex = 1)
abline(v=60000,col="white",lty=3,lwd=2)
abline(h=0.000003,col="orange",lty=3,lwd=2)
points(x=60000,y=0.000003,pch=19,col="orange",cex=1)
text(x=60000,y=0.000003,paste0("Oldest Customers \n[60,000]"),
     pos=3,cex = 1)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,
              frame.plot=F,outcol="red",outpch=19)
text(fivenum(a), labels =round(fivenum(a),2), 
     y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)
33473/79853

#MARITAL
par(mfrow=c(1,1),mai = c(0.7, 0.7, 0.7, 0.7),bg="white")
a<-MARITAL
t<-table(a)
plt<-plot(factor(a),col="blue",frame.plot=F,
          names=c("Unmarried","Married"),ylim=c(0,50000))
text(plt,45000,t)

#ACCOM
par(mfrow=c(1,1),mai = c(0.7, 0.7, 0.7, 0.7),bg="white")
a<-ACCOM
t<-table(a)
plt<-plot(factor(a),col="blue",frame.plot=F,
          names=c("Rented","Owned"),ylim=c(0,50000))
text(plt,t,t,pos=3)


#AREA_TYPE
par(mfrow=c(1,1),mai = c(0.7, 0.7, 0.7, 0.7),bg="white")
a<-AREA_TYPE
t<-table(a)
plt<-plot(factor(a),col="blue",frame.plot=F,ylim=c(0,60000))
text(plt,t,t,pos=3)

#CHANNEL
par(mfrow=c(1,1),mai = c(0.7, 0.7, 0.7, 0.7),bg="white")
a<-CHANNEL
t<-table(a)
plt<-plot(factor(a),col="blue",frame.plot=F,ylim=c(0,60000))
text(plt,t,t,pos=3)

#DEFAULT
par(mfrow=c(1,1),mai = c(1.2, 1.2, 1.2, 1.2),bg="white")
a<-DEFAULT
table(DEFAULT)
t<-table(a)
plt<-plot(factor(a),col="blue",frame.plot=F,ylim=c(0,85000),names=c("No_Default","Defaulted"))
text(plt,t,t,pos=3)

4998/74855

#Correlation Analysis ----
corrplot(cor(Ins_data_Numeric[-c(1,8,9,10,11,15)]),diag = T)
corrplot(cor(Ins_data_Numeric[-c(1)]),diag = T,method = "shade",order = "hclust",addrect = 3)

corrplot(cor(Ins_data_Numeric[-c(1)]), type = "upper", order = "hclust",
         col = c("black", "white"), bg = "lightblue")

corrplot(cor(Ins_data_Numeric[-c(1)]), order = "hclust", addrect = 7, col = c("black", "white"), bg = "gold2")


#Regression Analysis ----
Ins_data_Numeric<-Ins_data_Numeric[-1]
str(Ins_data_Numeric)
table(Ins_data_Numeric$DEFAULT)

logit <- glm(DEFAULT~.,
             Ins_data_Numeric,
             family = binomial())

summary(logit)

pIns_data <- predict.glm(logit,
                         type = "response")
Ins_data_Numeric$Prediction<-pIns_data
Ins_data_Numeric$OutcomePrediction<-ifelse(Ins_data_Numeric$Prediction>0.5,
                                           1,0)
confusionMatrix(table(Ins_data_Numeric$OutcomePrediction,
                      Ins_data_Numeric$DEFAULT),positive = '1')


detach(Insurance.DataSet)

#Bivariate Analysis ----

#PER_CASH vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$PER_CASH
varBins<-cut(a,breaks = 10)
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)

brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,2000),beside = T,
               width=2,space = c(0,0.5),horiz = T,las=1,
               main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 2000, 200), col = "white", lty = 1,lwd=2)
legend(x=23000,y=50,legend=c("No-Default","Default"),
       bty="n",fill=c("blue","red"),border=F)
tt<-t
tt
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,
     pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,
              names=c("No-Default","Default"),las=1,ylab="",
              xlab="% Premium by Cash",frame.plot=F)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#AGE vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$AGE/365
varBins<-cut(a,breaks = 10)
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,1500),
               beside = T,width=2,space = c(0,0.5),horiz = T,
               las=1,main="Perc. Premium by Cash Vs. Default",
               border=F,cex.axis = 0.75)
abline(v = seq(0, 1500, 100), col = "white", lty = 1,lwd=2)
legend(x=15000,y=50,legend=c("No-Default","Default"),
       bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,
     pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,
              names=c("No-Default","Default"),las=1,ylab="",
              xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))


#INCOME vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=log10(Insurance.DataSet$INCOME)
varBins<-cut(a,breaks = 10)
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,2500),
               beside = T,width=2,space = c(0,0.5),horiz = T,
               las=1,main="Perc. Premium by Cash Vs. Default",
               border=F,cex.axis = 0.75)
abline(v = seq(0, 2500, 100), col = "white", lty = 1,lwd=2)
legend(x=25000,y=50,legend=c("No-Default","Default"),bty="n",
       fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,
              names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",
              frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#3TO6_LATE vs DEFAULT
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$L3TO6
varBins<-cut(a,breaks = c(-0.5,2.6,5.2,10.4,13))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 5000, 200), col = "white", lty = 1,lwd=2)
legend(x=48000,y=25,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#6TO12_LATE vs DEFAULT
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$L6TO12
varBins<-cut(a,breaks = c(-0.5,2.6,5.2,10.4,13))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 5000, 200), col = "white", lty = 1,lwd=2)
legend(x=48000,y=25,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#MORE12_LATE vs DEFAULT
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$LMORE12
varBins<-cut(a,breaks = c(-0.5,2.6,5.2,10.4,13))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,6000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 6000, 300), col = "white", lty = 1,lwd=2)
legend(x=48000,y=25,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))


#RISK_SCORE vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$RISK_SCORE
varBins<-cut(a,breaks = c(91,93,98,100))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 5000, 100), col = "white", lty = 1,lwd=2)
legend(x=56000,y=4,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#NO_PREMIUMS vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$NO_PREMIUMS
varBins<-cut(a,breaks = c(0,20,40,50,60))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 5000, 100), col = "white", lty = 1,lwd=2)
legend(x=30000,y=30,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))


#PREMIUM vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$PREMIUM
varBins<-cut(a,breaks = c(1100,20000,44000,48000,61000))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 5000, 100), col = "white", lty = 1,lwd=2)
legend(x=45000,y=19,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.5,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(3.0, 2.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))

#CHANNEL vs DEFAULT
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$CHANNEL
b=Insurance.DataSet$DEFAULT
t<-table(b,a)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,3000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 3000, 200), col = "white", lty = 1,lwd=2)
legend(x=35000,y=25,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=0.75,pos=4,col=c("black")) 
cor(as.numeric(a),as.numeric(b))

#VEH_OWNED vs DEFAULT
par(mfrow=c(5,1),mai = c(0.5, 1.0, 0.5, 0.5),bg="white")
a=Insurance.DataSet$VEH_OWNED
varBins<-cut(a,breaks = 3)
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,2000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Veh Owned Vs. Default",border=F,cex.axis = 0.75)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 

#DEPENDANTS vs DEFAULT
a=Insurance.DataSet$DEPENDANTS
varBins<-cut(a,breaks = 4)
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,2000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Dependants Vs. Default",border=F,cex.axis = 0.75)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 


#MARITAL vs DEFAULT
a=Insurance.DataSet$MARITAL
b=Insurance.DataSet$DEFAULT
t<-table(b,a)
brplt<-barplot(t[2,],names=c("Unmarried","Married"),col=c("blue","red"),xlim = c(0,4000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Marital Vs. Default",border=F,cex.axis = 0.75)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 

#ACCOM vs DEFAULT
a=Insurance.DataSet$ACCOM
b=Insurance.DataSet$DEFAULT
t<-table(b,a)
brplt<-barplot(t[2,],names=c("Rented","Owned"),col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="ACCOM Vs. Default",border=F,cex.axis = 0.75)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 



#AREA_TYPE vs DEFAULT
a=Insurance.DataSet$AREA_TYPE
b=Insurance.DataSet$DEFAULT
t<-table(b,a)
brplt<-barplot(t[2,],col=c("blue","red"),xlim = c(0,5000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Area Type Vs. Default",border=F,cex.axis = 0.75)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t[2,],brplt , paste("",tt[2,],"%", sep="") ,cex=1.25,pos=4,col=c("black")) 


# Multivariate Analysis ----

#PREMIUM Vs INCOME vs DEFAULT
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$PREMIUM
b=Insurance.DataSet$INCOME
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,col=ifelse(c=="1","green","blue"),pch=19,xlab = "PREMIUM",ylab = "INCOME",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
abline(lm(b~a),col="red",lwd=2,lty=1)
legend(horiz=T,x=20000,y=95000000,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#AGE vs PER_CASH
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$PER_CASH
b=Insurance.DataSet$AGE
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(7670,50000),col=ifelse(c=="1","green","blue"),pch=19,xlab = "PER_CASH",ylab = "AGE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
abline(lm(b~a),col="red",lwd=2,lty=1)
legend(horiz=T,x=0.3,y=50000,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#PREMIUM vs NO_PREMIUMS
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$PREMIUM
b=Insurance.DataSet$NO_PREMIUMS
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,80),col=ifelse(c=="1","green","blue"),pch=19,xlab = "PREMIUM",ylab = "NO_PREMIUMS",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
abline(lm(b~a),col="red",lwd=2,lty=1)
legend(horiz=T,x=25000,y=80,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#RISK_SCORE vs NO_PREMIUMS
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$RISK_SCORE
b=Insurance.DataSet$NO_PREMIUMS
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,80),col=ifelse(c=="1","green","blue"),pch=19,xlab = "RISK_SCORE",ylab = "NO_PREMIUMS",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=94,y=80,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)

#PER_CASH vs 3TO6_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$L3TO6
b=Insurance.DataSet$PER_CASH
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,1.5),col=ifelse(c=="1","green","blue"),pch=19,ylab = "PER_CASH",xlab = "3TO6_LATE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=1.5,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#PER_CASH vs 6TO12_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$L6TO12
b=Insurance.DataSet$PER_CASH
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,1.5),col=ifelse(c=="1","green","blue"),pch=19,ylab = "PER_CASH",xlab = "6TO12_LATE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=1.5,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)

#PER_CASH vs MORE12_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$LMORE12
b=Insurance.DataSet$PER_CASH
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,1.5),col=ifelse(c=="1","green","blue"),pch=19,ylab = "PER_CASH",xlab = "MORE12_LATE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=1.5,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#3TO6_LATE vs 6TO12_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$L3TO6
b=Insurance.DataSet$L6TO12
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,15),col=ifelse(c=="1","green","blue"),pch=19,xlab = "3TO6_LATE",ylab = "6TO12_LATE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=15,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#6TO12_LATE vs MORE12_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$LMORE12
b=Insurance.DataSet$L6TO12
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,15),col=ifelse(c=="1","green","blue"),pch=19,xlab = "MORE12_LATE",ylab = "6TO12_LATE",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=15,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)


#3TO6_LATE vs MORE12_LATE
par(mfrow=c(1,1),mai = c(1.0, 1.0, 1.0, 1.0),bg="white")
a=Insurance.DataSet$LMORE12_LATE
b=Insurance.DataSet$L3TO6
c=Insurance.DataSet$DEFAULT
plt<-plot(a,b,ylim=c(0,15),col=ifelse(c=="1","green","blue"),pch=19,xlab = ">12 Months Late",ylab = "3-6 Months Late",main=paste0("Pearson's Score = ",round(cor(a,b)*100,2),"%"),frame.plot = F)
legend(horiz=T,x=4,y=15,legend=c("No-Default","Default"),bty="n",fill=c("blue","green"),border=F)

#Variable Transformation ----
Insurance.DataSet$INCOME<-log10(Insurance.DataSet$INCOME)
summary(Insurance.DataSet$INCOME)
Insurance.DataSet$AGE<-(Insurance.DataSet$AGE/365)
summary(Insurance.DataSet$AGE)
Insurance.DataSet$RISK_SCORE<-(Insurance.DataSet$RISK_SCORE)/100
summary(Insurance.DataSet$RISK_SCORE)


#Variable Addition ----
Insurance.DataSet$PREMIUM_RATE<-Insurance.DataSet$PREMIUM/Insurance.DataSet$NO_PREMIUMS
Insurance.DataSet$PREMIUM_RATE<-Insurance.DataSet$PREMIUM/Insurance.DataSet$NO_PREMIUMS
summary(Insurance.DataSet$PREMIUM_RATE)

#PREMIUM_RATE

#Univariate
par(mfrow=c(3,1),mai = c(0.3, 0.3, 0.3, 0.3),bg="white")
a=Insurance.DataSet$PREMIUM_RATE
hist(a,col="blue",labels = T,border="white",breaks = 10,ylim = c(0,75000),main = NULL,xlim = c(0,10000))
d<-density(a)
plot(d,col="blue",frame.plot=F,ylim=c(0,0.0008),main = NULL,xlim = c(0,10000))
polygon(d, col="blue", border="blue")
abline(v=600,col="white",lty=3,lwd=2)
abline(h=0.00067,col="red",lty=3,lwd=2)
points(x=600,y=0.00067,pch=19,col="red",cex=1.5)
text(x=1400,y=0.00067,"High Volume \nat Rate=500",pos=3,cex = 1)
bplt<-boxplot(a,horizontal = T,col="blue",notch = T,frame.plot=F,main =NULL)
text(fivenum(a), labels =fivenum(a), y=c(1.3,0.7,1.3,0.7,1.3), col = "blue", cex = 0.9)

#Bivariate
par(mfrow=c(1,1),mai = c(0.0, 2.0, 0.5, 1.0),bg="white")
a=Insurance.DataSet$PREMIUM_RATE
varBins<-cut(a,breaks = c(0,8000,16000,24000,30000))
b=Insurance.DataSet$DEFAULT
t<-table(b,varBins)
brplt<-barplot(t,col=c("blue","red"),xlim = c(0,70000),beside = T,width=2,space = c(0,0.5),horiz = T,las=1,main="Perc. Premium by Cash Vs. Default",border=F,cex.axis = 0.75)
abline(v = seq(0, 75000, 2000), col = "white", lty = 1,lwd=2)
legend(x=25000,y=19,legend=c("No-Default","Default"),bty="n",fill=c("blue","red"),border=F)
tt<-t
tt[1,]<-round(t[1,]/(t[2,]+t[1,]),2)*100
tt[2,]<-round(t[2,]/(t[2,]+t[1,]),2)*100
text(t,brplt , paste("",tt,"%", sep="") ,cex=0.75,pos=4,col=c("black")) 
par(mfrow=c(1,1),mai = c(1.0, 1.0, 0.5, 1.0),bg="white")
bplt<-boxplot(a~b,col=c("blue","red"),horizontal = T,names=c("No-Default","Default"),las=1,ylab="",xlab="AGE",frame.plot=F,outcol="red",outpch=19)
abline(lm(b~a),col="orange",lwd=3,lty=3)
bplt
cor(a,as.numeric(b))



# Feature Selection ----

dim(Insurance.DataSet)
names(Insurance.DataSet)
Insurance.DataSet<-Insurance.DataSet[-1] #Removing ID
Insurance.DataSet<-Insurance.DataSet[-7] #Removing MARITAL
Insurance.DataSet<-Insurance.DataSet[-7] #Removing VEH_OWNED
Insurance.DataSet<-Insurance.DataSet[-7] #Removing DEPENDANTS
Insurance.DataSet<-Insurance.DataSet[-7] #Removing ACCOM
Insurance.DataSet<-Insurance.DataSet[-10] #Removing AREA_TYPE
dim(Insurance.DataSet)
names(Insurance.DataSet)

# Data Partitioning ----
set.seed(6)
split <- sample.split(Insurance.DataSet$DEFAULT, SplitRatio = 0.7)
trainSample<- subset(Insurance.DataSet, split == TRUE)
testSample<- subset(Insurance.DataSet, split == FALSE)

dim(trainSample)
table(trainSample$DEFAULT)
dim(testSample)
table(testSample$DEFAULT)

#Outlier Treatment ----
minvalue<-quantile(trainSample$INCOME,probs = 0.25)-1.5*IQR(trainSample$INCOME)
maxvalue<-quantile(trainSample$INCOME,probs = 0.75)+1.5*IQR(trainSample$INCOME)
trainSample$INCOME<-Winsorize(trainSample$INCOME,minval = minvalue ,maxval = maxvalue)

minvalue<-quantile(trainSample$AGE,probs = 0.25)-1.5*IQR(trainSample$AGE)
maxvalue<-quantile(trainSample$AGE,probs = 0.75)+1.5*IQR(trainSample$AGE)
trainSample$AGE<-Winsorize(trainSample$AGE,minval = minvalue ,maxval = maxvalue)

summary(trainSample$INCOME)
summary(trainSample$AGE)

temp<-sqrt(Insurance.DataSet$RISK_SCORE)
boxplot(temp~Insurance.DataSet$DEFAULT,horizontal = T)


# Model 1 ---- LOGISTIC

# Oversampling ----

OverSample.Insurance<-NULL
names(trainSample)

tbl<-table(trainSample$DEFAULT)
if(tbl[1]<tbl[2]) {
        
        trainSample$DEFAULT<-ifelse(trainSample$DEFAULT=="0",1,0)
}

tbl<-table(testSample$DEFAULT)
if(tbl[1]<tbl[2]) {
        
        testSample$DEFAULT<-ifelse(testSample$DEFAULT=="0",1,0)
}


52398*2
OverSample.Insurance <- ovun.sample(DEFAULT~.,data = trainSample, method = "over", N = 104796)$data
table(OverSample.Insurance$DEFAULT)

str(OverSample.Insurance)

tempp<-ifelse(OverSample.Insurance$CHANNEL=="A",1,
              ifelse(OverSample.Insurance$CHANNEL=="B",2,
                     ifelse(OverSample.Insurance$CHANNEL=="C",3,
                            ifelse(OverSample.Insurance$CHANNEL=="D",4,5)
                     )
              )
)

tempp1<-ifelse(testSample$CHANNEL=="A",1,
               ifelse(testSample$CHANNEL=="B",2,
                      ifelse(testSample$CHANNEL=="C",3,
                             ifelse(testSample$CHANNEL=="D",4,5)
                      )
               )
)

plot(OverSample.Insurance$RISK_SCORE,tempp)

names(OverSample.Insurance)
str(OverSample.Insurance)
Logit.DataSet1<-OverSample.Insurance
Logit.DataSet1$DEFAULT<-as.factor(Logit.DataSet1$DEFAULT)
Logit.DataSet1$CHANNEL<-tempp
Logit.DataSet1$CHANNEL<-as.numeric(Logit.DataSet1$CHANNEL)
Logit.DataSet1$INCOME<-log(Logit.DataSet1$INCOME)
Logit.DataSet1$CummLate<-(OverSample.Insurance$PER_CASH*OverSample.Insurance$L3TO6*OverSample.Insurance$L6TO12*OverSample.Insurance$LMORE12)
Logit.DataSet1$ChannelBehavior<-(OverSample.Insurance$PER_CASH)^tempp

names(testSample)
Logit.Test1<-testSample
Logit.Test1$DEFAULT<-as.factor(testSample$DEFAULT)
Logit.Test1$INCOME<-log(testSample$INCOME)
Logit.Test1$CHANNEL<-tempp1
Logit.Test1$CHANNEL<-as.numeric(Logit.Test1$CHANNEL)
Logit.Test1$CummLate<-(testSample$PER_CASH*testSample$L3TO6*testSample$L6TO12*testSample$LMORE12)
Logit.Test1$ChannelBehavior<-(testSample$PER_CASH)^tempp1

table(testSample$DEFAULT)

set.seed(6)

#Create Folds with same proportion of DEFAULT
folds1 <- createFolds(factor(Logit.DataSet1$DEFAULT), k = 2, list = FALSE)

#Define Hyperparameters
HyperLogit1 <- trainControl(method="repeatedcv", #Cross Validation
                            number=2, #K Folds   
                            savePredictions = T) 


summary(Logit.DataSet1)

#Fit Model
Logit.Fit <- train(DEFAULT~.,
                   data=Logit.DataSet1[,-10],
                   method="glm",
                   family="binomial",
                   trControl=HyperLogit1)

#Summary
summary(Logit.Fit)
confusionMatrix(predict(Logit.Fit, Logit.DataSet1, type = "raw"),Logit.DataSet1$DEFAULT,positive='1')

probsTrain <- predict(Logit.Fit, Logit.DataSet1, type = "prob")
rocCurve   <- roc(response = Logit.DataSet1$DEFAULT,
                  predictor = probsTrain[, "1"],
                  levels = rev(levels(Logit.DataSet1$DEFAULT)))
plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.745,col="red")
abline(h=0.768,col="red")

rocCurve$auc

table(ifelse(predict(Logit.Fit, Logit.Test1, type = "prob")[2]>0.478,1,0))
tbl<-table(ifelse(predict(Logit.Fit, Logit.Test1, type = "prob")[2]>0.478,1,0),Logit.Test1$DEFAULT)

confusionMatrix(tbl,positive='1')
probsTrain <- predict(Logit.Fit, Logit.Test1, type = "prob")
rocCurve   <- roc(response = Logit.Test1$DEFAULT,
                  predictor = probsTrain[, "1"],
                  levels = rev(levels(Logit.Test1$DEFAULT)))
plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.717,col="red")
abline(h=0.809,col="red")
rocCurve$auc

1130/(1130+369)


# Model 2 ---- RANDOM FOREST

str(OverSample.Insurance)
mydata <- OverSample.Insurance[,-9]

str(mydata)

set.seed(6)
rndFor = randomForest(DEFAULT ~ ., data = mydata,
                      ntree=101, mtry = 4, nodesize = 100,
                      importance=TRUE,replace=T,do.trace=T)

plot(rndFor)
rndFor$importance
rndFor

confusionMatrix(factor(ifelse(predict(rndFor, mydata, type="class")>0.5,1,0)),factor(mydata$DEFAULT),positive = "1")

probsTrain <- predict(rndFor, mydata, type = "class")
rocCurve   <- roc(response = mydata$DEFAULT,
                  predictor = probsTrain)

plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.970,col="red")
abline(h=0.988,col="red")
rocCurve$auc


confusionMatrix(factor(ifelse(predict(rndFor, newdata = testSample, type="class")>0.5,1,0)),factor(testSample$DEFAULT),positive = "1")

probsTrain <- predict(rndFor, testSample, type = "class")
rocCurve   <- roc(response = testSample$DEFAULT,
                  predictor = probsTrain)

plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.754,col="red")
abline(h=0.755,col="red")
rocCurve$auc



# Model 3 ---- ADABOOST


names(OverSample.Insurance)
names(testSample)
str(OverSample.Insurance)
str(testSample)

OverSample.Insurance$CHANNEL<-as.numeric(OverSample.Insurance$CHANNEL)
testSample$CHANNEL<-as.numeric(testSample$CHANNEL)


ada_response_train <- ifelse(OverSample.Insurance$DEFAULT=="1",1,-1)
ada_predictors_train <-as.matrix(OverSample.Insurance[,-c(11)])
ada_predictors_test <-as.matrix(testSample[,-c(11)])

adaBoostTrain <- adaboost(ada_predictors_train,ada_response_train,tree_depth = 3,
                          n_rounds=200, verbose = T)

adaBoostTrain$confusion_matrix

confusionMatrix(adaBoostTrain$confusion_matrix)

probsTrain <- predict(adaBoostTrain, ada_predictors_train, type = "prob")
rocCurve   <- roc(response = ada_response_train, predictor = probsTrain)
plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.805,col="red")
abline(h=0.735,col="red")
rocCurve$auc

probsTrain <- predict(adaBoostTrain, ada_predictors_test, type = "prob")
confusionMatrix(factor(ifelse(probsTrain>0.5,1,0)),factor(testSample$DEFAULT),positive="1")
rocCurve   <- roc(response = testSample$DEFAULT, predictor = probsTrain)
plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.782,col="red")
abline(h=0.744,col="red")
rocCurve$auc



# Model 5 ---- XGBOOST

str(testSample)
xgBoost.testSample<-testSample
xgBoost.trainSample<-OverSample.Insurance

names(xgBoost.testSample)
names(xgBoost.trainSample)

str(xgBoost.testSample)
str(xgBoost.trainSample)

gd_features_train <- as.matrix(xgBoost.trainSample[,-c(11)])
gd_labels_train <- as.matrix(xgBoost.trainSample[,c(11)])
gd_features_test <- as.matrix(xgBoost.testSample[,-c(11)])

xgb.fit <- xgboost(
        data = gd_features_train,
        label=gd_labels_train,
        eta=0.01,
        max_depth=7,
        min_child_weight=27,
        nrounds = 67,
        nfold=2,
        objective="binary:logistic",
        verbose=0,
        early_stopping_rounds = 10
)

confusionMatrix(factor(ifelse(predict(xgb.fit,gd_features_train)>0.5,1,0)),factor(gd_labels_train),positive="1")
probsTrain <- predict(xgb.fit, gd_features_train, type = "prob")
rocCurve   <- roc(response = gd_labels_train, predictor = probsTrain)
plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.776,col="red")
abline(h=0.770,col="red")
rocCurve$auc


confusionMatrix(factor(ifelse(predict(xgb.fit,gd_features_test)>0.5,1,0)),factor(testSample$DEFAULT),positive="1")
probsTrain <- predict(xgb.fit, gd_features_test, type = "prob")
rocCurve   <- roc(response = testSample$Default, predictor = probsTrain)
plt<-plot(rocCurve, print.thres = "best",print.default=T)
abline(v=0.772,col="red")
abline(h=0.767,col="red")
rocCurve$auc

