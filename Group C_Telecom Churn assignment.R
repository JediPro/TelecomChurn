#read in data and check summary
dc=read.table("cal_sorted.csv", header = T, sep = ",")
summary(dc)

#find correlations
cor(dc[,c(3:5)])
cor(dc[,c(6,7)])
cor(dc[,c(8,9,10)])
cor(dc[,c(15:17)])
cor(dc[,c(14:16)])
cor(dc[,c(17:20)])
cor(dc[,c(21:24)])
cor(dc[,c(25:28)])
cor(dc[,c(29,30)], use = "complete.obs")
cor(dc[,c(31:36)], use = "complete.obs")
cor(dc[,c(37,38)], use = "complete.obs")
cor(dc[,c(39,40)], use = "complete.obs")
cor(dc[,c(41,42)], use = "complete.obs")
cor(dc[,c(43:48)], use = "complete.obs")
cor(dc[,c(50,51)], use = "complete.obs")
cor(dc[,c(52:53)], use = "complete.obs")
cor(dc[,c(52:53)], use = "complete.obs")
cor(dc[,c(55:74)], use = "complete.obs")
library(corrgram)
corrgram(cor(dc[,c(55:74)], use = "complete.obs"))
cor(dc[,c(75:78)], use = "complete.obs")
cor(dc[,c(79:82)], use = "complete.obs")
cor(dc[,c(83:84)], use = "complete.obs")
cor(dc[,c(85:88)], use = "complete.obs")
cor(dc[,c(89:92)], use = "complete.obs")
cor(dc[,c(93:96)], use = "complete.obs")
cor(dc[,c(93:96)], use = "complete.obs")
cor(dc[,c(98:99)], use = "complete.obs")
cor(dc[,c(100:102)], use = "complete.obs")
cor(dc[,c(103:104)], use = "complete.obs")
cor(dc[,c(105:106)], use = "complete.obs")
cor(dc[,c(107,108,111)], use = "complete.obs")
cor(dc[,c(109:110)], use = "complete.obs")
cor(dc[,c(112:115)], use = "complete.obs")
cor(dc[,c(41,42,79:82,116,117)], use = "complete.obs")

cnames=c("ADJMOU","ATTEMPT_MEAN","AVG3MOU","AVGMOU","AVGMOU","BLCK_DAT_MEAN","BLCK_VCE_MEAN","CALLFWDV_MEAN","CALLWAIT_MEAN","CC_MOU_MEAN","CHANGE_MOU","COMP_DAT_MEAN","COMPLETE_MEAN","CUSTCARE_MEAN","DA_MEAN","DATOVR_MEAN","DROP_BLK_MEAN","DROP_DAT_MEAN","EQPDAYS","INONEMIN_MEAN","IWYLIS_VCE_MEAN","MOU_CDAT_MEAN","MOU_MEAN","OPK_DAT_MEAN","OPK_VCE_MEAN","OVRREV_MEAN","OWYLIS_VCE_MEAN","PEAK_DAT_MEAN","PEAK_VCE_MEAN","PLCD_DAT_MEAN","PLCD_VCE_MEAN","RECV_SMS_MEAN","RECV_VCE_MEAN","REV_MEAN","RMMOU","ROAM_MEAN","THREEWAY_MEAN","TOTMOU","TOTMRC_MEAN","TOTMRC_RANGE","UNAN_DAT_MEAN","UNAN_VCE_MEAN")
dcor=dc[,cnames]
source("functions.R")
crl=corrl(cor(dcor,use="complete.obs"))

#create new data frame of selected predictors
cnames1=c("AVGMOU","BLCK_DAT_MEAN","CALLWAIT_MEAN","CC_MOU_MEAN","CHANGE_MOU","COMP_DAT_MEAN","CUSTCARE_MEAN","DATOVR_MEAN","DROP_BLK_MEAN","DROP_DAT_MEAN","EQPDAYS","IWYLIS_VCE_MEAN","MOU_CDAT_MEAN","OPK_VCE_MEAN","OVRREV_MEAN","OWYLIS_VCE_MEAN","PLCD_VCE_MEAN","RECV_SMS_MEAN","RECV_VCE_MEAN","RETDAYS","RMMOU","ROAM_MEAN","TOTMRC_MEAN","TOTMRC_RANGE","UNAN_DAT_MEAN","UNAN_VCE_MEAN")
d1=dc[,cnames1]

#impute missing values in select predictors through logic
d1$RETDAYS=ifelse(is.na(d1$RETDAYS),0,ifelse(d1$RETDAYS<60,1,ifelse(d1$RETDAYS<120,2,ifelse(d1$RETDAYS<180,3,4))))
d1$RMMOU[is.na(d1$RMMOU)]=0
d1$ROAM_MEAN[is.na(d1$ROAM_MEAN)]=0

#remove negative values for EQPDAYS
d1$EQPDAYS[d1$EQPDAYS<0]=0
d1$EQPDAYS[is.na(d1$EQPDAYS)]=342


#check distributions of catgorical data
#separate out the categorical data
dcat=dc[,c(1,2,118:173)]


#plot histograms for each categorical data
library(lattice)
for (i in 3:58) {
  histogram(~dcat[,i]|CHURN,dcat,xlab = names(dcat[,i])) 
}


#create data frame of selected predictors
cnames2=c("CUSTOMER_ID","CHURN","DWLLSIZE","HND_PRICE","HND_WEBCAP","LOR","PHONES","TOT_ACPT","TOT_RET")
d2=dcat[,cnames2]
summary(d2)

#impute NA's according to given logic
d2$TOT_ACPT[is.na(d2$TOT_ACPT)]=5
d2$TOT_RET[is.na(d2$TOT_RET)]=6
d2$PHONES[is.na(d2$PHONES)]=1


#replace NA's in HND_WEBCAP with separate factor
vt=as.character(d2$HND_WEBCAP)
vt[is.na(vt)]="NWC"
vt[vt==""]="UNKW"
vt=as.factor(vt)
d2$HND_WEBCAP=vt

#remove DWLLSIZE and LOR from data frame
d2=d2[,-c(3,6)]

#Bind categorical and numeric data
df=cbind(d2,d1)
summary(df)

#save data frame for future reference
write.csv(df,"cal_3.csv")

#read data file

#recategorize HND_PRICE
histogram(~HND_PRICE|CHURN, df)
df$HND_PRICE=as.numeric(df$HND_PRICE)
df$HND_PRICE=ifelse(df$HND_PRICE<30,1,ifelse(df$HND_PRICE<100,2,ifelse(df$HND_PRICE<150,3,4)))

#recategorize PHONES
histogram(~PHONES|CHURN,df)
df$PHONES[df$PHONES>2]=3

#recategorize TOT_ACPT
histogram(~TOT_ACPT|CHURN,df)
df$TOT_ACPT[df$TOT_ACPT<5]=1

#recategorize TOT_RET
histogram(~TOT_RET|CHURN,df)
df$TOT_RET[df$TOT_RET<6]=1
df$TOT_RET[df$TOT_RET==6]=2

#convert categorical to factors
source("functions.R")
dsub=apply(df[,c(1:7,27)], 2, factorize)
summary(dsub)
dsub=as.data.frame(dsub)

#combine factors and remaining numerical data
ds=cbind(dsub,df[,c(8:26,28:33)])

#write new data frame
write.csv(ds,"cal_4.csv")

#load mice package to impute
library(mice)

#check pattern of missing data
md.pattern(df[,c(3,14,17,24,31,32)])

#visually check pattern of missing data
library(VIM)
miss.plot=aggr(df[,c(3,14,17,24,31,32)], col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3)

#check margin plots to see if data is MCAR or not
marginplot(df[,c(3,14)])
marginplot(df[,c(3,17)])
marginplot(df[,c(3,24)])
marginplot(df[,c(3,31)])
marginplot(df[,c(3,32)])

#not enough meomry available to run the function
#need to reduce the number of categories in categorical data
#for that, we opt to bin the data into different bins
#Check the distribution of data to fix bin sizes

#calculate the predictor matrix based on correlations
library(mice)
predmatr=quickpred(df[,-1], mincor=0.2)

#create imputed datasets
dtemp = mice(df[,-1],m=4,maxit=5,method="pmm",seed=5070, predictorMatrix = predmatr)
summary(dtemp)
library(lattice)
densityplot(dtemp, xlim=c(0,100), ylim=c(0,0.1),layout = c(3, 2))

#as the distributions are widely varying, we forego impuatation and go for listwise deletion

#select dataset with complete records
df1=ds[complete.cases(ds),]

#Partition data into training and validation sets
set.seed(67890)
part=sample(seq_len(nrow(df1)),(0.6*nrow(df1)),replace=F)
dft=df1[part,]
dfv=df1[-part,]

##Run Naive Bayes
library(klaR)
modNB=NaiveBayes(CHURN~., dft[,-1])

# use the model on validation data 
vlNB=predict(modNB,dfv[,-1])

#plot ROC curve
rocc(dfv$CHURN,vlNB$posterior[,2])

#create new class based on cutoff
cls=ifelse(vlNB$posterior[,2]>0.5,1,0)
metrics(table(dfv$CHURN,cls))



###run logistic regression
modLR=glm(CHURN~., data=dft[,-1], family = "binomial")
summary(modLR)

#predict using model
vlLR=predict(modLR,newdata = dfv[,-1], type="response")

#draw roc curve
rocc(dfv$CHURN,vlLR)

#create confusion matrix
pLR=ifelse(vlLR>0.48,1,0)
metrics(table(dfv$CHURN,pLR))



##k-NN algorithm
library(kknn)
prKKNN=kknn(CHURN~.,train = dft[,-1],test = dfv[,-1],k = 5)

#print confusion matrix
metrics(table(dfv$CHURN,prKKNN$fitted.values))

##Fit a CART
library(rpart)
modCT=rpart(CHURN~.,dft[,-1],method="class",parms=list(split="information"))
printcp(modCT)

#plot tree
plot(modCT,uniform=T,branch=1,margin=.2)
text(modCT,cex=.6, pretty=0)

#test on validation data
vlCT=predict(modCT,newdata = dfv[,-1])
prCT=ifelse(vlCT[,2]>0.5,1,0)
metrics(table(dfv$CHURN,prCT))

# Use RandomForest
library(randomForest)
modRF=randomForest(CHURN~.,dft[,-1])
varImpPlot(modRF, cex=0.5)

#predict on validation data
pRF=predict(modRF,newdata = dfv[,-1])
pRF=as.data.frame(pRF)
metrics(table(dfv$CHURN,pRF))

###Include LOR as continuous variable
dg=read.table("cal_4.csv", header = T, sep = ",")
dg=dg[,-1]
dg$LOR=dc$LOR

#convert categorical to factors
source("functions.R")
dsub=apply(dg[,c(1:8)], 2, factorize)
summary(dsub)
dsub=as.data.frame(dsub)

#bind columns
dg=cbind(dsub,dg[,c(9:34)])
summary(dg)

#impute data
library(mice)
predmatr=quickpred(dg[,-1], mincor=0.2)

#create imputed datasets
dtemp <- mice(dg[,-1],m=4,maxit=4,seed=5070, predictorMatrix = predmatr)
summary(dtemp)
densityplot(dtemp, xlim=c(0,100), ylim=c(0,0.1),layout = c(3, 3))

#run logistic on imputed datasets
mLR=with(dtemp,glm(CHURN~HND_PRICE+HND_WEBCAP+PHONES+TOT_ACPT+TOT_RET+RETDAYS+AVGMOU+BLCK_DAT_MEAN+CALLWAIT_MEAN+CC_MOU_MEAN+CHANGE_MOU+COMP_DAT_MEAN+CUSTCARE_MEAN+DATOVR_MEAN+DROP_BLK_MEAN+DROP_DAT_MEAN+EQPDAYS+IWYLIS_VCE_MEAN+MOU_CDAT_MEAN+OPK_VCE_MEAN+OVRREV_MEAN+OWYLIS_VCE_MEAN+PLCD_VCE_MEAN+LOR,family = "binomial"))
summary(mLR)
mLR$analyses

#predict on the validation data
pLR=predict(mLR$analyses[[4]], newdata = dg[,-1], type = "response")
rocc(dg$CHURN,pLR)
pplr=ifelse(pLR>0.47,1,0)
metrics(table(dg$CHURN,pplr))



###predict churn on current scored data
dcur=read.table("current_score.csv", header = T, sep = ",")
names(dcur)=toupper(names(dcur))
dcur1=dcur[,c("AVGMOU","BLCK_DAT_MEAN","CALLWAIT_MEAN","CC_MOU_MEAN","CHANGE_MOU","COMP_DAT_MEAN","CUSTCARE_MEAN","DATOVR_MEAN","DROP_BLK_MEAN","DROP_DAT_MEAN","EQPDAYS","IWYLIS_VCE_MEAN","MOU_CDAT_MEAN","OPK_VCE_MEAN","OVRREV_MEAN","OWYLIS_VCE_MEAN","PLCD_VCE_MEAN","RECV_SMS_MEAN","RECV_VCE_MEAN","RETDAYS","RMMOU","ROAM_MEAN","TOTMRC_MEAN","TOTMRC_RANGE","UNAN_DAT_MEAN","UNAN_VCE_MEAN")]
dcur2=dcur[,c("CUSTOMER_ID","HND_PRICE","HND_WEBCAP","PHONES","TOT_ACPT","TOT_RET")]

#impute missing values in select predictors through logic
dcur1$RETDAYS=ifelse(is.na(dcur1$RETDAYS),0,ifelse(dcur1$RETDAYS<60,1,ifelse(dcur1$RETDAYS<120,2,ifelse(dcur1$RETDAYS<180,3,4))))
dcur1$RMMOU[is.na(dcur1$RMMOU)]=0
dcur1$ROAM_MEAN[is.na(dcur1$ROAM_MEAN)]=0

#remove negative values for EQPDAYS
dcur1$EQPDAYS[dcur1$EQPDAYS<0]=0
dcur1$EQPDAYS[is.na(dcur1$EQPDAYS)]=342

#recategorize HND_PRICE
dcur2$HND_PRICE=as.numeric(dcur2$HND_PRICE)
dcur2$HND_PRICE=ifelse(dcur2$HND_PRICE<30,1,ifelse(dcur2$HND_PRICE<100,2,ifelse(dcur2$HND_PRICE<150,3,4)))

#recategorize PHONES
dcur2$PHONES[dcur2$PHONES>2]=3

#recategorize TOT_ACPT
dcur2$TOT_ACPT[dcur2$TOT_ACPT<5]=1
dcur2$TOT_ACPT[is.na(dcur2$TOT_ACPT)]=5

#recategorize TOT_RET
dcur2$TOT_RET[dcur2$TOT_RET<7]=1
dcur2$TOT_RET[is.na(dcur2$TOT_RET)]=2

#replace NA's in HND_WEBCAP with separate factor
vt=as.character(dcur2$HND_WEBCAP)
vt[is.na(vt)]="NWC"
vt=as.factor(vt)
dcur2$HND_WEBCAP=vt

#convert categorical to factors
source("functions.R")
dsub=apply(dcur2, 2, factorize)
summary(dsub)
dsub=as.data.frame(dsub)
dcur1$RETDAYS=as.factor(dcur1$RETDAYS)
df.cur=cbind.data.frame(dsub,dcur1)
names(df.cur)
df.cur=df.cur[complete.cases(df.cur),]

#score the data using the random Forest model that we had obtained
#predict on current_score data
pRF.cur=predict(modRF,newdata = df.cur[,-1])
pRF=as.data.frame(pRF.cur)
df.cur$CHURN=pRF.cur
write.csv(df.cur,"CHURN_current_score.csv")




##carry out above process for future_score data
dfut=read.table("future_score.csv", header = T, sep = ",")
names(dfut)=toupper(names(dfut))
dfut1=dfut[,c("AVGMOU","BLCK_DAT_MEAN","CALLWAIT_MEAN","CC_MOU_MEAN","CHANGE_MOU","COMP_DAT_MEAN","CUSTCARE_MEAN","DATOVR_MEAN","DROP_BLK_MEAN","DROP_DAT_MEAN","EQPDAYS","IWYLIS_VCE_MEAN","MOU_CDAT_MEAN","OPK_VCE_MEAN","OVRREV_MEAN","OWYLIS_VCE_MEAN","PLCD_VCE_MEAN","RECV_SMS_MEAN","RECV_VCE_MEAN","RETDAYS","RMMOU","ROAM_MEAN","TOTMRC_MEAN","TOTMRC_RANGE","UNAN_DAT_MEAN","UNAN_VCE_MEAN")]
dfut2=dfut[,c("CUSTOMER_ID","HND_PRICE","HND_WEBCAP","PHONES","TOT_ACPT","TOT_RET")]

#impute missing values in select predictors through logic
dfut1$RETDAYS=ifelse(is.na(dfut1$RETDAYS),0,ifelse(dfut1$RETDAYS<60,1,ifelse(dfut1$RETDAYS<120,2,ifelse(dfut1$RETDAYS<180,3,4))))
dfut1$RMMOU[is.na(dfut1$RMMOU)]=0
dfut1$ROAM_MEAN[is.na(dfut1$ROAM_MEAN)]=0

#remove negative values for EQPDAYS
dfut1$EQPDAYS[dfut1$EQPDAYS<0]=0
dfut1$EQPDAYS[is.na(dfut1$EQPDAYS)]=342

#recategorize HND_PRICE
dfut2$HND_PRICE=as.numeric(dfut2$HND_PRICE)
dfut2$HND_PRICE=ifelse(dfut2$HND_PRICE<30,1,ifelse(dfut2$HND_PRICE<100,2,ifelse(dfut2$HND_PRICE<150,3,4)))

#recategorize PHONES
dfut2$PHONES[dfut2$PHONES>2]=3

#recategorize TOT_ACPT
dfut2$TOT_ACPT[dfut2$TOT_ACPT<5]=1
dfut2$TOT_ACPT[is.na(dfut2$TOT_ACPT)]=5

#recategorize TOT_RET
dfut2$TOT_RET[dfut2$TOT_RET<7]=1
dfut2$TOT_RET[is.na(dfut2$TOT_RET)]=2

#replace NA's in HND_WEBCAP with separate factor
vt=as.character(dfut2$HND_WEBCAP)
vt[is.na(vt)]="NWC"
vt=as.factor(vt)
dfut2$HND_WEBCAP=vt

#convert categorical to factors
source("functions.R")
dsub=apply(dfut2, 2, factorize)
summary(dsub)
dsub=as.data.frame(dsub)
dfut1$RETDAYS=as.factor(dfut1$RETDAYS)
df.fut=cbind.data.frame(dsub,dfut1)
names(df.fut)
df.fut=df.fut[complete.cases(df.fut),]

#score the data using the random Forest model that we had obtained
#predict on future_score data
pRF.fut=predict(modRF,newdata = df.fut[,-1])
df.fut$CHURN=pRF.fut
write.csv(df.fut,"CHURN_future_score.csv")
