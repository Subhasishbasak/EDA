data=read.csv(file.choose(),header = T)
data$credit.amount
i=5
summary(data[,i])
table(data[,i])
hist(data[,i],main = "Credit amount of individuals",col = "Grey",nclass=20,xlab="Credit amount",probability=T)
barplot(table(data[,i]))

A=matrix(c(length(data[,i][data[,21]==1 & data[,i]==1]),length(data[,i][data[,21]==2 & data[,i]==1]),length(data[,i][data[,21]==1 & data[,i]==2]),length(data[,i][data[,21]==2 & data[,i]==2]),length(data[,i][data[,21]==1 & data[,i]==3]),length(data[,i][data[,21]==2 & data[,i]==3]),length(data[,i][data[,21]==1 & data[,i]==4]),length(data[,i][data[,21]==2 & data[,i]==4])),byrow=F,nrow=2)
colnames(A)=c("< 0 DM\n 50.7%","0-200 DM\n 60.9%",">= 200 DM\n 77.7%","no checking account\n 88.3%")
rownames(A)=c("Good","Bad")
barplot(A,xlim=c(0, ncol(A) + 2),legend=T,legend=colnames(A)
,args.legend = list(ncol(A)+2),xlab = "Account Status\nGood%",ylab="No. of individuals",main = "Individuals classified wrt Checking Account status")

barplot(A,xlim=c(0, ncol(A) + 2),legend=T,args.legend = list(ncol(A)+2))


B=matrix(c(length(data[,i][data[,21]==1 & data[,i]==1]),length(data[,i][data[,21]==2 & data[,i]==1]),length(data[,i][data[,21]==1 & data[,i]==2]),length(data[,i][data[,21]==2 & data[,i]==2]),length(data[,i][data[,21]==1 & data[,i]==3]),length(data[,i][data[,21]==2 & data[,i]==3]),length(data[,i][data[,21]==1 & data[,i]==4]),length(data[,i][data[,21]==2 & data[,i]==4]),length(data[,i][data[,21]==1 & data[,i]==5]),length(data[,i][data[,21]==2 & data[,i]==5])),byrow=F,nrow=2)
colnames(B)=c("< 100 DM\n 64%","100-500 DM\n 66.9%","500-1000 DM\n 82.5%",">=1000 DM\n 87.5%","no savings account\n 82.5%")
rownames(B)=c("Good","Bad")
barplot(B,xlim=c(0, ncol(B) + 2),legend=T
        ,args.legend = list(ncol(B)+2),xlab = "Account Status\nGood%",ylab="No. of individuals",main = "Individuals classified wrt Savings Account status")



C=matrix(c(length(data[,i][data[,21]==1 & data[,i]==1]),length(data[,i][data[,21]==2 & data[,i]==1]),length(data[,i][data[,21]==1 & data[,i]==2]),length(data[,i][data[,21]==2 & data[,i]==2]),length(data[,i][data[,21]==1 & data[,i]==3]),length(data[,i][data[,21]==2 & data[,i]==3]),length(data[,i][data[,21]==1 & data[,i]==4]),length(data[,i][data[,21]==2 & data[,i]==4]),length(data[,i][data[,21]==1 & data[,i]==5]),length(data[,i][data[,21]==2 & data[,i]==5])),byrow=F,nrow=2)
colnames(C)=c("Unemployed\n 64%","<1 year\n 66.9%","1-4 year\n 82.5%","4-7 year\n 87.5%",">7 year\n 82.5%")
rownames(C)=c("Good","Bad")
barplot(C,xlim=c(0, ncol(C) + 3),legend=T
        ,args.legend = list(ncol(C)+3),xlab = "Employment Status\nGood%",ylab="No. of individuals",main = "Individuals classified wrt Employment status")


legend=colnames(A)
A=matrix(c(length(data[,i][data[,21]==1 & data[,i]==0]),length(data[,i][data[,21]==2 & data[,i]==0]),length(data[,i][data[,21]==1 & data[,i]==1]),length(data[,i][data[,21]==2 & data[,i]==1]),length(data[,i][data[,21]==1 & data[,i]==2]),length(data[,i][data[,21]==2 & data[,i]==2]),length(data[,i][data[,21]==1 & data[,i]==3]),length(data[,i][data[,21]==2 & data[,i]==3]),length(data[,i][data[,21]==1 & data[,i]==4]),length(data[,i][data[,21]==2 & data[,i]==4])),byrow=F,nrow=2)
colnames(A)=c("no credits taken\n 37.5%","credits paid back duly\n 42.8%","existing credits paid\n 68.1%","delay in paying\n 68.1%","critical account\n 82.9%")
rownames(A)=c("Good","Bad")
barplot(A,xlim=c(0, ncol(A) + 2),legend=T,args.legend = list(ncol(A)+2),ylab="No. of individuals",main = "Individuals classified wrt Credit history")


aa=data.frame(table(data[,4]))
colnames(aa)=c("Purpose","Frequency")
rownames(aa)=c("car (new)", 
               "car (used)" ,
               "furniture/equipment", 
               "radio/television", 
               "domestic appliances", 
                "repairs", 
               "education", 
               "retraining", 
               "business", 
               "others")

dataa=mpg
p=ggplot(dumb,aes(dummy,credit.amount))
p+geom_boxplot()
dummy=data.frame(data$job)
dummy[dummy==4]="self-employed"
dumb=cbind(data,dummy)


barplot(table(data[,8]))

i=11
D=matrix(c(length(data[,i][data[,21]==1 & data[,i]==1]),length(data[,i][data[,21]==2 & data[,i]==1]),length(data[,i][data[,21]==1 & data[,i]==2]),length(data[,i][data[,21]==2 & data[,i]==2]),length(data[,i][data[,21]==1 & data[,i]==3]),length(data[,i][data[,21]==2 & data[,i]==3]),length(data[,i][data[,21]==1 & data[,i]==4]),length(data[,i][data[,21]==2 & data[,i]==4])),byrow=F,nrow=2)
colnames(D)=c("1\n 72.3%","2\n 68.5%","3\n 71.1%","4\n 69.9%")
rownames(D)=c("Good","Bad")
barplot(D,legend=T,xlim=c(0, ncol(D) + 2),
        ,args.legend = list(ncol(D)+24),xlab = "Residential status\nGood%",ylab="No. of individuals",main = "Individuals classified wrt residential status")

par(mfrow=c(1,2))
#hist(data$age,probability=T,main="",xlab="age groups",col = "grey",grid())
hist(data$age[data[,21]==1],probability=T,main="Good customers",xlab="age groups",col = "grey")
hist(data$age[data[,21]==2],probability=T,main="Bad customers",xlab="age groups",col = "grey")

hist(data$age[data[,21]==1],probability=T,main="Good customers",xlab="age groups",col = "grey")
hist(data$age[data[,21]==2],probability=T,main="Bad customers",xlab="age groups",col = "grey")

pie(table(data$X..credits))
pie(table(data$X..people))


table(data$resident)

pie(table(data$purpose))

length(data$age[data$age>=20 & data$age<=35 & data$purpose==0])
table(data$purpose)
length(data$age[data$age>=20 & data$age<=35 & data$purpose==0])*100/length(data$age[data$purpose==0])
length(data$age[data$age>=20 & data$age<=35 & data$purpose==1])*100/length(data$age[data$purpose==1])
length(data$age[data$age>=20 & data$age<=35 & data$purpose==2])*100/length(data$age[data$purpose==2])
length(data$age[data$age>=20 & data$age<=35 & data$purpose==3])*100/length(data$age[data$purpose==3])
length(data$age[data$age>=20 & data$age<=35 & data$purpose==9])*100/length(data$age[data$purpose==9])



table(data$purpose[data$age>=70 & data$age<=80])
table(data$purpose[data$age>=70 & data$age<=80 & data$credit.amount>3447])
