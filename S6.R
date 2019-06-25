x1<-df$x*df$g
x0<-df$x*(1-df$g)
df0<-cbind(df,x0)
df1<-cbind(df0,x1)
fr<-lm(y~x0+x1+age, df1)
summary(fr)
yhat3<-summary(fr)$coefficients[1,1]+summary(fr)$coefficients[2,1]*df1$x0+summary(fr)$coefficients[3,1]*df1$x1+summary(fr)$coefficients[4,1]*df1$age
plot(df1$y,yhat3,col="green",lwd=2,cex=0.5,pch=16,xlab="y",ylab="yhat2")
e<-(df1$y-yhat3)
plot(df1$y,e,col="blue",lwd=2,cex=0.5,pch=16,xlab="y",ylab="error")
#This is the final model.