
mr<-lm(y ~ x + age, data=df)
summary(mr)
yhat2<-summary(mr)$coefficients[1,1]+summary(mr)$coefficients[2,1]*df$x+summary(mr)$coefficients[3,1]*df$age
plot(df$y,yhat2,col="green",lwd=2,cex=0.5,pch=16,xlab="y",ylab="yhat2")
e<-(df$y-yhat2)
plot(df$y,e,col="blue",lwd=2,cex=0.5,pch=16,xlab="y",ylab="error")