# we will examine as to what happens if we add x x0 and x1 as explanatory variables
# of course x=x0+x1
# so if x is rounded off to 3 decimal places.
# now we have near multi colinearity
# R2 is about same as S6 
df1$x<-floor(df1$x*1000)/1000
fr<-lm(y~x+x0+x1+age, df1)
summary(fr)
yhat4<-summary(fr)$coefficients[1,1]+summary(fr)$coefficients[2,1]*df1$x+summary(fr)$coefficients[3,1]*df1$x0+summary(fr)$coefficients[4,1]*df1$x1+summary(fr)$coefficients[5,1]*df1$age
plot(df1$y,yhat4,col="green",lwd=2,cex=0.5,pch=16,xlab="y",ylab="yhat4")
e<-(df1$y-yhat4)
plot(df1$y,e,col="blue",lwd=2,cex=0.5,pch=16,xlab="y",ylab="error")
