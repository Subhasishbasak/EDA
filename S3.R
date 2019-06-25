yhat<-summary(sr)$coefficients[1,1]+summary(sr)$coefficients[2,1]*df$x
plot(df$y,yhat,col="blue",lwd=2,cex=0.5,pch=16,xlab="y",ylab="yhat")
