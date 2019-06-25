#illustrates effect of multicolinearity
#res1 gives the coefficients of the model fr which is good
#res2 gives the coefficients of the model zr which has multicolinearity


n<-500
res1<-matrix(0,nrow=20,ncol=4)
res2<-matrix(0,nrow=20,ncol=5)
for(i in c(1:20))
{
	s<-sample(c(1:m),n,replace=FALSE)
	df2<-df1[s,]
	fr<-lm(y~x0+x1+age, df2)
	#summary(fr)
	res1[i,]<-summary(fr)$coefficients[,1]
	zr<-lm(y~x+x0+x1+age, df2)
	#summary(zr)
	res2[i,]<-summary(zr)$coefficients[,1]
}
print(res1)
print(res2)

#see the wild variation in res2