# This code generates data 
# you can see the true model here, the independent variables are 
#x :  alchohol in breath exhaled - in some units
#g : gender 1 : M or 0 : F
#age : to be read from driver licence
#dependent variable
#y =0.6*x*g+0.30*x*(1-g)+0.05*(60-age)+0.5*z
#z : error normal mean 0 var 1


seed=1029298
set.seed(seed)
m=6000
x<-10*runif(m,0.1,1.1)
age<-sample(c(20:60),n,replace=TRUE)
z<-rnorm(m)
g<-rbinom(m,1,0.6)
y<-0.6*x*g+0.30*x*(1-g)+0.05*(60-age)+0.5*z
d1<-cbind(x,age)
d2<-cbind(d1,g)
d3<-cbind(d2,y)
df<-as.data.frame(d3)
write.csv(df,"regdata.csv")
