## Here we are studying the idea of sampling distributions
## If exact sampling distribution is not known to us, in that case we check if large sample properties work or not
## CLT approximates the sampling distribution of sample mean if (i) sample sizes are large, (ii) population mean and variance exists

## Ex 1 sampling distribution of sample proportion

n<-100 ## try different sample size like n=10,20,30,50
p<-0.6
sim.size<-10000
p_hat<-s<-rep(NA,sim.size)

for(i in 1:sim.size){
  x<-rbinom(n,size = 1,prob = p)
  p_hat[i]<-sum(x)/n  
  s[i]<-sd(x)
}
hist(p_hat,probability = TRUE,col="red",xlim = c(0,1))

hist(s,probability = TRUE, col = "red")

sigma2<-p*(1-p)
p_hat<-sort(p_hat)
lines(p_hat,dnorm(p_hat,mean = p,sd=sqrt(sigma2/n)))


## Ex2: sampling distribution of sample mean, when population follows Gaussian


n<-20
mu<-0
sigma<-1
sim.size<-10000
x_bar<-s<-v<-rep(NA,sim.size)
for(i in 1:sim.size){
  x<-rnorm(n,mean = mu,sd = sigma)
  x_bar[i]<-sum(x)/n  
  s[i]<-sd(x)
  v[i]<-var(x)
}
hist(x_bar,probability = TRUE,col="red",xlim=c(mu-2.5*sigma,mu+2*sigma))
x_bar<-sort(x_bar)
lines(x_bar,dnorm(x_bar,mean = mu,sd=sqrt(sigma^2/n)))
x_bar<-seq(mu-2.5*sigma,mu+2*sigma,0.1)
lines(x_bar,dnorm(x_bar,mean = mu,sd=sqrt(sigma^2)),col="blue")

hist(s, probability = TRUE,col = "red")
v<-sort(v)
hist(n*v, probability = TRUE, col="blue")
lines(n*v,dchisq(n*v,df=n))



### Ex 3: sampling distribution of sample mean, when population follows Exponential

rate<-1
n<-30
sim.size<-10000

x<-seq(0,3,length.out = 500)
plot(x,dexp(x,rate = rate),type = "l")

x_bar<-v<-rep(NA,sim.size)
for(i in 1:sim.size){
  x<-rexp(n,rate = rate)
  x_bar[i]<-sum(x)/n  
  v[i]<-var(x)
}
hist(x_bar,probability = TRUE,col="red")
hist(v,probability = TRUE,col="blue")


### Ex 4: sampling distribution of sample mean, when population follows uniform

n<-30
sim.size<-10000

x<-seq(0,1,length.out = 500)
plot(x,dunif(x,min = 0, max = 1),type = "l")

x_bar<-rep(NA,sim.size)
for(i in 1:sim.size){
  x<-runif(n,min = 0, max = 1)
  x_bar[i]<-sum(x)/n  
}
hist(x_bar,probability = TRUE,col="red",nclass = 20)

## Ex 5: sampling distribution of sample mean, when population follows Cauchy distribution

n<-30
sim.size<-10000

x<-seq(-3,3,length.out = 500)
plot(x,dcauchy(x,location = 0, scale= 1),type = "l")

x_bar<-rep(NA,sim.size)
for(i in 1:sim.size){
  x<-rcauchy(n,location = 0, scale = 1)
  x_bar[i]<-sum(x)/n  
}
hist(x_bar,probability = TRUE,col="red",nclass = 50)


