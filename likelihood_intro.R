## likelihood

## "likelihood is not necessaily probability"

## Experiment
## Suppose a game between Man U vs Chelsea and Man U scored 3 goals - Man U is home team

## Random Variable
## x : Number goals by home team

x<-3

## Prob distribution 
## x ~ Poisson(lambda)
## x ~ Geometric(p)

## (i) Goodness of fit - very old
## (ii) Model Selection Criterion - AIC, BIC

## P(X<=2)=? what is the bottleneck in this problem?

## lambda is unknown
## we want to make an educated guess about lambda

## Suppose lambda=1, P(X=3|lambda=1)=?

dpois(x,lambda = 1)
dpois(x,lambda = 2)
dpois(x,lambda = 3)
dpois(x,lambda = 3.23)
dpois(x,lambda = 4)


lambda_1<-seq(0.01,6,by=0.05)
f<-dpois(x,lambda = lambda_1)

plot(lambda_1,f,cex=0.3,ylab = "P(X = 3 | lambda) : likelihood")

## Let's extend this for sample of size 3

x<-c(3,0,2)
## suppose lambda=1 , P(X1=3, X2=0, X3= 2 | lambda =1)=?
##  Assume : each games are independent
##  X1,X2,...,Xn independently follow Poisson(lambda) 

## P(X1=3, X2=0, X3= 2 | lambda =1) = P(X1=3|lambda =1)*P(X2=0|lambda =1) * P(X3=2|lambda =1) 

prod(dpois(x,lambda = 1))

sum(log(dpois(x,lambda = 1)))

f<-rep(NA,length(lambda_1))
for(i in 1:length(lambda_1)){
  f[i]<-prod(dpois(x,lambda = lambda_1[i]))
}

plot(lambda_1,f,cex=0.3,ylab = "likelihood")
abline(v=1.6)

### The value at which the likelihood function is maximum is known as the Maximum Likelihhod Function (MLE) of parameter lambda

f<-rep(NA,length(lambda_1))
for(i in 1:length(lambda_1)){
  f[i]<--sum(dpois(x,lambda = lambda_1[i],log = TRUE))
}

plot(lambda_1,f,cex=0.3,ylab = "neg log-likelihood")
abline(v=1.6)

neg_log_like_poisson<-function(data,theta){
  nll<--sum(dpois(data,lambda = theta,log = TRUE))
  return(nll)
}

theta.init<-1
neg_log_like_poisson(data=x, theta = theta.init)

fit_poisson<-optimize(f=neg_log_like_poisson
                      ,interval=c(0,10)
                      ,maximum = FALSE
                      ,data=x)

## MLE: lambda_hat = 1.666684

nll(lambda) = - sum (log exp(- lambda)*lambda^x/x!)
d nll(lambda)/d lambda = 0

mean(x)

##

neg_log_like_geom<-function(data,theta){
  nll<--sum(dgeom(data,prob = theta,log = TRUE))
  return(nll)
}

fit_geometric<-optimize(f=neg_log_like_geom
                        ,interval=c(0,1)
                        ,maximum = FALSE
                        ,data=x)


### Akaike Information Criterion (AIC)

AIC_geometric =?
AIC_poisson =?  
