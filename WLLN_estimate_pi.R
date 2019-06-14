N<-1000000
S<-0
for(i in 1:N){
  x<-runif(1)
  y<-runif(1)
  d<-sqrt(x^2+y^2)
  if(d<1)S<-S+1
}
p_hat<-  S/N
pi_hat<- p_hat*4
pi_hat