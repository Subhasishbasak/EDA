y=c(0,1,3,5)
x=c(-0.86,-0.3,-0.05,0.73)
n=c(5,5,5,5)
L=function(par)
{
  a=par[1]
  b=par[2]
  z=exp(a+b*x)
  L=-prod(((z/(1+z))^y)*(1-(z/(1+z)))^(n-y))
}  
result=optim(par = c(1,1),L,hessian = T)
H=result$hessian
#estimated model:
#log(p/1-p)= 0.8465 + 7.7488*x
x=0.1
p=exp(0.8465 + 7.7488*x)/(1+exp(0.8465 + 7.7488*x))
#for x=0.1 , estimated p is 0.83                   
#Expected no. of deaths,k=n*p_hat
k=30*0.83
#K is apprx 25
#Prob[k>20]=1-pbinom(20,30,.83)=.977


### hessian method for estimating se
asym_var=solve(H)
se_hat=sqrt(diag(H))


###parametric bootstrap method for estimating se

for i in 1:5:

