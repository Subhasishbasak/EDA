B_wins<-function(sim.size){

  N<-sim.size ## simulation size
  dice<-1:6   ## define dice
  count<-0    
  for(i in 1:N){
    A<-sample(dice,size=1,replace = T)
    B<-sample(dice,size = 1,replace = T)
    if(B>A) count<-count+1  
  }
  
  return(count/N)
}

## B_wins(sim.size = 100)



