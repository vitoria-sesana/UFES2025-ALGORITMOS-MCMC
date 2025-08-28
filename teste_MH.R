# teste  ------------------------------------------------------------------

# distribuição alvo = exponencial 
# distribuição proposta = normal
target = function(x){
  return(ifelse(x<0,0,exp(-x)))
}

x = rep(2,10)
x[1] = 3     
for(i in 2:10){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x) 
  if(runif(1)<A){
    x[i] = proposed_x       
  } else {
    x[i] = current_x        
  }
}
x

plot(x,main="values of x visited by the MH algorithm")
