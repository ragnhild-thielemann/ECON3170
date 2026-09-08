

game = function(){
  number = 1
  mynt = runif(1,0,1)
  while(mynt<0.5){
    mynt = runif(1,0,1)
    
    number = number + 1
  }
  return(number)
}

gains = function(){
  n = game()

  return (2**((n)))
}
print(game())
sprintf("Han vinner %s krone(r)", gains())

nytte = function(eta,c){
  n = 1000
  total = 0
  
  for (n in 1:n){
    c = gains()
    if (eta != 1){
    total = total +((c**(1-eta)-1))/(1-eta)
    }else{
      total = total + log(c)
    }
    
  }
  
  return (total/n)
}

eta_vektor = c(0.5,1,2)

for (n in eta_vektor){
  print(sprintf("eta = %s gir u(c) %0f",n,nytte(n)))
}


likevekt = function(eta){
  c = 0.01
  diff = -1
  print(c)
  while (diff<1){
  if (eta == 1){
    diff = log(c)-c
    c = c + 0.1
  }else{
    diff = ((c(1-eta)-1)/(1-eta))-c
    c = c + 0.1
  }
    print(c)
  }
  return (c)}

print(likevekt(2))