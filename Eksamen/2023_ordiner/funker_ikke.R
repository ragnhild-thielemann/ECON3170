
f = function(eta){
  c = 0.1
  diff = 1
  print(c)
  
  print(diff)
  while (diff>1){
    if (eta == 1){
      diff = log(c)-c
      c = c + 1
      print(diff)
    }else{
      diff = ((c**(1-eta)-1)/(1-eta))-c
      c = c + 0.1
    }
   
  }
  return (c)}

print(f(2))