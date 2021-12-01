## COLLATZ CONJECTURE
#####################

# This code simulates collatz conjecture for a given sequence
#############################################################

## FUNCTION
collatz_conjecture <- function(number){
  
  result <- number
  i=1
  
  while(result!=1){
    
    if(result%%2==0){
      result <- result/2
    }else{
      result <- 3*result+1
    }
    #print(result)
    i=i+1
  }
  print(sprintf("%i is converged to 1 in %i iterations", number,i))
  return(i)
}

## Single Example
collatz_conjecture(7)

## Running through a Loop
convergence_ite <- list()
mysequence <- seq(2,1000,1)

for(i in mysequence){
  result <- collatz_conjecture(i)
  convergence_ite <- append(convergence_ite,result)}

## Plotting the Number of Iterations to Converge
plot(mysequence, convergence_ite, type="c")
