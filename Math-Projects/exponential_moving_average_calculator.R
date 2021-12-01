#########################################
# EXPONENTIAL-MOVING-AVERAGE CALCULATOR #
#########################################
# Created by Ugur Uresin, 2021 #
# Github: ugururesin #
#########################################

### E-M-A FUNCTION
##################
ema_calculator <- function(vector, periodicity, plot=FALSE, legend_pos="topleft"){
  
  #calculating the ema factor
  ema_factor  <- 2/(periodicity+1)
  
  #creating a list for ema values (first n elements will be NA)
  ema_list <- rep(NA, periodicity)
  
  #calculating the first ema value as the previous ones average
  pre_sum   <- sum(vector[1:periodicity])
  ema_first   <- (vector[periodicity]-pre_sum/periodicity)*ema_factor + pre_sum/periodicity
  
  #adding the first ema value to the list
  ema_list <- append(ema_list, ema_first)
  
  #calculating the other ema values
  for(i in (periodicity+2):length(vector)){
    C = vector[i-1]
    P = ema_list[i-1]
    ema = P + (C-P)*ema_factor
    ema = round(ema,2)
    ema_list <- append(ema_list , ema)
  }
  
  if(plot==TRUE){
    plot(1:length(vector), vector, type="l", xlab="Index", ylab="Value", col="red")
    lines(1:length(ema_list), ema_list, col="blue")
    legend(legend_pos, legend=c("Data","EMA"), col=c("red", "blue"), lty=1:1.5, cex=0.75)
  }
  
  return(ema_list)
}


# E-M-A CALCULATION EXAMPLE
###########################
data <- c(30.50, 30.60, 30.35, 29.70, 28.10, 29.25, 30.25, 30.90, 31.05, 32.15, 32.00, 31.5)
periodicity <- 4

ema_calculator(data, periodicity, plot=TRUE)

### END OF THE CODE ###