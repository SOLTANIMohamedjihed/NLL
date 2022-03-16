###Negative of the log likelihood


sigmoid = function(x){
  1/(1+exp(-x))
}

neg_log_likelihood = function(P, data, y, include_alpha = T){
  x = data[,names(data) != y]
  y_data = data[,y]

  # 1. Calculate theta
  if(include_alpha){

    # Multiply each value by their parameter
    multiplied =  mapply("*",x,P[2:length(P)])

    # We sum for each observation and add alpha
    theta =  rowSums(multiplied) + P[1]
  }else{
    theta =  rowSums(mapply("*",x,P))
  }

  # 2. Calculate p
  P = sigmoid(theta)
  #p = exp(theta) / (1 + exp(theta))

  # 3. Calculate -log likelihood
  val = -sum(y_data * log(P) + (1-y_data)*log(1-P))

  return(val)
}







