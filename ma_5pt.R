## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_08_18

ma_5pt <- function(my_data){ 
  n <- length(my_data)  
  ma <- enframe(stats::filter(my_data, filter = c(1, 1, 1, 1, 1)/5))
  ma$value[1] <- 0.5*my_data[1] + 0.5*my_data[2]
  ma$value[2] <- (1/3)*my_data[1] + (1/3)*my_data[2] + (1/3)*my_data[3]
  ma$value[n - 1] <- (1/3)*my_data[n] + (1/3)*my_data[n - 1] + (1/3)*my_data[n - 2]
  ma$value[n] <- 0.5*my_data[n] + 0.5*my_data[n - 1]
  return(as.numeric(ma$value))
}