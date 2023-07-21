## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_08_18

ma_3pt <- function(my_data){ 
  n <- length(my_data)  
  ma <- enframe(stats::filter(my_data, filter = c(1, 1, 1)/3))
  ma$value[1] <- 0.5*my_data[1] + 0.5*my_data[2]
  ma$value[n] <- 0.5*my_data[n] + 0.5*my_data[n - 1]
  return(as.numeric(ma$value))
}