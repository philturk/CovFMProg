## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_08_18

trend_check <- function(Obs, Exp){
  
  ##### function trend_check()
  #
  # Checks for adherence to the 7-day strict trend rule
  # Arguments:
  # Obs = 7 observed data
  # Exp = 7 expected values (median)
  #
  # Test: x <- c(-3:3); y <- c(3:-3); trend_check(x, y) ## Caution
  # Test: x <- c(-3:3); y <- c(3,2,1,1,-4,2,1); trend_check(x, y) ## Pass
  # Test: x <- c(-3:3); y <- c(3:1,1,3,4,1); trend_check(x, y) ## Can't have difference of 0
  #
  #--------------------------------------------------------
  
  del1 <- Obs - Exp  
  del2 <- diff(del1)
  stopifnot(del2 != 0)
  sgn_del2 <- if_else(del2 > 0, 1, -1)
  rle(sgn_del2)
  max_rn <- max(rle(sgn_del2)$lengths)
  
  if(max_rn >= 5){ cat("Caution: inspect data") 
  }else{ cat("Pass") 
  }
}
