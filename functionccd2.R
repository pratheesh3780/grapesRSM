gen_level_2 <- function(x1, y1, x2, y2) {
  # x min
  # y max
  mid1 <- (x1 + y1) / 2
  l11 <- x1
  l21 <- (((2.414*mid1)-y1)/1.414)
  l31 <- mid1
  l41 <- ((y1+(0.414*mid1))/1.414)
  l51 <- y1
  levels1 <- as.vector(c(l11, l21, l31, l41, l51))
  levels1 <- round(levels1, 3)
  L1 <- levels1
  
  mid2 <- (x2 + y2) / 2
  l12 <- x2
  l22 <- (((2.414*mid2)-y2)/1.414)
  l32 <- mid2
  l42 <- ((y2+(0.414*mid2))/1.414)
  l52 <- y2
  levels2 <- as.vector(c(l12, l22, l32, l42, l52))
  levels2 <- round(levels2, 3)
  L2 <- levels2
  
  # design matrix
  D <- matrix(c(
    -1,	-1,
    1,	-1,
    -1,	1,
    1,	1,
    0,	0,
    0,	0,
    -1.414,	0,
    1.414,	0,
    0,	-1.414,
    0,	1.414,
    0,	0,
    0,	0,
    0,	0
    
  ), nrow = 13, ncol = 2, byrow = TRUE)
  run <- as.data.frame(c(1:13))
  obs <- as.data.frame(matrix(0, 13, 1))
  
  #function to round
  round_to_high<-function(x){
    if(x<=0){
      y=floor(x)
    }
    else if (x>0){
      y= ceiling(x)
    }
    y
  }
  
  Factor_1 <- matrix(0, 13, 1)
  for (i in 1:13) {
    Factor_1[i, 1] <- c(L1[round_to_high(D[i, 1]) + 3])
  }
  
  Factor_2 <- matrix(0, 13, 1)
  for (i in 1:13) {
    Factor_2[i, 1] <- c(L2[round_to_high(D[i, 2]) + 3])
  }
  
  Final_des <- cbind(run, D, Factor_1, Factor_2, obs)
  colnames(Final_des) <- c("runs", "X1", "X2", "T1", "T2", "Observation")
  Final_des
}