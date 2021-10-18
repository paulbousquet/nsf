#footnote5

const <- 1000 
beta <- 1/2 
range <- 2*const
mat <- matrix(,nrow = range,ncol = 1) 
for (i in 1:const){
  mat[i] <- const-i+1 
  mat[range-i+1] <- -1*mat[i]
}


matq <- (mat+beta)/beta
var(mat)+beta/sqrt(beta) var(matq)*(beta/3)
var(mat)+beta/3>var(matq)*(beta/sqrt(beta))
