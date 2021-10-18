#footnote6
y <- matrix(1:51, nrow = 51, ncol = 1) 
x1 <- as.matrix(rep(1,79))
x0 <- as.matrix(rep(1,79))
x <- cbind(x0,x1)

for (i in 1:51){
  
  
  x[i,2] <- -75 + 3*(i-1)
  
  
  y[i] <- (y[i])*((-1)^i)
  
  
}
b <- solve(t(x)%*%x)%*%t(x)%*%y
k <- length(b)
T <- length(y)



x1 <- matrix(1:4,ncol=1)
x0 <- matrix(rep(1,4),ncol=1)
x <- as.matrix(cbind(x0,x1))
y <- matrix(seq.int(2,8,2),ncol = 1)
b <- solve(t(x)%*%x)%*%t(x)%*%y
k <- length(b)
T <- length(y)

########
a <- 0 
for (i in 0:100000) {
  b1 <- abs(b[2]) 
  cb <- matrix(c(b1),ncol = 1)
  b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
  nb <- as.matrix(rbind(b0,cb))
  yh <- x%*%nb 
  ne <- y-yh
  nrss <- t(ne)%*%ne
  nsig <- as.numeric((nrss)/(T-k))
  nvb <- diag(nsig*(solve(t(x)%*%x)))
  testa <- abs(b[2]/2)
  testc <- matrix(c(testa),ncol = 1)
  testb <- as.matrix(sum(y-(x[,2]%*%testc))/T)
  tnb <- as.matrix(rbind(tb0,testc))
  tyh <- x%*%tnb 
  tne <- y-tyh
  tnrss <- t(tne)%*%tne
  tsig <- as.numeric((tnrss)/(T-k))
  tvb <- diag(tsig*(solve(t(x)%*%x)))
  if (nrss < tnrss) {
    b1 <- abs(as.numeric(((.25*(1-((-1)^i)*(2*i+1)))*(sqrt(nvb[2])))/500+(b[2])))
    cb <- matrix(c(b1),ncol = 1)
    b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
    nb <- as.matrix(rbind(b0,cb))
    yh <- x%*%nb 
    ne <- y-yh
    ane <- sum(abs(ne))/T
    ybar <- sum(y)/T
    ydiff <- sum(abs(y-ybar))/T
    efav <- (ydiff/ane)
    if (efav < 1) {
      efav <- 0 
    }
    else {
      efav <- efav 
    }
    a <- efav + a
  }
  else { 
    b1 <- abs(as.numeric(((.25*(1-((-1)^i)*(2*i+1)))*(sqrt(tvb[2])))/500-(b[2]/2)))
    cb <- matrix(c(b1),ncol = 1)
    b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
    nb <- as.matrix(rbind(b0,cb))
    yh <- x%*%nb 
    ne <- y-yh
    ane <- sum(abs(ne))/T
    ybar <- sum(y)/T
    ydiff <- sum(abs(y-ybar))/T
    efav <- (ydiff/ane)
    if (efav < 1 ) {
      efav <- 0 
    }
    a <- efav + a
  }
  if (is.infinite(a)==TRUE) {
    a <- 1*(10^100)
  }
}
x[,-1]
nx[,1:2]
iop <- matrix(c(1,2),ncol = 1)
iop[1:2]
iop
tb <- 34
sum(y-x[,-1]%*%matrix(c(iop[-c(1,2)],tb),ncol = 1))
nb
######
a <- 0 
pwr <- matrix(,nrow = 9, ncol = T)
for (q in 2:10) { 
  pwr[(q-1),] <- (x[,2])^(q) }
nx <- cbind(x,t(pwr))
for (q in 2:11) {
  x <- nx[,1:q]
  if (det((solve(t(x)%*%x)))<1e^-19){
    break }
  b <- solve(t(x)%*%x, tol = 1e-30)%*%t(x)%*%y
  bb <- abs(b[q])
  nb <- matrix(c(b[1:(q-1),],bb),nrow = (q))
  yh <- x%*%nb 
  ne <- y-yh
  nrss <- t(ne)%*%ne
  nsig <- as.numeric((nrss)/(T-k))
  nvb <- diag(nsig*(solve(t(x)%*%x)))
  ane <- sum(abs(ne))/T
  ybar <- sum(y)/T
  ydiff <- sum(abs(y-ybar))/T
  efav <- (ydiff/ane)
  a <- a + efav
  for (i in 1:1000) {
    tb <- abs(as.numeric(((.25*(1-((-1)^i)*(2*i+1)))*(sqrt(nvb[q])))/25+(nb[q])))
    cb <- matrix(c(nb[-c(1,q),],tb),ncol = 1)
    b0 <- as.matrix(sum(y-(x[,-1]%*%cb))/T)
    nb <- as.matrix(rbind(b0,cb))
    yh <- x%*%nb 
    ne <- y-yh
    ane <- sum(abs(ne))/T
    ybar <- sum(y)/T
    ydiff <- sum(abs(y-ybar))/T
    efav <- (ydiff/ane)
    if (efav < 1) {
      efav <- 0 
    }
    a <- efav + a
  }
  if (is.infinite(a)==TRUE) {
    a <- 1*(10^100)
  }
}

pos <- a 
######
a <- 0
for (i in 0:100000) {
  b1 <- -abs(b[2]) 
  cb <- matrix(c(b1),ncol = 1)
  b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
  nb <- as.matrix(rbind(b0,cb))
  yh <- x%*%nb 
  ne <- y-yh
  nrss <- t(ne)%*%ne
  nsig <- as.numeric((nrss)/(T-k))
  nvb <- diag(nsig*(solve(t(x)%*%x)))
  testa <- -abs(b[2]/2)
  testc <- matrix(c(testa),ncol = 1)
  testb <- as.matrix(sum(y-(x[,2]%*%testc))/T)
  tnb <- as.matrix(rbind(tb0,testc))
  tyh <- x%*%tnb 
  tne <- y-tyh
  tnrss <- t(tne)%*%tne
  tsig <- as.numeric((tnrss)/(T-k))
  tvb <- diag(tsig*(solve(t(x)%*%x)))
  if (nrss < tnrss) {
    b1 <- -abs(as.numeric(((.25*(1-((-1)^i)*(2*i+1)))*(sqrt(nvb[2])))/500+(b[2])))
    cb <- matrix(c(b1),ncol = 1)
    b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
    nb <- as.matrix(rbind(b0,cb))
    yh <- x%*%nb 
    ne <- y-yh
    ane <- sum(abs(ne))/T
    ybar <- sum(y)/T
    ydiff <- sum(abs(y-ybar))/T
    efav <- (ydiff/ane)
    if (efav < 1) {
      efav <- 0 
    }
    a <- efav + a
  }
  else { 
    b1 <- -abs(as.numeric(((.25*(1-((-1)^i)*(2*i+1)))*(sqrt(tvb[2])))/500-(b[2]/2)))
    cb <- matrix(c(b1),ncol = 1)
    b0 <- as.matrix(sum(y-(x[,2]%*%cb))/T)
    nb <- as.matrix(rbind(b0,cb))
    yh <- x%*%nb 
    ne <- y-yh
    ane <- sum(abs(ne))/T
    ybar <- sum(y)/T
    ydiff <- sum(abs(y-ybar))/T
    efav <- (ydiff/ane)
    if (efav < 1 ) {
      efav <- 0 
    }
    a <- efav + a
  }
  if (is.infinite(a)==TRUE) {
    a <- 1*(10^100)
  }
}
neg <- a 
tot <- neg + pos
ptot <- (pos/tot -.5)*2; ptot 

