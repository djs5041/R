above10 <- function(x) {
   use <- x >10
   x[use]
}

aboveN <- function(x,n = 10,removeNA = TRUE) {
   use <- x > n
   
   x[use]
}

colMean <- function(x, removeNA = TRUE) {
	nc <-ncol(x)
	means <- numeric(nc)
	
	for(i in 1:nc) {
		means[i] <- mean(y[,i],na.rm=removeNA)
	}
	means
}

f <- function(a,b=1,c=2,d=NULL,e=1){
	print(a)
	print(a^2)
	print(e)
	a+b*2
}