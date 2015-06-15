colMean <- function(x, removeNA = TRUE) {
	nc <-ncol(x)
	means <- numeric(nc)
	
	for(i in 1:nc) {
		means[i] <- mean(y[,i],na.rm=removeNA)
	}
	means
}