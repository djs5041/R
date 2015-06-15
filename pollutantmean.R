pollutantmean <- function(directory, pollutant, id = 1:332) {
  ##process the vector of id's passed to the function
  for (i in seq_along(id)){
    ##assign numeric file id's to character value, may need 
    ##to pad depending on overall length
    fileNum <- as.character(id[i])
    
    ##we are looking for a 3 character file id number, so 
    ##pad the file number with 0's if the file number is not 
    ##3 characters long
    while ( nchar(fileNum) < 3 ) {
      fileNum <- paste("0",fileNum,sep="")
    }
    
    ##read in the entire file contents
    tempFile <- read.csv(paste(getwd(),"/",directory,"/",fileNum,".csv",sep=""),sep=",",header=TRUE)
    
    ##since we or may not be processing multiple
    ##files, we need to bind the vectors together
    ##into a single vector but we cannot just call the 
    ##final vector during the first file process, we 
    ##have to initialize the "final" file based on where
    ## we are in the loop
    if(i==1){
      finalFile <- tempFile
    } else {
      ##perform the rowing binding of the vectors
      finalFile <- rbind(finalFile,tempFile)
    }
    
    ##determine if we need to take the sulfate column 
    ##or the nitrate column.  There is probably a better
    ##way to do this like using a $ but this is a hack 
    ##for now
    if(pollutant=="sulfate") {
      x <- finalFile[2]  
    } else {
      x <- finalFile[3]
    }
    
    ##remove "NA" data from the vector
    naData <- is.na(x)
    
    ##store the cleaned data to a new vector
    output <- x[!naData]
  }
  
  ##compute the mean as the last step 
  ##which will force the return of the mean
  mean(output)
}