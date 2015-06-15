complete <- function(directory, id = 1:332){
  ##process the directory, read each file and determine the number of 
  ##complete observations
  
  ##create an empty data frame that is only a structure
  output <- data.frame(id=integer(),nobs=integer()) 
  
  ##process the vector of id's passed to the function
  for (i in seq_along(id)){
    ##assign numeric file id's to character value, may need 
    ##to pad depending on overall length
    fileNum <- as.character(id[i])
  
    ##we are looking for a 3 character file id number, so 
    ##pad the file number with 0's if the file number is not 
    ##3 characters long
    while (nchar(fileNum) < 3 ) {
      fileNum <- paste("0",fileNum,sep="")
    }
    
    dirFile<-paste(getwd(),"/",directory,"/",fileNum,".csv",sep="")
    
    ##read in the entire file contents
    tempFile <- read.csv(dirFile,sep=",",header=TRUE)
    
    ##now, subset the entire data file based on the complete
    ##cases. This will process each column at a time and get
    ##rid of any rows that have a NA for any column value.
    cmpCases <- tempFile[complete.cases(tempFile),]
    
    ##add a record to the data frame
    output <- rbind(output, c(id[i],nrow(cmpCases)))
    
  }
  ##explicity name the columns what I want them to be :)
  colnames(output) <- c("id","nobs")
  
  output
}