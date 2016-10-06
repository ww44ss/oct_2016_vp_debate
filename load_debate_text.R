

load_debate_text <- function(file_name){
    
    ## load raw debate text as data
    ## Read a text file. Filters and conditions the text. Creates a data frame with the row number, candidate name, and text.
    ## INPUT:
    ##      file_name is the name of the file to read
    ##      data are the raw text files downloaded from the UCSB website.
    
    ## OUTPUT:
    ##      returns a data frame with row number, candidate name, and text
    ##      EXAMPLE OF OUTPUT DATA FRAME
    ##           n        name     text
    ##       50 50       RUBIO     Our greatest days lie ahead ....
    ##       51 51 QUINTANILLA     Mr. Trump?
    ##       52 52       TRUMP     I think maybe my greatest weakness ... [laughter]
    
     
    directory <- "/Users/winstonsaunders/Documents/Presidential_Debates_2015/"
    mydata <- read.table(paste0(directory, file_name), header=FALSE, sep="\n", stringsAsFactors = FALSE, quote = "")
    
    ## This is what a sample of the the raw data looks like
    ## mydata[100:105,1]
    # [1] "QUINTANILLA: Hold it. We'll cut it back to..."
    # [2] "QUICK: Dr. Carson, let's talk about taxes."
    # [3] "You have a flat tax plan of 10 percent flat taxes..."
    # [4] "If you were to took a 10 percent tax..."
    # [5] "So what analysis got you to the point where you think this will work?"
    # [6] "CARSON: Well, first of all..."
    
    ## fix a few parasitic cases
    mydata$V1 <- gsub("\\.\\.\\. ", "", mydata$V1)
    mydata$V1 <- gsub("\\.\\.\\.", "", mydata$V1)
    mydata$V1 <- gsub("ISIS","Isis", mydata$V1)
    
    ## This regex gets rid of all but the capitalized names and creates a new column $name
    mydata$name <- gsub(":.+|^[A-Z][^A-Z].+|^[a-z].+", "", mydata$V1 )
    
    ## Fill in the blank rows of $name by looking to the row above if blank
    for (i in 2: nrow(mydata)){
        if (mydata$name[i] == "") mydata$name[i] <- mydata$name[i-1]
    }
    
    #unique(mydata$name)  # use this to check the filtering above. 
    
    ## CREATE COLUMN OF DEBATE TEXT AND CLEAN UP TEXT
    mydata$text <- gsub (".", "", mydata$V1, fixed=TRUE)
    ## clean up
    mydata$text <- gsub ("'", "", mydata$text)
    mydata$text <- gsub ("=", "", mydata$text)
    mydata$text <- gsub ("[A-Z]{2,}: ", "", mydata$text)
    mydata$text <- tolower(mydata$text)
    mydata$text <- gsub ("   ", " ", mydata$text)
    mydata$text <- gsub ("  ", " ", mydata$text)
    
    return(mydata)
}

