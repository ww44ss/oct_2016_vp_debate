


candidate_text_tc <- function(candidate, mydata, word.filter = "") {
    ## 
    ## PUT CANDIDATE TEXT INTO A TEXT CORPUS
    ## 
    ## INPUTS:
    ##      mydata: a candidate data frame with candidate name and unprocessed text
    ##          n        name     text
    ##      50 50       RUBIO     Our greatest days lie ahead ....
    ##      51 51 QUINTANILLA     Mr. Trump?
    ##      52 52       TRUMP     I think maybe my greatest weakness ... [laughter]
    ##
    ##      candidate: a name (e.g. "trump")
    ##    
    ##      word.filter - a word that can be selected to filter text for content
    ## OUTPUTS:
    ##      t_c: A Text Corpus of candidate speech
    
    ## filter for candidate and specific words
    
    ## error handling in case candidate invalid
    if (candidate %in% unique(mydata$name)) {
        candidate <- candidate
    } else {
        candidate <- ""
    }
    
    ## select text
    text<-mydata$text[mydata$name==candidate & grepl(word.filter, mydata$text)]
    text<-paste(text, collapse = " ")
    
    t_c <- Corpus(VectorSource(text))
    t_c <- tm_map(t_c, content_transformer(tolower))
    t_c <- tm_map(t_c, removePunctuation)
    t_c <- tm_map(t_c, removeNumbers)
    t_c <- tm_map(t_c, removeWords, stopwords("english"))
    
    ## filter for some specific words 
    t_c <- tm_map(t_c, removeWords, c("applause", "thats", "laughter", "dont", "back", "can", "get", "cant", "come", "big", "inaudible", "dont", "back", "can", "get"))
    
    return(t_c)   
    
} 
