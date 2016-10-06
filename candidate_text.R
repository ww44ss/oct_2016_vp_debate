
candidate_text <- function(candidate, mydata, word.filter=""){
    ## 
    ## GET CANDIDATE TEXT
    ##
    ## Assumes load_debate_text has been run and the debate text is stored in "mydata"
    ## creates a text list
    ## INPUT
    ##      candidate name
    ##      file of all debate text
    ## OUTPUT:
    ##      a file of all the candidate's speech
    
    ## filter for candidate
    text <- mydata$text[mydata$name==candidate & grepl(word.filter, mydata$text)]
    
    return(text)
    
}