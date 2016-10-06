
## check for installed packages
if("tm" %in% rownames(installed.packages()) == FALSE) {install.packages("tm")}
if("RWeka" %in% rownames(installed.packages()) == FALSE) {install.packages("RWeka")}
if("SnowballC" %in% rownames(installed.packages()) == FALSE) {install.packages("SnowballC")}

text_tc <- function(mydata){
    
    
    ##
    ## FUNCTIONL TEXT_TC
    ##
    ## this function returns a text corpus using the {tm} package after some rudimentary filtering
    ##
    ## INPUT:
    ##      mydata (a text file)
    ## 
    ## OUTPUT:
    ##      t_c a text corpus
    
    t_c <- Corpus(VectorSource(text))
    t_c <- tm_map(t_c, content_transformer(tolower))
    t_c <- tm_map(t_c, removePunctuation)
    t_c <- tm_map(t_c, removeNumbers)
    t_c <- tm_map(t_c, removeWords, stopwords("english"))
    t_c <- tm_map(t_c, removeWords, c("applause", "thats", "laughter", "dont", "back", "can", "get", "cant", "come", "big", "inaudible", "dont", "back", "can", "get"))
    
    return(t_c)   
    
} 