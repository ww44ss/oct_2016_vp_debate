
vector.normalize <- function(x){
    ##
    ## a function that produces a normal vector parallel to vector x
    ## INPUTS:
    ##    x: a vector
    ## OUTPUTS:
    ##    normal.x: a normal vector parallel to x
    
    if (! is.null(nrow(x))) x<-colSums(x)
    if (sum(abs(x)) > 0 ) normal.x <- x / sqrt(sum(x*x)) 
    else normal.x<-0
    return(normal.x)
    
    
}