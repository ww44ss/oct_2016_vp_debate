
library(tidyverse)
data(mtcars)

mtcars_tibl <- mtcars %>% as_data_frame

    ## sample computation

    power_to_disp <- mtcars_tibl %>%
        mutate(hp2disp = hp/disp) %>%
        group_by(cyl) %>%
        summarize(mean_hp2disp = mean(hp2disp))
    
    ggplot(power_to_disp, aes(x = cyl, y = mean_hp2disp)) + 
        geom_point(color = "red", fill = "blue", size = 3, pch = 21) + geom_smooth(method = "lm") 
        

    ## works well. 
    ## The problem is in debugging. Let's say in a more complex compuation some upstream data changed.
    ##      for instance some upstream operation like this would break the computation.
    ##      mtcars_tibl$hp <- as.character(mtcars_tibl$hp)
    ## The function would crash, but where and why would be unknown.
    ## To debug, we'd need to start at the end, say comment out the summarize,
    ## delete the %>% at the end of line above, etc. undoing each successive pipe
    ## to find where the compuataion was broken so it could then be fixed. 
   
    
    power_to_disp <- mtcars_tibl %>%
        mutate(hp2disp = hp/disp) %>%
        group_by(cyl)
        #summarize(mean_hp2disp = mean(hp2disp))
    
    
    ## And once fixed, we'd need to go back and retype all the pipes etc.
    ## That's a pain.
        
## instead invent 
    idem <-function(x){x}
    ## this does nothing but repeat what was done earlier. 
    ## - an identify function (which is by definition idempotent).
    ## Hence the name 'idem'
    
## It is used like:    
    power_to_disp <- mtcars_tibl %>%
        mutate(hp2disp = hp/disp) %>%
        group_by(cyl) %>%
        summarize(mean_hp2disp = mean(hp2disp)) %>%
        idem

    ## now to debug you can comment out each line in the chain without having to erase the pipes. 
    
    #such as:
    power_to_disp <- mtcars_tibl %>%
        mutate(hp2disp = hp/disp) %>%
        group_by(cyl) %>%
        #summarize(mean_hp2disp = mean(hp2disp)) %>%
        idem
    
    ## leaves the pipse intact
    ## a little less work