# SENTIMENT OF OCT 2016 VP DEBATE SPEECH
WW44SS  
Oct 6, 2016  


###SUMMARY
Can we learn anything about a debate and it's outcome from a "sentiment" analysis? I adapt methods recently hightlight by David Robinson and Julia Silge to create a kind of "movie" version of the sentiment analysis, with text of especially "strong" sentiment highlighted.

###DATA SOURCES AND METHODS
The text of the debate are downloaded from the [UCSB Presidency Project](http://www.presidency.ucsb.edu/debates.php). Transcripts were pasted into Apple Pages and stored as unformatted .txt files.  

The .txt files are made tidy by a separate `.R` program which removes punctuation and annotation and then categorized by speaker. The data are stored as a .csv file which is loaded here for analysis. 


```r
## this is an identity helper-function useful for simplifying debug of piped analysis steps
## it does "nothing", but does so as a function.
yo <-function(x){return(x)}
```


```r
    library(dplyr)
    library(animation)
    library(ggplot2)
    library(tidytext)
```


```r
## .txt file detection

directory <- "/Users/winstonsaunders/Documents/oct_2016_vp_debate/"
list_of_files <- list.files(directory)
```



```r
## search for and read lightly cleaned debate text .csv
file_of_data <- list_of_files[grepl("_tidy", list_of_files)]
debate_text <- read.csv(paste0(directory, file_of_data), stringsAsFactors = FALSE) %>% 
    as_data_frame
```

We now begin processing by taking the text, unnesting the sentences, and removing stop words using the onix lexicon


```r
## compute stop_words_list
list_of_stop_words <- stop_words %>% filter(lexicon == "snowball") %>% select(word) %>% 
    yo


## create tidy df of debate words
words_from_the_debate <- debate_text %>% unnest_tokens(word, text) %>% filter(!word %in% 
    list_of_stop_words) %>% yo
```

We create a "sentiment dictionary" from the information stored in the `tidytext` package and use a `left_join` to assicate words with the sentiment values.


```r
## compute sentiment dictionary
word_sentiment_dict <- sentiments %>% filter(lexicon == "AFINN") %>% select(word, 
    sentiment = score) %>% yo

## get sentiment of individual words by joining data with dictionary using
## left_join and then assigning NA's to zero
debate_words_sentiments <- words_from_the_debate %>% left_join(word_sentiment_dict, 
    by = "word") %>% mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>% 
    mutate(sentiment = as.numeric(sentiment)) %>% yo

nonzero_sentiment_debate_words <- debate_words_sentiments %>% filter(sentiment != 
    0)
```

To look at the trend of the sentiment, we can create an exponentially damped cummulative sum function to apply to the data. The idea is that words have immediate punch, but it wanes as time and words pass. So that's the idea...


```r
decay_sum <- function(x, decay_rate = 0.1421041) {
    ## EXPONENTIALLY DAMPED CUMMULIATVE SUM input: x (a vector of length >1)
    ## decay_rate (exponential damping factor) output: decay_sum (a vector with
    ## the cummulatve sum)
    
    ## create output vector
    if (length(x) > 1) 
        decay_sum <- 1 * (1:length(x))
    
    ## initialize
    decay_sum[1] <- x[1] * 1
    
    ## compute the sum using a dreaded loop.
    if (length(x) > 1) {
        for (i in 2:length(x)) {
            decay_sum[i] <- x[i] + exp(-1 * decay_rate) * decay_sum[i - 1]
        }
    }
    
    return(decay_sum)
}
```

We can now compute the sum of the sentiment and the cummulative sum.


```r
    ## compute sentiment of debate responses by regrouping and compute means and cumsums
    debate_sentiment <- debate_words_sentiments %>%
        group_by(X, name) %>%
        summarize(sentiment = sum(sentiment)) %>%
        group_by(name) %>%
        mutate(cumm_sent = decay_sum(sentiment, decay_rate = 0.02)) %>%
        yo
```

The final step is to pull it all together to create a plot data frame. 


```r
## create data_frame for plotting. Since some X have no entry, need to fix
## those
plot_df <- debate_sentiment %>% left_join(debate_text, by = c("X", "name")) %>% 
    select(X = X, name = name, sentiment, cumm_sent, text) %>% group_by(name) %>% 
    yo
```
From here use the `animation` package functions to create the gif.






```r
    saveGIF({
    for (i in gif_steps) {
        
        if (plot_df$sentiment[plot_df$X == i] < 0) {
            title_color = "darkred"
        } else {
            title_color = "darkblue"
        }
        
        title_h = 0
        
        print( 
           
            ggplot(plot_df, aes(x = X, y = sentiment, fill = name)) +
                geom_bar(stat = 'identity', alpha = 1., width = 2) +
                geom_line(data = plot_df%>% filter(X <= i), aes(x=X, y = 15*cumm_sent/max(abs(plot_df$cumm_sent))), size = 3, color = "grey20", alpha = 0.5) +
                xlim(range(plot_df$X)) +
                ylim(range(plot_df$sentiment)) +
                xlab("index") +
                ggtitle(paste0(plot_df$name[plot_df$X == i], ": ",                             paste(nonzero_sentiment_debate_words$word[nonzero_sentiment_debate_words$X == i] %>% unique, collapse = " "))) +
                facet_grid(name~.) +
                theme(plot.title = element_text(size = 18, hjust = title_h, color = title_color, face="bold"), legend.position = "bottom")
        ) 
        
    }
    }, interval = 1.2, movie.name = paste0(directory,"sentiment_animation2.gif"), ani.width = 700, ani.height = 450)
```


Here is the gif produced. The title shows the "high sentiment" words used by each candidate in that particular segment of speech (not the entire phrase). The grey bars advance with the title, while the mapped sentiment of the whole debate is displayed as a bar chart undrlying the trend lines.   
  
Some observations: 
* The sentiment lines of both candidate track one another pretty closely.  
* The moderator appears to use much more neutral language than the candidates. 
* Thru the first ~ third of the debate nothing much happens. 
* Then sentiment plunges. (This was the point in the debate when police violence was discussed).  
* Sentiment rises thru the last thrid of the debate

![Gif](/Users/winstonsaunders/Documents/oct_2016_vp_debate/sentiment_animation2.gif) 

For what it's worth, here is the text of the most polarized statements


```r
knitr::kable(plot_df %>% filter(X %in% gif_steps ) %>% select(X, name, sentiment, text), caption = "Most Positive and Negative Sentiment Phrases")
```



Table: Most Positive and Negative Sentiment Phrases

   X  name       sentiment  text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
----  --------  ----------  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  16  PENCE             14  well first off thank you elaine and thank you to thank you to norwood university for their wonderful hospitality and the commission on presidential debates its deeply humbling for me to be here to be surrounded by my my wonderful family                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
  17  PENCE              8  and senator kaine its an honor to be here with you as well and i just i also want to say i want to say thanks to everyone thats looking in tonight who understands what an enormously important time this is in the life of our nation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
  19  PENCE             15  i also want to thank donald trump for making that call and inviting us to be a part of this ticket i have to tell you im a im a smalltown boy from a place not too different from farmville i grew up with a cornfield in my backyard my grandfather had immigrated to this country when he was about my sons age my mom and dad built a everything that matters in a small town in southern indiana they built a family and and a good name and a business and they raised a family and i dreamed some day of representing my home town in washington but i honestly elaine i never imagined never imagined id have the opportunity to be governor of the state that i love let alone be sitting at a table like this in this kind of a position 
  20  PENCE             11  so to answer your question i would say i i would hope that if if the responsibility ever fell to me in this role that i would meet it with the way that im going to meet the responsibility should i be elected vice president of the united states and thats to bring a lifetime of experience a lifetime growing up in a small town a lifetime where ive served in the congress of the united states where where ive led a state that works in the great state of indiana and whatever other responsibilities might follow from this i i would hope and frankly i would pray to be able to meet that moment with that that lifetime of experience                                                                                               
 195  PENCE              8  donald trump and i are going to make sure that law enforcement have the resources and the tools to be able to really restore law and order to the cities and communities in this nation its probably probably why the members of the fraternal order of police endorsed donald trump as the next president of the united states of america because they see his commitment to them they see his commitment to law and order                                                                                                                                                                                                                                                                                                                       
 196  PENCE            -15  but they also they also hear the bad mouthing the bad mouthing that comes from people that seize upon tragedy in the wake of police action shootings as as a reason to to use a broad brush to accuse law enforcement of of implicit bias or institutional racism and that really has got to stop                                                                                                                                                                                                                                                                                                                                                                                                                                                 
 197  PENCE            -11  i mean when an africanamerican police officer in charlotte named brentley vinson an allstar football player who went to liberty university here in the state came home followed his dad into law enforcement joined the force in charlotte joined the force in charlotte in was involved in a police action shooting that claimed the life of keith keith lamont scott it was a tragedy i mean i we we mourn with those who mourn we we grieve with those who grieve and were saddened at the loss of life                                                                                                                                                                                                                                        
 200  PENCE             -8  i just think what we ought to do is we ought to stop seizing on these moments of tragedy we ought to assure the public that well have a full and complete and transparent investigation whenever theres a loss of life because of police action but senator please you know enough of this seeking every opportunity to demean law enforcement broadly by making the accusation of implicit bias every time tragedy occurs                                                                                                                                                                                                                                                                                                                        
 204  KAINE             -7  the guy philando castile who was killed in st paul he was a worker a valued worker in a local school and he was killed for no apparent reason in an incident that will be discussed and will be investigated                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
 231  PENCE             -7  donald trumps laid out a plan to end illegal immigration once and for all in this country weve been talking it to death for years hillary clinton and tim kaine want to continue the policies of open borders amnesty catch and release sanctuary cities all the things that are driving that are driving wages down in this country senator and also too often with criminal aliens in the country its bringing heartbreak                                                                                                                                                                                                                                                                                                                       
 274  PENCE            -12  but he said the focus has to be on criminal aliens we just we just had a conversation about law enforcement we just had a conversation about the the violence thats besetting our cities the reality is that theres heartbreak and tragedy that has struck american families because people that came into this country illegally are now involved in criminal enterprise and activity and we dont have the resources or the will to deport them systemically                                                                                                                                                                                                                                                                                     
 285  KAINE             -9  the terrorist threat has decreased in some ways because bin laden is dead the terrorist threat has decreased in some ways because an iranian nuclear weapons program has been stopped the terrorist threat to united states troops has been decreased in some ways because theres not in a dangerous part of the world theres only                                                                                                                                                                                                                                                                                                                                                                                                                
 303  PENCE              8  my heart breaks for the likes of lance cpl scott zubowski he fell in fallujah in he fought hard through some of the most difficult days in operation iraqi freedom and he paid the ultimate sacrifice to defend our freedom and secure that nation and that nation was secured in                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
 404  KAINE             -9  so get this on hillary clinton and donald trumps hometown was attacked by the worst terrorist attack in the history of the united states young men and women young men and women signed up to serve in the military to fight terrorism hillary clinton went to washington to get funds to rebuild her city and protect first responders but donald trump was fighting a very different fight it was a fight to avoid paying taxes so that he wouldnt support the fight against terror                                                                                                                                                                                                                                                             
 406  KAINE             14  he wouldnt support troops he wouldnt he wouldnt support this is important elaine when a guy running for president will not support the troops not support veterans not support teachers thats really important                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
 433  KAINE             -8  but let me tell you what will really make the middle east dangerous donald trumps idea that more nations should get nuclear weapons saudi arabia japan south korea ronald reagan said something really interesting about nuclear proliferation back in the s he said the problem with nuclear proliferation is that some fool or maniac could trigger a catastrophic event and i think thats who governor pences running mate is exactly who governor reagan warned us about                                                                                                                                                                                                                                                                      
 472  KAINE              9  youve got to be tough on russia so lets start with not praising vladimir putin as a great leader donald trump and mike pence have said hes a great leader and donald trump has business                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
 500  PENCE             13  and then of course syria i mean it really is extraordinary that syria is imploding you just asked a very thoughtful question about the disaster in aleppo isis is headquartered in raqqa it is isis from raqqa has overrun vast areas that at great sacrifice the american soldier won in operation iraqi freedom and yet senator kaine still sits here loyal soldier i get all that in saying that the foreign policy of hillary clinton and barack obama somehow made the world more secure i mean it really is astonishing that on the day                                                                                                                                                                                                     
 538  KAINE             13  shes not going around praising vladimir putin as a great guy but she knows how to sit down at a table and negotiate tough deals this is a very challenging part of the world and we ought to have a commanderinchief who is prepared and done it rather than somebody who goes around praising vladimir putin as a great leader                                                                                                                                                                                                                                                                                                                                                                                                                   
 556  KAINE              8  hillary clinton as secretary of state took no action to benefit the foundation the state department did an investigation and they concluded that everything hillary clinton did as secretary of state was completely in the interest of the united states so the foundation does good work and hillary clinton as secretary of state acted in the interests of the united states                                                                                                                                                                                                                                                                                                                                                                  
 560  KAINE             -8  in addition donald trump has a foundation the foundation was just fined for illegally contributing foundation dollars to a political campaign of a florida attorney general they made an illegal contribution and then they tried to hide it by disguising it to somebody else and the person they donated to was somebody whose office was charged with investigating trump university                                                                                                                                                                                                                                                                                                                                                           
 580  KAINE             12  yeah thats an easy one for me elaine its an easy one im really fortunate i grew up in a wonderful household with great irish catholic parents my mom and dad are sitting right here i was educated by jesuits at rockhurst high school in kansas city my th reunion is in days                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
 582  KAINE             -7  for me the hardest struggle in my faith life was the catholic church is against the death penalty and so am i but i was governor of a state and the state law said that there was a death penalty for crimes if the jury determined them to be heinous and so i had to grapple with that                                                                                                                                                                                                                                                                                                                                                                                                                                                          
 587  PENCE             11  well its a wonderful question and my christian faith is at the very heart of who i am i was also raised in a wonderful family of faith it was a church on sunday morning and grace before dinner                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
 622  PENCE              8  and i have appreciated the fact that youve supported the hyde amendment which bans the use of taxpayer funding for abortion in the past but thats not hillary clintons view people need to understand we can come together as a nation we can create a culture of life more and more young people today are embracing life because we know we are were better for it we can like mother teresa said at that famous national prayer breakfast                                                                                                                                                                                                                                                                                                      
 650  QUIJANO            3  from farmville virginia im elaine quijano of news good night                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      


It's worthwhile to note that in some isntances while the sentiment tool records the message as "positive", in face the message is intended to be negative, for isntance at index `X = 500` in the above table. This highlights that some caution is needed in interpreting results. 
