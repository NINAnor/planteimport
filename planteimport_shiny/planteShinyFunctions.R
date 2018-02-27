
## Cumulative plot function

cumPlot <- function(input, 
                    what = c("Species", "Individuals"),
                    col = NinaR::ninaPalette()[3]){
  what = match.arg(what)

  aggData <- input %>% 
    arrange(container,subsample) %>% 
    mutate(noContainer = cumsum(!duplicated(container)),
           noInd = cumsum(amount),
           noSpec = cumsum(!duplicated(species_latin))) %>% 
    group_by(container) %>% 
   summarize(noContainer = last(noContainer),
             noInd = last(noInd),
             noSpec = last(noSpec)) 

g <- ggplot(aggData)  
   
if(what == "Species"){
  if(nrow(aggData) == 1){
    g + geom_point(mapping = aes(x = noContainer,
                                y = noSpec, group = 1),
                  col = col,
                  size = 2) +
      labs(title = "Cumulative number of species",
           x = "Number of containers",
           y = "Number of species")
  } else
 g + geom_line(mapping = aes(x = noContainer,
                             y = noSpec),
               col = col,
               size = 2) +
    labs(title = "Cumulative number of species",
         x = "Number of containers",
         y = "Number of species")
} else

 if(what == "Individuals"){
   
   if(nrow(aggData) == 1){
   g + geom_point(mapping = aes(x = noContainer,
                                  y = noInd, group = 1),
                    col = col,
                  size = 2) +
       labs(title = "Cumulative number of species",
            x = "Number of containers",
            y = "Number of species")
   } else
   g + geom_line(mapping = aes(x = noContainer,
                             y = noInd),
                col = col,
                size = 2) +
     labs(title = "Cumulative number of individuals",
          x = "Number of containers",
          y = "Number of individuals")
 }


     
} 
