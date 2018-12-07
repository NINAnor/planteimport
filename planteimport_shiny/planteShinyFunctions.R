
## Cumulative plot function

cumPlot <- function(input, 
                    what = c("Taxa", "Individer"),
                    species = "All",
                    country = "All",
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
  
  if(what == "Taxa"){
    if(nrow(aggData) == 1){
     g <-  g + geom_point(mapping = aes(x = noContainer,
                                   y = noSpec, group = 1),
                     col = col,
                     size = 2) 
    } else
     g <-  g + geom_line(mapping = aes(x = noContainer,
                                  y = noSpec),
                    col = col,
                    size = 2) 
  } else
    
    if(what == "Individer"){
      
      if(nrow(aggData) == 1){
        g <-  g + geom_point(mapping = aes(x = noContainer,
                                     y = noInd, group = 1),
                       col = col,
                       size = 2) 
      } else
        g <- g + geom_line(mapping = aes(x = noContainer,
                                    y = noInd),
                      col = col,
                      size = 2) 
    }
  
  g + labs(title = paste0(what, " from shipping country: ", country, ", shipping species: ", species),
           x = "Number of containers",
           y = paste("Number of", what))
  
} 


cumPlot2 <- function(input,
                     what = c("Taxa", "Individer"),
                     col =  c(ninaLogoPalette(), NinaR::ninaPalette())){
  what = match.arg(what)

  g <- ggplot(input)

  myColors <- c(ninaLogoPalette(), NinaR::ninaPalette())
  names(myColors) <- c("Stedegne", "LO","Ikke vurd.", "NR", "NK", "PH", "HI", "SE")
  colScale <- scale_fill_manual(name = "Fremmed-\nartkategori", values = myColors)


  if(what == "Taxa"){
    g <- g +
      geom_area(aes(x = noContainer, y = noSpec, fill = blacklist_cat)) +
      ylab("Artsantall")
  } else
  {
    g <- g +
      geom_area(aes(x = noContainer, y = noInd, fill = blacklist_cat)) +
      ylab("Individantall")
  }
  g <- g +
    colScale +
    xlab("Kontainer")
  g <- g 

  g
}


acumData <- function(input){

  if(nrow(input) == 0){return(NULL)}


  aggData <- input %>%
    group_by(blacklist_cat) %>%
    arrange(container, subsample) %>%
    mutate(noContainer = cumsum(!duplicated(container)),
           noInd = cumsum(amount),
           noSpec = cumsum(!duplicated(species_latin))) %>%
    group_by(container, blacklist_cat) %>%
    summarize(noContainer = last(noContainer),
              noInd = last(noInd),
              noSpec = last(noSpec))

  filler <- expand.grid(unique(aggData$noContainer), unique(aggData$blacklist_cat))
  names(filler) <- c("noContainer", "blacklist_cat")
  filler$blacklist_cat <- as.character(filler$blacklist_cat)
  #filler$noInd <- 0
  #filler$noSpec <- 0

  hm <- aggData %>%
    right_join(filler, by = c("noContainer" = "noContainer", "blacklist_cat" = "blacklist_cat")) %>%
    arrange(noContainer, blacklist_cat)

  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "HI" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NK" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "PH" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "SE" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NR" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "LO" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Stedegne" & is.na(hm$noSpec)] <- 0
  hm$noSpec[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Ikke vurd." & is.na(hm$noSpec)] <- 0

  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "HI" & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NK" & is.na(hm$noInd) & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "PH"] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "SE" & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NR" & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "LO" & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Stedegne" & is.na(hm$noInd)] <- 0
  hm$noInd[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Ikke vurd." & is.na(hm$noInd)] <- 0

  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "HI" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NK" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "PH" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "SE" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "NR" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "LO" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Stedegne" & is.na(hm$noContainer)] <- 0
  hm$noContainer[hm$noContainer == min(hm$noContainer) & hm$blacklist_cat == "Ikke vurd." & is.na(hm$noContainer)] <- 0



  hm <- hm %>%
    arrange(noContainer, blacklist_cat) %>%
    group_by(blacklist_cat) %>%
    fill(noSpec, noContainer, noInd) %>%
    arrange(noContainer, blacklist_cat)

  hm <- hm %>%
    ungroup() %>%
    mutate(blacklist_cat = fct_reorder(blacklist_cat, noSpec))

  hm
}
